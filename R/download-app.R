library(tidyverse)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(ncdf4)
library(lubridate)
library(shinythemes)

source('~/mesonet-download/R/base_map.R')

#bring in current station list
stations = getURL("https://mesonet.climate.umt.edu/api/stations?type=csv&clean=true") %>%
  read_csv() %>%
  arrange(`Station name`)

#bring in available variables
elements = getURL("https://mesonet.climate.umt.edu/api/elements?type=csv&clean=true") %>%
  read_csv() %>%
  arrange(Element)

#run app
shinyApp(ui <- fluidPage(theme = shinytheme("cosmo"),
  # build our UI defining that we want a vertical layout
  verticalLayout(),
  # first we want to display the map
  leafletOutput("mymap"),
  #add a space for aesthetics
  headerPanel(""),
  #add a horizontal line for aesthetics
  hr(),
  #set up a fluid row class for inputs
  fluidRow(align = "center",
           #first collumn (out of 12 total)
           column(4, align="center",
                  #station selection
                  selectInput("Station","Station",
                              stations$`Station name`, multiple = F),
                  #variable selection
                  selectInput("Variable","Variable(s)",
                              elements$Element, multiple = T)),
           #second collumn 
           column(4, align="center", 
                  #date selection
                  dateInput("date_start", "Start Date"),
                  dateInput("date_end", "End Date")),
           #final collumn
           column(4, align="center", 
                  #temporal aggregation
                  selectInput("aggregation","Aggregation Interval",
                              c('No Aggregation (15 Minute Data)', 'Daily', 'Monthly'), multiple = F))
  ),
  #space for aesthetics
  headerPanel(""),
  #action button for running request
  actionButton("do", "Run Request", width = '100%'),
  #space for aesthetics
  headerPanel(""),
  #add a new collumn class to center the download button
  column(12, align="center",
         uiOutput("download_button", inline = T)
         
  ),
  #error message if needed
  mainPanel(
    textOutput("error_message")
  ),
),
# end of User Interface
# now on to the server
server <- function(input, output, session) {
  # this is our map that we will display
  #based on the base_map() function defined above
  output$mymap <- renderLeaflet({
    base_map() %>%
      #add circle points for the station locations 
      #this will allow the user to select the station based on the map
      addCircleMarkers(data = stations, lat = ~Latitude, lng = ~Longitude, stroke = TRUE, layerId = ~`Station name`,
                           fillColor = "blue", fillOpacity = 0.5, color = "black", opacity = 0.8, radius = 6, weight = 2,
                           label = stations$`Station name`,
                           labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                       style = list(
                                                         "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                         "font-size" = "16px"
                                                       ))) 
  })
  # Now for the truely reactive portion 
  # This will update the station selector based on clicking on the leaflet map
  observeEvent(input$mymap_marker_click, { 
    p = input$mymap_marker_click$id   
    updateSelectInput(session, "Station",
                      label = 'Station',
                      choices = stations$`Station name`,
                      selected = p
    )
  })
  #this is the main function to crunch numbers and produce final download (tabular)
  #reactive for when the user hits the 'do' button
  observeEvent(input$do, {
    tryCatch({
      #define vars to name the download button
      #must define here or else reactive response from UI will change button names before processing data
      station_name = input$Station
      aggregation_name = input$aggregation
      #define meta data based on UI selection 
      station_meta = stations[which(stations$`Station name`==input$Station),]
      elements_meta = elements[which(elements$Element %in% input$Variable),]
      #pull in the whole dataset from flat file based on UI defined station name
      temp = read_csv(paste0('~/mesonet-download-data/', station_meta$`Station ID`, '.csv')) %>%
        #filter by selected vars and time
        filter(name %in% elements_meta$`Element ID`,
               datetime >= input$date_start,
               datetime < input$date_end + 1) 
      #if the UI selects no aggregation, just clean data nd pivot wider
      if(input$aggregation == 'No Aggregation (15 Minute Data)'){
        export = temp %>% 
          pivot_wider(-units) %>%
          select(-qc_code)
        
        #define name for export
        name = paste0("MT_Mesonet_", station_meta$`Station ID`, '_raw_data.csv')
      }
      #if the UI selects daily, then aggreate by yday
      if(input$aggregation == 'Daily'){
        export = temp %>%
          #define some time meta data for grouping
          mutate(year = year(datetime),
                 yday = yday(datetime)) %>%
          #group
          group_by(station_key, yday, year, name) %>%
          #conditional summary (precip = sum, everything else is mean)
          summarise(value = ifelse(first(name == 'precipit'), sum(value, na.rm = T), mean(value, na.rm = T))) %>%
          #recompute time
          mutate(datetime = as.POSIXct(paste(yday, year, sep = "-"), format = "%j-%Y")) %>%
          #order by time
          arrange(datetime) %>%
          #pivot wider
          pivot_wider() %>%
          #ungroup
          ungroup() %>%
          #deslect columns that are unwated
          select(., -c('yday', 'year'))
        
        #define name for export
        name = paste0("MT_Mesonet_", station_meta$`Station ID`, '_daily_data.csv')
      }
      #if UI selects monthly aggreation 
      if(input$aggregation == 'Monthly'){
        export = temp %>%
          #define some time meta
          mutate(year = year(datetime),
                 month = month(datetime)) %>%
          #group
          group_by(station_key, month, year, name) %>%
          #conditional summary (precip = sum, everything else is mean)
          summarise(value = ifelse(first(name == 'precipit'), sum(value, na.rm = T), mean(value, na.rm = T))) %>%
          #order by time
          arrange(year,month) %>%
          #pivot wider
          pivot_wider() %>%
          #ungroup
          ungroup() 
        
        #define name for export
        name = paste0("MT_Mesonet_", station_meta$`Station ID`, '_monthly_data.csv')
      }
      #set up download file
      output$download <- downloadHandler(
        filename = function() {
          name
        },
        content = function(filename) {
          write_csv(export, filename)
        }
      )
      #display download button only after the first process has been completed
      output$download_button <- renderUI({
        if(!is.null(export)) {
          downloadButton('download', 
                         paste0('Download Output File for ', station_name, ' [', aggregation_name, ']'))
        }
      })

    # general error handling to keep app from crashing  
    }, error = function(e){
      output$error_message <- renderText({
        'Oops, there seams to be an error...'
      })
    })
  })
})

#fin