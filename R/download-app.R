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
  headerPanel(""),
  hr(),
  fluidRow(align = "center",
    column(4, align="center",
           #station selection
           selectInput("Station","Station",
                       stations$`Station name`, multiple = F),
           #variable selection
           selectInput("Variable","Variable(s)",
                       elements$Element, multiple = T)),
    column(4, align="center", 
           #date selection
           # Default value is the date in client's time zone
           dateInput("date_start", "Start Date"),
           dateInput("date_end", "End Date")),
    column(4, align="center", #temporal aggregation
           selectInput("aggregation","Aggregation Interval",
                       c('No Aggregation (15 Minute Data)', 'Daily', 'Monthly'), multiple = F))
  ),
  #action button for running request
  headerPanel(""),
  actionButton("do", "Run Request", width = '100%'),
  verbatimTextOutput("dfStr"),
  headerPanel(""),
  column(12, align="center",
         uiOutput("download_button", inline = T)
         
  ),
  #error message if needed
  mainPanel(
    textOutput("error_message")
  ),
  #download button
),
# now on to the server
server <- function(input, output, session) {
  # this is our map that we will display
  output$mymap <- renderLeaflet({
    base_map() %>%
      addCircleMarkers(data = stations, lat = ~Latitude, lng = ~Longitude, stroke = TRUE, layerId = ~`Station name`,
                           fillColor = "blue", fillOpacity = 0.5, color = "black", opacity = 0.8, radius = 6, weight = 2,
                           label = stations$`Station name`,
                           labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                       style = list(
                                                         "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                         "font-size" = "16px"
                                                       ))) 
  })
  # Now for our reactive portion 
  # This will update the selector based on clicking on the leaflet map
  observeEvent(input$mymap_marker_click, { 
    p = input$mymap_marker_click$id   
    updateSelectInput(session, "Station",
                      label = 'Station',
                      choices = stations$`Station name`,
                      selected = p
    )
  })
  
  #this is the main function to crunch numbers and produce final download
  observeEvent(input$do, {
    tryCatch({
      station_name = input$Station
      aggregation_name = input$aggregation
      station_meta = stations[which(stations$`Station name`==input$Station),]
      elements_meta = elements[which(elements$Element %in% input$Variable),]
      temp = read_csv(paste0('~/mesonet-download-data/', station_meta$`Station ID`, '.csv')) %>%
        filter(name %in% elements_meta$`Element ID`,
               datetime >= input$date_start,
               datetime < input$date_end + 1) 
      if(input$aggregation == 'No Aggregation (15 Minute Data)'){
        export = temp %>% 
          pivot_wider(-units) %>%
          select(-qc_code)
        
        name = paste0("MT_Mesonet_", station_meta$`Station ID`, '_raw_data.csv')
      }
      if(input$aggregation == 'Daily'){
        export = temp %>%
          mutate(year = year(datetime),
                 yday = yday(datetime)) %>%
          group_by(station_key, yday, year, name) %>%
          summarise(value = ifelse(first(name == 'precipit'), sum(value), mean(value))) %>%
          mutate(datetime = as.POSIXct(paste(yday, year, sep = "-"), format = "%j-%Y")) %>%
          arrange(datetime) %>%
          pivot_wider() %>%
          ungroup() %>%
          select(., -c('yday', 'year'))
        
        name = paste0("MT_Mesonet_", station_meta$`Station ID`, '_daily_data.csv')
      }
      if(input$aggregation == 'Monthly'){
        export = temp %>%
          mutate(year = year(datetime),
                 month = month(datetime)) %>%
          group_by(station_key, month, year, name) %>%
          summarise(value = ifelse(first(name == 'precipit'), sum(value), mean(value))) %>%
          arrange(year,month) %>%
          pivot_wider() %>%
          ungroup() 
        
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
      #display download button
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

