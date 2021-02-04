library(dplyr)
library(readr)
library(stringr)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(ncdf4)
library(lubridate)
library(shinythemes)
library(RCurl)
library(tidyr)

#new
library(plotly)

source('/home/zhoylman/mesonet-download/R/base_map.R')

#bring in current station list
stations = getURL("https://mesonet.climate.umt.edu/api/stations?type=csv&clean=true") %>%
  read_csv() %>%
  arrange(`Station name`)

#bring in available variables
elements = getURL("https://mesonet.climate.umt.edu/api/elements/all?type=csv&clean=true") %>%
  read_csv() %>%
  arrange(Element) %>%
  mutate(Units = str_remove_all(Units, 'Â'),
         new_col_names = paste0(Element, ' (', Units, ')'))

site_specific_elements = read_csv('/home/zhoylman/mesonet-download/data/site_specific_elements.csv')

# site_specific_elements = getURL('https://mesonet.climate.umt.edu/api/latest?tz=US%2FMountain&simple_datetime=false&wide=false&type=csv') %>%
#   read_csv() %>%
#   group_by(station_key) %>%
#   distinct(name)

# site_specific_elements = getURL('https://mesonet.climate.umt.edu/api/observations/all?type=csv&wide=false') %>%
#   read_csv() %>%
#   drop_na() %>%
#   group_by(station_key) %>%
#   distinct(name)
# 
# write_csv(site_specific_elements, '/home/zhoylman/mesonet-download/data/site_specific_elements.csv')

#troubleshooting data
# temp = read_csv(paste0('~/mesonet-download-data/wsrabsaw.csv')) %>%
#   filter(name %in% c('air_temp','atmos_pr', 'vpd_atmo', 'precipit'))%>%
#   select(-units, -qc_code)

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
                                  #final collumn
                                  column(4, align="center", 
                                         #temporal aggregation
                                         selectInput("aggregation","Aggregation Interval",
                                                     c('No Aggregation (15 Minute Data)', 'Daily', 'Monthly'), multiple = F,
                                                     selected = 'Daily')),
                                  #second collumn 
                                  column(4, align="center", 
                                         #date selection
                                         dateInput("date_start", "Start Date"),
                                         dateInput("date_end", "End Date"))
                         ),
                         #space for aesthetics
                         headerPanel(""),
                         #action button for running request
                         actionButton("do", "Run Request", width = '100%'),
                         #space for aesthetics
                         headerPanel(""),
                         hr(),
                         #add a processing message to tell the user its working
                         column(12, align = 'center',
                                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                 tags$div("Processing Request...",
                                                          id="loadmessage"),
                                                 tags$style("#loadmessage{color: red;
                                 font-size: 24px;
                                 }"
                                                 ))
                         ),
                         #error message if needed
                         column(12, align = 'center',
                                textOutput("error_message")
                         ),
                         hr(),
                         # column(12, align = 'center',
                         #   plotlyOutput('plotly_output')
                         # ),
                         column(12, align = 'center',
                                div(
                                  class = "container",
                                  uiOutput("dynamic_tabs")
                                )),
                         headerPanel(""),
                         headerPanel(""),
                         headerPanel(""),
                         hr(),
                         #add a new collumn class to center the download button
                         column(12, align="center",
                                uiOutput("download_button", inline = T))
),
# end of User Interface (UI)
# now on to the server
server <- function(input, output, session) {
  #dynamic tabs are NULL to start
  rv = reactiveValues(start = NULL)
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
  
  ##############################################################################
  ######################## DYNAMIC SELECTION UPDATE ############################
  ##############################################################################
  
  # This will update the station selector based on clicking on the leaflet map
  observeEvent(input$mymap_marker_click, { 
    p = input$mymap_marker_click$id   
    updateSelectInput(session, "Station",
                      label = 'Station',
                      choices = stations$`Station name`,
                      selected = p
    )
  })
  
  # This will update the variables that can be selected depending on what station is selected
  observeEvent(input$Station, { 
    #site specific variables
    station_name = stations %>%
      filter(`Station name` == input$Station)
    
    #get full station meta to update selectors
    selectors = site_specific_elements %>%
      filter(station_key == station_name$`Station ID`)
    
    #find the Element names assosiated with these
    elements_UI = elements %>% 
      filter(`Element ID` %in% selectors$name)
    
    #find valid pre selected variables to reset selected vars
    already_selected = input$Variable[input$Variable %in% elements_UI$Element]
    
    #update selector based on vars assosiated with specific site 
    #and reselect valid vars 
    updateSelectInput(session, "Variable",
                      label = 'Variable(s)',
                      choices = elements_UI$Element,
                      selected = already_selected
    )
  })
  
  observeEvent(input$Station, { 
    #site specific date meta
    station_meta = stations %>%
      filter(`Station name` == input$Station) %>%
      mutate(simple_start_date = mdy(`Start date`))
    
    #update the date selector so it is stire specific start date and 
    #truncates date range to min site specific date
    updateDateInput(session, "date_start",
                    label = 'Start Date',
                    value = station_meta$simple_start_date,
                    min   = station_meta$simple_start_date)
    
  })
  
  
  #this is the main function to crunch numbers and produce final download (tabular)
  #reactive for when the user hits the 'do' button
  observeEvent(input$do, {
    tryCatch({
      #custom error handling condtions
      #no variable selected
      if(is.null(input$Variable)){
        stop()
      }
      #logical date error
      if(input$date_start > input$date_end){
        stop()
      }
      #define vars to name the download button
      #must define here or else reactive response from UI will change button names before processing data
      station_name = input$Station
      aggregation_name = input$aggregation
      #define meta data based on UI selection 
      station_meta = stations[which(stations$`Station name`==input$Station),]
      elements_meta = elements[which(elements$Element %in% input$Variable),]
      
      #pull in the whole dataset from flat file based on UI defined station name
      temp = read_csv(paste0('/home/zhoylman/mesonet-download-data/', station_meta$`Station ID`, '.csv')) %>%
        #filter by selected vars and time
        filter(name %in% elements_meta$`Element ID`,
               datetime >= input$date_start,
               datetime < input$date_end + 1) 
      #if the UI selects no aggregation, just clean data nd pivot wider
      if(input$aggregation == 'No Aggregation (15 Minute Data)'){
        export = temp %>% 
          pivot_wider(-units) %>%
          select(-qc_code)%>%
          mutate(datetime = as.character(datetime))
        
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
          select(., -c('yday', 'year')) %>%
          mutate(datetime = as.character(datetime))
        
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
          ungroup() %>%
          #recreate psudo datetime for plotting
          mutate(datetime = as.POSIXct(paste(year, month, '01', sep = "-")) %>%
                   as.character()) %>%
          relocate(station_key, month, year, datetime)
        
        #define name for export
        name = paste0("MT_Mesonet_", station_meta$`Station ID`, '_monthly_data.csv')
      }
      # rename columns to include units 
      new_col_names = elements %>%
        filter(`Element ID` %in% colnames(export))
      export = export %>% 
        rename_at(vars(new_col_names$`Element ID`), function(x) new_col_names$new_col_names) 
      
      ####################### Plotting with Dynamic Tabs ###############################
      
      #reactive values for dynamic plot creation
      rv$start = new_col_names$new_col_names
      
      output$dynamic_tabs = renderUI({
        rv$start %>% 
          purrr::map(~
                       shiny::tabPanel(
                         title = .x,
                         div(
                           class = "panel", 
                           div(
                             class = "panel-header",
                             tags$h3(.x)
                           ),
                           div(
                             class = "panel-body",
                             
                             export %>%
                               pivot_longer(cols = -c(station_key, datetime)) %>% 
                               dplyr::filter(name == .x) %>% 
                               plot_ly(x = ~datetime %>% as.POSIXct(), y = ~value, name = ~name, color = ~name, 
                                       showlegend=F, colors = 'black', legendgroup = ~name) %>% 
                               add_lines() %>%
                               layout(legend = list(orientation = "h",
                                                    xanchor = "center",  
                                                    x = 0.5),
                                      xaxis = list(title = ""),
                                      yaxis = list(title = .x),
                                      title= input$Station,
                                      height = 500) %>%
                               config(displaylogo = FALSE,
                                      toImageButtonOptions= list(filename = paste0(input$Station, ' ', .x),
                                                                 width = 1000,
                                                                 height =  700)) %>%
                               layout(
                                 images = list(
                                   list(source = "https://raw.githubusercontent.com/zhoylman/mesonet-download/master/data/MCO_logo.png",
                                        xref = "paper",
                                        yref = "paper",
                                        x= 0,
                                        y= 1,
                                        sizex = 0.2,
                                        sizey = 0.2,
                                        opacity = 0.8
                                   )))
                                   
                           )
                         )
                       )
          ) -> gap
        
        do.call(what = tabsetPanel, 
                args = gap %>% 
                  append(list(type = "pills",
                              id   = "var_tabs")))
        
      })
      
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
      #clear error message is there was no error
      output$error_message <- renderText({
        ''
      })
      
      # general error handling to keep app from crashing  
    }, error = function(e){
      #custom error messages based on specific circumstances
      message = ifelse(is.null(input$Variable), 'Please select a variable to download.',
                       ifelse(input$date_start > input$date_end, 
                              '"Start Date" must be equal to or earlier than "End Date".', 'Oops, there seams to be an error...'))
      
      output$error_message <- renderText({
        message
      })
      
    })
  })
},  options = list(height = 1200))

#fin