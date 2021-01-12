#Fetch rainfall from MARS database

#shiny
library(shiny)
#shiny themes for color 
library(shinythemes)
#pwdgsi for fetching rainfall data
library(pwdgsi)
#tidyverse for data manipulation 
library(tidyverse)
#pool for database connections
library(pool)
#odbc for database connection
library(odbc)
#easy javascript commands
library(shinyjs)
#load extra shiny widgets
library(shinyWidgets)

options(stringsAsFactors=FALSE)

#set up

#set db connection
#using a pool connection so separate connections are unified
#gets environmental variables saved in local or pwdrstudio environment
poolConn <- dbPool(odbc(), dsn = "mars_testing", uid = Sys.getenv("shiny_uid"), pwd = Sys.getenv("shiny_pwd"))

#disconnect from db on stop 
onStop(function(){
  poolClose(poolConn)
})

#js warning about leaving page
#jscode <- 'window.onbeforeunload = function() { return "Please use the button on the webpage"; };'

#define global variables that will be required each time the UI runs
#query all SMP IDs
smp_id <- odbc::dbGetQuery(poolConn, paste0("select distinct smp_id from smp_loc")) %>% 
  dplyr::arrange(smp_id) %>% 
  dplyr::pull()

#min_rainfall_date <- '1990-01-01'
max_rainfall_date <- as.Date(odbc::dbGetQuery(poolConn, paste0("select max(dtime_edt) from public.rainfall_gage where dtime_edt > '2020-09-30'")) %>% pull)

max_baro_date <- as.Date(odbc::dbGetQuery(poolConn, paste0("SELECT max(dtime_est) FROM public.barodata_neighbors where dtime_est > '2020-11-01'
")) %>% pull)

max_date = max(c(max_rainfall_date, max_baro_date))

ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("MARS Fetch Rainfall & Baro Data"),
                sidebarPanel(
                  selectInput("smp_id", "SMP ID", choices = c("", smp_id), selected = NULL), 
                  selectInput("data_type", "Data Type", choices = c("", "Rainfall" = 1, "Baro" = 2), selected = NULL),
                  airDatepickerInput("daterange", "Date Range", range = TRUE),#maxDate = max_date),
                verbatimTextOutput("res"),
                conditionalPanel(condition = 'input.data_type == 2',
                  selectInput("interval", "Interval (min)", choices = c("", 5, 15), selected = NULL)
                  ), 
                conditionalPanel(condition = 'input.data_type == 1', 
                  checkboxInput("dst", "Daylight Saving")
                  ), 
                conditionalPanel(condition = 'input.data_type == 1', 
                  actionButton("rainfall_data", "Get Rainfall Data"), 
                  disabled(downloadButton("dl_rainfall", "Download Rainfall .csv"))
                  ), 
                conditionalPanel(condition = 'input.data_type == 2', 
                  actionButton("baro_data", "Get Baro Data"), 
                  #disabled(downloadButton("dl_baro_report", "Download Baro Report")), 
                  disabled(downloadButton("dl_baro_data", "Download Baro .csv"))
                  ) 
                  ),
                useShinyjs(), 
                mainPanel(
                  #create a header to let user know that the data was retrieved/generated. 
                  h4(textOutput("rainfall_title")), 
                  h4(textOutput("baro_title"))
                )
                )

server <- function(input, output, session){
  
  #establish rv for reactive values
  rv <- reactiveValues()
  
  #set end date depending on rainfall or baro
  
  rv$max_date <- reactive(if(length(input$data_type) == 0){
    lubridate::today()
  }else if(input$data_type == 1){
    max_rainfall_date
  }else if(input$data_type == 2){
    max_baro_date
  })
  
  observe(updateAirDateInput(session, "daterange", options = list(maxDate = rv$max_date())))
  
  observeEvent(input$data_type, {
    if(length(input$data_type) > 0 & length(input$daterange[2]) > 0){
    if(input$daterange[2] > rv$max_date()){
     updateAirDateInput(session, "daterange", clear = TRUE)
    }
    }
    })
  
  #toggle state (enable/disable)
  rainfall_go <- reactive(nchar(input$smp_id) > 0 & length(input$daterange[1]) > 0 & length(input$daterange[2]) > 0)
  
  observe(toggleState(id = "rainfall_data", condition = rainfall_go()))
  
  baro_go <- reactive(nchar(input$smp_id) > 0 & length(input$daterange[1]) > 0 & 
    length(input$daterange[2]) > 0 & nchar(input$interval) > 0)
  
  observe(toggleState(id = "baro_data", condition = baro_go()))
  
  #when you click get rainfall data
  observeEvent(input$rainfall_data, {
    
    #marsFetchRainGageData
    rv$rainfall_data <- marsFetchRainGageData(con = poolConn, 
                                          target_id = input$smp_id, 
                                          start_date = input$daterange[1], 
                                          end_date = input$daterange[2], 
                                          daylightsavings = input$dst)
    
    #update header depending on whether rainfall generated properly
    rv$rainfall_header <- if(nrow(rv$rainfall_data) == 0){
      paste("No rainfall data found for", input$smp_id, "from", input$daterange[1], "to", input$daterange[2], ".")
    }else{
      paste("Rainfall data for", input$smp_id, "from", input$daterange[1], "to", input$daterange[2], "has been generated.")
    }
    
    #render header
    output$rainfall_title <- renderText(
      rv$rainfall_header
    )
    
    #if it worked, enable the download button
    if(nrow(rv$rainfall_data) > 0){
      enable("dl_rainfall")
    }
    
  })
  
  #when you click "download rainfall"
  output$dl_rainfall <- downloadHandler(
    filename = function(){
      paste(input$smp_id, input$daterange[1], "to", input$daterange[2], "rainfall.csv", sep = "_")
    },
    
    content = function(file){
      write.csv(rv$rainfall_data, file)
    }
  )
  
  #click "get baro data"
  observeEvent(input$baro_data, {
    
    #fetch baro data. this also generates a report at a predetermined network location. because how it used to be and that's how I'm leaving it (for now)
    rv$barodata <- marsFetchBaroData(con = poolConn, 
                                  target_id = input$smp_id, 
                                  start_date = input$daterange[1], 
                                  end_date = input$daterange[2], 
                                  data_interval = input$interval
    ) 
    
    rv$baro_header <-  paste("Barometric pressure data found for", input$smp_id, "from", input$daterange[1], "to", input$daterange[2], " has been generated. A markdown report is at O:\\Watershed Sciences\\GSI Monitoring\\07 Databases and Tracking Spreadsheets\\13 MARS Analysis Database\\Scripts\\Downloader\\Baro Data Downloader\\Reports")
    
    #render header
    output$baro_title <- renderText(
      rv$baro_header
    )
    
   # enable("dl_baro_report")
    enable("dl_baro_data")
    
  })
  
  output$dl_baro_data <- downloadHandler(
    filename = function(){
      paste(input$smp_id, input$daterange[1], "to", input$daterange[2], "baro.csv", sep = "_")
    },
    
    content = function(file){
      write.csv(rv$barodata, file)
    }
  )
  
  
}
            
#Run this function to run the app!
shinyApp(ui, server)    