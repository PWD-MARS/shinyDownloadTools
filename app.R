#Fetch rainfall from MARS database

  #0 set up ------
  #0.0 load libraries -----
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
  #work with dates
  library(lubridate)
  
  options(stringsAsFactors=FALSE)
  
  #0.1 set db connection -------
  #using a pool connection so separate connections are unified
  #gets environmental variables saved in local or pwdrstudio environment
  poolConn <- dbPool(odbc(), dsn = "mars_data", uid = Sys.getenv("new_shiny_uid"), pwd = Sys.getenv("shiny_pwd"))
  # poolConn <- dbPool(odbc(), dsn = "mars_data")
  
  #disconnect from db on stop 
  onStop(function(){
    poolClose(poolConn)
  })
  
  #js warning about leaving page
  #jscode <- 'window.onbeforeunload = function() { return "Please use the button on the webpage"; };'
  
  #0.2 global variables -------
  #define global variables that will be required each time the UI runs

  refer_date <- today() %m-% months(2)

  
  #min_rainfall_date <- '1990-01-01'
  max_rainfall_date <- as.Date(odbc::dbGetQuery(poolConn, paste0("select max(dtime_edt) from data.gage_rain where dtime_edt > '", refer_date, "'")) %>% pull)
  
  max_baro_date <- as.Date(odbc::dbGetQuery(poolConn, paste0("SELECT max(dtime_est) FROM data.barodata_neighbors where dtime_est > '", refer_date, "'")) %>% pull)
  
  max_date = max(c(max_rainfall_date, max_baro_date))

#1.0 UI --------
ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("MARS Fetch Rainfall & Baro Data"),
                #1.1 SidebarPanel --------
                sidebarPanel(
                  selectizeInput("smp_id", "SMP ID", 
                                 choices = NULL, 
                                 options = list(
                                   placeholder = 'Select an Option', 
                                   onInitialize = I('function() { this.setValue(""); }')
                                 )),
                 # selectInput("smp_id", "SMP ID", choices = c("", smp_id), selected = NULL), 
                  selectInput("data_type", "Data Type", choices = c("", "Rainfall" = 1, "Baro" = 2), selected = NULL),
                  airDatepickerInput("daterange", "Date Range", range = TRUE),
                  verbatimTextOutput("res"),
                  conditionalPanel(condition = 'input.data_type == 2',
                    selectInput("interval", "Interval (min)", choices = c("", 5, 15), selected = NULL)
                    ), 
                  conditionalPanel(condition = 'input.data_type == 1', 
                    checkboxInput("dst", "Daylight Saving (leave unchecked if doing QA/QC)")
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
                #1.2 Text Outputs -------
                mainPanel(
                  #create a header to let user know that the data was retrieved/generated. 
                  h4(textOutput("rainfall_title")), 
                  h4(textOutput("baro_title"))
                )
                )

  #2.0 Server -------
server <- function(input, output, session){
  
  #2.0.1 Initial set up --------
  #establish rv for reactive values
  rv <- reactiveValues()
  
  #check for max dates out of range
  if(is.na(max_rainfall_date)){
    rain_error <- "The latest rainfall data is 2 months or older and must be updated."
    rv$rainfall_header <- rain_error
    output$rainfall_title <- renderText(
      rv$rainfall_header
    )
  }
  
  if(is.na(max_baro_date)){
    baro_error <- "The latest barometric pressure data is 2 months or older and must be updated."
    rv$baro_header <- baro_error
    output$baro_title <- renderText(
      rv$baro_header
    ) 
    
  }
  
  #query all SMP IDs
  smp_id <- odbc::dbGetQuery(poolConn, paste0("select distinct smp_id from admin.smp_loc")) %>% 
    dplyr::arrange(smp_id) %>% 
    dplyr::pull()
  
  #updates smp ids
  updateSelectizeInput(session, "smp_id", choices = smp_id, selected = character(0), server = TRUE)
  
  #set end date depending on rainfall or baro
  
  rv$max_date <- reactive(if(length(input$data_type) == 0){
    lubridate::today()
  }else if(input$data_type == 1){
    max_rainfall_date
  }else if(input$data_type == 2){
    max_baro_date
  })
  
  
  
  observe(updateAirDateInput(session, "daterange", options = list(maxDate = rv$max_date())))
  
  #2.1 sanitize/prepare inputs ------
  rv$start_date <- reactive(lubridate::ymd(input$daterange[1], tz = "EST"))
  rv$end_date <- reactive(lubridate::ymd(input$daterange[2], tz = "EST"))
  
  #update date ranges when switching data type
  observeEvent(input$data_type, {
    if(length(input$data_type) > 0 & length(input$daterange[2]) > 0){
    if(input$daterange[2] > rv$max_date()){
     updateAirDateInput(session, "daterange", clear = TRUE)
    }
    }
    })
  
  #update interval to say "mins"
  rv$interval <- reactive(paste(input$interval, "mins"))
  
  #2.2 toggle states based on inputs -----
  #toggle state (enable/disable)
  rainfall_go <- reactive(nchar(input$smp_id) > 0 & length(input$daterange[1]) > 0 & length(input$daterange[2]) > 0)
  
  observe(toggleState(id = "rainfall_data", condition = rainfall_go()))
  
  baro_go <- reactive(nchar(input$smp_id) > 0 & length(input$daterange[1]) > 0 & 
    length(input$daterange[2]) > 0 & nchar(input$interval) > 0)
  
  observe(toggleState(id = "baro_data", condition = baro_go()))
  
  #2.3 get rainfall data button ------
  #when you click get rainfall data
  observeEvent(input$rainfall_data, {
    
    #marsFetchRainGageData
    rv$rainfall_data <- marsFetchRainfallData(con = poolConn, 
                                          target_id = input$smp_id, 
                                          source = "gage",
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
  
  #2.4 download rainfall ------
  #when you click "download rainfall"
  output$dl_rainfall <- downloadHandler(
    filename = function(){
      paste(input$smp_id, input$daterange[1], "to", input$daterange[2], "rainfall.csv", sep = "_")
    },
    
    content = function(file){
      write.csv(rv$rainfall_data, file)
    }
  )
  
  
  #2.5 click "get baro data" button ----
  observeEvent(input$baro_data, {
    
    #fetch baro data
    #why is it only getting first day ugh 
    rv$barodata <- marsFetchBaroData(con = poolConn, 
                                  target_id = input$smp_id, 
                                  start_date = rv$start_date(), 
                                  end_date = rv$end_date(), 
                                  data_interval = rv$interval()
    ) 
    
    rv$baro_header <-  paste("Barometric pressure data found for", input$smp_id, "from", input$daterange[1], "to", input$daterange[2], " has been generated.")
    
    #render header
    output$baro_title <- renderText(
      rv$baro_header
    )
    
   # enable("dl_baro_report")
    enable("dl_baro_data")
    
  })
  
  #2.6 download baro data -----
  output$dl_baro_data <- downloadHandler(
    filename = function(){
      paste(input$smp_id, input$daterange[1], "to", input$daterange[2], "baro.csv", sep = "_")
    },
    
    content = function(file){
      write.csv(rv$barodata, file)
    }
  )
  
  
}

#3.0 shiny runApp --------           
#Run this function to run the app!
shinyApp(ui, server)    