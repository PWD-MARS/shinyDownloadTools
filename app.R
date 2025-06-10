#Fetch rainfall from MARS database

library(shiny)
library(shinythemes)
library(pwdgsi)
library(tidyverse)
library(pool)
library(shinyjs)
library(shinyWidgets)
library(lubridate)

options(stringsAsFactors=FALSE)

# Connect to DB
poolConn <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_prod",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("shiny_pwd"),
  timezone = NULL)

# Disconnect from DB when stopped
onStop(function(){
  poolClose(poolConn)
})


# Refer dat is today minus 2 months
refer_date <- today() %m-% months(2)


# The last rain gage date
max_rainfall_date <- as.Date(odbc::dbGetQuery(poolConn, paste0("select max(dtime) from data.tbl_gage_rain where dtime > '", refer_date, "'")) %>% pull)
# The last baro date
max_baro_date <- as.Date(odbc::dbGetQuery(poolConn, paste0("SELECT max(dtime) FROM data.viw_barodata_neighbors where dtime > '", refer_date, "'")) %>% pull)
# The last date of baro or rain gage data
#### Why is this needed?
max_date = max(c(max_rainfall_date, max_baro_date))

NY <- NA
EST <- NA


# UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("MARS Fetch Rainfall & Baro Data"),
                # Sidebar Panel
                sidebarPanel(
                  selectizeInput("smp_id", "SMP ID", 
                                 # Intialize with zero choices 
                                 choices = NULL, 
                                 options = list(
                                   placeholder = 'Select an Option', 
                                   # JS: Set initial value to ""
                                   onInitialize = I('function() { this.setValue(""); }')
                                 )),
                  # Assign numeric values to rainfall and baro data
                  selectInput("data_type", "Data Type", choices = c("", "Rainfall" = 1, "Baro" = 2), selected = NULL),
                  airDatepickerInput("daterange", "Date Range", range = TRUE),
                  #### What is this for?
                  verbatimTextOutput("res"),
                  conditionalPanel(condition = 'input.data_type == 2',
                    selectInput("interval", "Interval (min)", choices = c("", 5, 15), selected = NULL)
                    ),
                  conditionalPanel(condition = 'input.data_type == 1', 
                    actionButton("rainfall_data", "Get Rainfall Data"), 
                    disabled(downloadButton("dl_rainfall", "Download Rainfall .csv"))
                    ), 
                  conditionalPanel(condition = 'input.data_type == 2', 
                    actionButton("baro_data", "Get Baro Data"), 
                    disabled(downloadButton("dl_baro_data", "Download Baro .csv"))
                    ) 
                  ),
                useShinyjs(),
                # Main panel
                mainPanel(
                  # Header to let user know that the data was retrieved/generated. 
                  #### Header defaults to baro?
                  h4(textOutput("rainfall_title")), 
                  h4(textOutput("baro_title"))
                )
                )

  # Server
server <- function(input, output, session){
  
  # Initialize reactiveValues()
  rv <- reactiveValues()
  
  # Update rainfall title based on requested/max dates
  # If rain gage data hasn't been updated since refer_date, update header to reflect
  if(is.na(max_rainfall_date)){
    rain_error <- "The latest rainfall data is 2 months or older and must be updated."
    rv$rainfall_header <- rain_error
    output$rainfall_title <- renderText(rv$rainfall_header)
  } else {
    # based on current logs structure milestones greater than 5 mean that gage data has been retrieved properly and have moved on to radar
    #### Why are we doing this from the logs? Why aren't we doing this from the actual data tables?
    rainfall_log <- dbGetQuery(poolConn, "select max(date) from log.tbl_script_rainfall where milestone > 5") %>% pull
    rv$rainfall_header <- paste0("Rainfall data was last updated on: ",
                                 as.Date(rainfall_log),
                                 ". The latest value is from: ", 
                                 max_rainfall_date,
                                 ".")
  }
  
  # If rain gage data hasn't been updated since refer_date, update header to reflect
  if(is.na(max_baro_date)){
    baro_error <- "The latest barometric pressure data is 2 months or older and must be updated."
    rv$baro_header <- baro_error
    output$baro_title <- renderText(rv$baro_header)
   } else {
     baro_log <- dbGetQuery(poolConn, "select max(date) from log.tbl_script_baro where exit_code = 0") %>% pull
     rv$baro_header <- paste0("Barometric data was last updated on: ",
                              as.Date(baro_log),
                              ". The latest value is from: ", 
                              max_baro_date,
                              ".")
   }
  
  
  # Get list of all smp_ids
  #### Why are we doing this?
  smp_id <- dbGetQuery(poolConn, paste0("select distinct smp_id from admin.tbl_smp_loc")) %>% 
    dplyr::arrange(smp_id) %>% 
    dplyr::pull()
  
  # Update input smp_ids with all distinct ones.
  updateSelectizeInput(session, "smp_id", choices = smp_id, selected = character(0), server = TRUE)
  
  
  # Set end date depending on rainfall or baro
  rv$max_date <- reactive(
    # Default to today
    if(length(input$data_type) == 0) {
      lubridate::today()
      # If rainfall
      } else if (input$data_type == 1) {
        max_rainfall_date
        # If baro 
        } else if(input$data_type == 2) {
          max_baro_date
  })
  # Check for changes in daterange 
  observe(updateAirDateInput(session, "daterange", options = list(maxDate = rv$max_date())))
  
  # Store dates in rv
  rv$start_date <- reactive(lubridate::ymd(input$daterange[1], tz = "America/New_York"))
  rv$end_date <- reactive(lubridate::ymd(input$daterange[2], tz = "America/New_York"))
  
  # Update date ranges and header when switching data type
  observeEvent(input$data_type, {
    # If there is no data type and there is an end date
    #### This block should be the else?
    if(length(input$data_type) > 0 & length(input$daterange[2]) > 0) {
      # If the end date is greater than max date, clear
      if(input$daterange[2] > rv$max_date()) {
        updateAirDateInput(session, "daterange", clear = TRUE)
      }
    }
    # If rainfall data
    if(input$data_type == 1) {
      output$rainfall_title <- renderText(rv$rainfall_header)
      output$baro_title <- renderText("")
    }
    # If baro data
    if(input$data_type == 2){
      output$baro_title <- renderText(rv$baro_header)
      output$rainfall_title <- renderText("")
      }
    })
  # Add "mins" to the interval
  rv$interval <- reactive(paste(input$interval, "mins"))
  
  # Toggle states based on inputs
  # TRUE/FALSE if smp_id is a least a character and there is an start and end date.
  #### Can we change name of this to be more reflective of what it's doing? 
  #### Do we need a duplicate one if they are the same statement?
  rainfall_go <- reactive(nchar(input$smp_id) > 0 & length(input$daterange[1]) > 0 & length(input$daterange[2]) > 0)
  # Observe status of rainfall_go 
  observe(toggleState(id = "rainfall_data", condition = rainfall_go()))
  
  baro_go <- reactive(nchar(input$smp_id) > 0 & length(input$daterange[1]) > 0 & 
    length(input$daterange[2]) > 0 & nchar(input$interval) > 0)
  
  observe(toggleState(id = "baro_data", condition = baro_go()))
  
  # Get rainfall data
  # Observe clicked rainfall_data button
  observeEvent(input$rainfall_data, {
    # Catch error from marsFetchRainfallData
    tryCatch(rv$rainfall_data <- marsFetchRainfallData(con = poolConn, 
                                         target_id = input$smp_id, 
                                         source = "gage",
                                         start_date = input$daterange[1], 
                                         end_date = input$daterange[2], 
                                         daylightsavings = FALSE),
             error = function(e) {
               rv$rainfall_data <- NULL
               showModal(
                 modalDialog(
                   title = "No available data",
                   easy_close = TRUE,
                   "There is no data available for this date range"
                   )
               )
              })
  # If rv$rainfall exists
  if(!is.null(rv$rainfall_data)) {
    # Moving America/New_York to EST to match baro CSV files for QAQC
    rv$rainfall_data <- rv$rainfall_data |>
      mutate(dtime = with_tz(dtime, tz = "EST"))
    rv$rainfall_header <- paste("Rainfall data for", input$smp_id, "from",
                                input$daterange[1],"to", input$daterange[2],
                                "has been generated.")
    # Enable download button
    enable("dl_rainfall")
  } else {
    # Make sure download is disabled
    disable("dl_rainfall")
    rv$rainfall_header <- paste("No Rainfall data for", input$smp_id, "from",
                                input$daterange[1],"to", input$daterange[2],
                                "is available.")
    # Render header
    output$rainfall_title <- renderText(rv$rainfall_header)
    }
  })

  # Download rainfall CSV file
  #### The download file should be one function/chunks for both data types.
  # Observe clicking download button
  output$dl_rainfall <- downloadHandler(
    filename = function(){
      paste(input$smp_id, input$daterange[1], "to", input$daterange[2], "rainfall.csv", sep = "_")
    },

    content = function(file){
      write.csv(rv$rainfall_data, file)
    }
  )

  # Observe Get Baro data button
  #### Should be just get data, which downloads the file or gives soft error
  # Catch error from marsFetchBaroData
  observeEvent(input$baro_data, {
    tryCatch(rv$baro_data <- marsFetchBaroData(con = poolConn,
                                     target_id = input$smp_id, 
                                     start_date = rv$start_date(), 
                                     end_date = rv$end_date(), 
                                     data_interval = rv$interval()),
             error = function(e) {
               rv$baro_data <- NULL
               showModal(
                 modalDialog(
                   title = "No available data",
                   easy_close = TRUE,
                   "There is no avaiable data for this date range"
                 )
               )
             })
    # If baro_data returns 1+ observations
    if(!is.null(rv$baro_data)) {
      rv$barodata <- rv$baro_data |>
        # Change time zone to EST for QAQC
        mutate(dtime = with_tz(dtime, tz = "EST"))
      rv$baro_header <-  paste("Barometric pressure data found for", input$smp_id, "from", input$daterange[1], "to", input$daterange[2], " has been generated.")

      # render header
      output$baro_title <- renderText(rv$baro_header)
      # Enable download button
      enable("dl_baro_data")
    } else {
      # Make sure download is disabled
      disable("dl_baro_data")
      rv$baro_header <- paste("No baro data for", input$smp_id, "from",
                                  input$daterange[1],"to", input$daterange[2],
                                  "is available.")
      # Render header
      output$baro_title <- renderText(rv$baro_header)
    }
  })
  
  # Download baro data
  output$dl_baro_data <- downloadHandler(
    filename = function(){
      paste(input$smp_id, input$daterange[1], "to", input$daterange[2], "baro.csv", sep = "_")
    },
    
    content = function(file){
      write.csv(rv$barodata, file)
    }
  )
}

shinyApp(ui, server)    