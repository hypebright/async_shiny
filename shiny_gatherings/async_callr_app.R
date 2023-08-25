library(callr) #NEW
library(shiny)
library(ggplot2)
library(httr)
library(jsonlite)

# Function to retrieve stock data
run_task <- function(symbol, start_date, end_date) {
  
  # simulate long retrieval time
  Sys.sleep(5)
  
  # get stock data
  url <- paste0("https://query1.finance.yahoo.com/v8/finance/chart/", symbol, "?period1=", 
                as.numeric(as.POSIXct(start_date)), "&period2=", as.numeric(as.POSIXct(end_date)), 
                "&interval=1d")
  
  response <- GET(url)
  json_data <- fromJSON(content(response, as = "text"))
  prices <- json_data$chart$result$indicators$quote[[1]]$close[[1]]
  dates <- as.Date(as.POSIXct(json_data$chart$result$timestamp[[1]], origin = "1970-01-01"))
  
  stock <- data.frame(Date = dates, Close = prices, stringsAsFactors = FALSE)
  
  ggplot(stock, aes(x = Date, y = Close)) +
    geom_line(color = "steelblue") +
    labs(x = "Date", y = "Closing Price") +
    ggtitle(paste("Stock Data for", symbol)) +
    theme_minimal()
  
}

ui <- fluidPage(
  
  titlePanel("callR: calling an API asynchronously and get AEX stock data 🚀 "), #NEW
  sidebarLayout(
    sidebarPanel(
      selectInput("company", "Select Company", choices = c("ADYEN.AS", "ASML.AS", "UNA.AS", "HEIA.AS", "INGA.AS", "RDSA.AS", "PHIA.AS", "DSM.AS", "ABN.AS", "KPN.AS")),
      dateRangeInput("dates", "Select Date Range", start = Sys.Date() - 365, end = Sys.Date()),
      actionButton("task", "Get stock data (5 seconds)")
    ),
    mainPanel(
      textOutput("status"),
      textOutput("time"),
      plotOutput("stock_plot")
    )
  )
  
)

server <- function(input, output, session) {
  
  # reactive values
  reactive_result <- reactiveVal(ggplot())
  reactive_status <- reactiveVal("No task submitted yet")
  bg_proc <- reactiveVal(NULL) #NEW
  reactive_poll <- reactiveVal(FALSE) #NEW
  
  # outputs
  output$stock_plot <- renderPlot(reactive_result())
  
  output$status <- renderText(reactive_status())
  
  output$time <- renderText({
    invalidateLater(1000, session)
    as.character(Sys.time())
  })
  
  # button to submit a task
  observeEvent(input$task, {
    
    p <-
      
      r_bg(
        
        func =
          function(run_task, symbol, start_date, end_date) {
            
            library(httr)
            library(jsonlite)
            library(ggplot2)
            
            # the result
            return(run_task(symbol, start_date, end_date))
            
          },
        
        supervise = TRUE, 
        args = list(run_task = run_task,
                    symbol = input$company,
                    start_date = input$dates[1],
                    end_date = input$dates[2])
        
      )
    
    # update reactive vals
    bg_proc(p)
    reactive_poll(TRUE)
    reactive_status("Running 🏃")
    
  })
  
  observe({
    
    req(reactive_poll())
    
    invalidateLater(millis = 1000)
    
    p <- bg_proc()
    
    # whenever the background job is finished the value of is_alive() will be FALSE
    if (p$is_alive() == FALSE) {
      
      reactive_status("Done ✅ ")
      
      reactive_poll(FALSE)
      bg_proc(NULL)
      
      # update the table data with results
      reactive_result(p$get_result())
      
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
