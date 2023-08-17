library(crew)
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

status_message <- function(n) {
  if (n > 0) {
    paste(format(Sys.time()), "tasks in progress ⏳ :", n)
  } else {
    "All tasks completed 🚀"
  }
}

ui <- fluidPage(
  
  titlePanel("Async programming in Shiny: calling an API asynchronously and get AEX stock data"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("company", 
                     "Select Company (max 2 supported)", 
                     choices = c("ADYEN.AS", "ASML.AS", "UNA.AS", "HEIA.AS", "INGA.AS", "PHIA.AS", "DSM.AS", "ABN.AS", "KPN.AS"),
                     selected = c("ADYEN.AS", "ASML.AS"),
                     multiple = TRUE,
                     options = list(maxItems = 2)),
      dateRangeInput("dates", "Select Date Range", start = Sys.Date() - 365, end = Sys.Date()),
      actionButton("task", "Get stock data (5 seconds)")
    ),
    mainPanel(
      textOutput("status"),
      plotOutput("stock_plot"),
      plotOutput("stock_plot2")
    )
  )
  
)

server <- function(input, output, session) {
  # reactive values and outputs
  reactive_result <- reactiveVal(NULL)
  reactive_result2 <- reactiveVal(NULL)
  reactive_result_retrieved <- reactiveVal(FALSE)
  reactive_result2_retrieved <- reactiveVal(FALSE)
  reactive_status <- reactiveVal("No task submitted yet")
  reactive_poll <- reactiveVal(FALSE)
  output$stock_plot <- renderPlot(reactive_result())
  output$stock_plot2 <- renderPlot(reactive_result2())
  output$status <- renderText(reactive_status())
  
  # crew controller
  controller <- crew_controller_local(workers = 4, seconds_idle = 10)
  controller$start()
  
  # make sure to terminate the controller on stop
  onStop(function() controller$terminate())
  
  # button to submit a task
  observeEvent(input$task, {
    
    # create arguments list dynamically
    for (i in 1:length(input$company)) {
      
      symbol <- input$company[i] 
      
      print(symbol)
      
      controller$push(
        command = run_task(symbol, start_date, end_date),
        # pass the function to the workers, and arguments needed
        data = list(run_task = run_task,
                    symbol = symbol,
                    start_date = input$dates[1],
                    end_date = input$dates[2]), 
        packages = c("httr", "jsonlite", "ggplot2")
      )
    }
    
    reactive_poll(TRUE)
    
  })
  
  # event loop to collect finished tasks
  observe({
    req(reactive_poll())
    invalidateLater(millis = 500)
    result <- controller$pop()$result
    
    if (!is.null(result)) {
      if (reactive_result_retrieved() == FALSE) {
        reactive_result(result[[1]])
        reactive_result_retrieved(TRUE)
      } else if(reactive_result2_retrieved() == FALSE) {
        reactive_result2(result[[1]])
        reactive_result2_retrieved(TRUE)
      }
      
      print(controller$summary()) # get a summary of workers
      
    }
    
    reactive_status(status_message(n = sum(controller$schedule$summary())))
    reactive_poll(controller$nonempty())
    
  })
}

shinyApp(ui = ui, server = server)
