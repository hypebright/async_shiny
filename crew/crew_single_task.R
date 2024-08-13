library(crew) # version 0.9.5
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
    paste(format(Sys.time()), "All tasks completed 🚀")
  }
}

ui <- fluidPage(
  
  titlePanel("Async programming in Shiny: calling an API asynchronously and get AEX stock data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("company", "Select Company", choices = c("ADYEN.AS", "ASML.AS", "UNA.AS", "HEIA.AS", "INGA.AS", "RDSA.AS", "PHIA.AS", "ABN.AS", "KPN.AS")),
      dateRangeInput("dates", "Select Date Range", start = Sys.Date() - 365, end = Sys.Date()),
      actionButton("task", "Get stock data (5 seconds)")
    ),
    mainPanel(
      textOutput("status"),
      plotOutput("stock_plot")
    )
  )
  
)

server <- function(input, output, session) {
  # reactive values and outputs
  reactive_result <- reactiveVal(ggplot())
  reactive_status <- reactiveVal("No task submitted yet")
  reactive_poll <- reactiveVal(FALSE)
  output$stock_plot <- renderPlot(reactive_result())
  output$status <- renderText(reactive_status())
  
  # crew controller
  controller <- crew_controller_local(workers = 4, seconds_idle = 10)
  controller$start()
  
  # make sure to terminate the controller on stop
  onStop(function() controller$terminate())
  
  # button to submit a task
  observeEvent(input$task, {
    controller$push(
      command = run_task(symbol, start_date, end_date),
      # pass the function to the workers, and arguments needed
      data = list(run_task = run_task,
                  symbol = input$company,
                  start_date = input$dates[1],
                  end_date = input$dates[2]), 
      packages = c("httr", "jsonlite", "ggplot2")
    )
    reactive_poll(TRUE)
    
  })
  
  # event loop to collect finished tasks
  observe({
    req(reactive_poll())
    invalidateLater(millis = 100)
    result <- controller$pop()$result
    
    if (!is.null(result)) {
      reactive_result(result[[1]])
      print(controller$summary()) # get a summary of workers
    }
    
    # wait for tasks to be assigned to workers
    if (sum(controller$client$summary()$assigned) > 0) {
      reactive_status(status_message(n = sum(controller$client$summary()$assigned) - sum(controller$client$summary()$complete)))
      reactive_poll(controller$nonempty())
    } else {
      reactive_status(paste(format(Sys.time()), "launching necessary workers..."))
    }
    
  })
}

shinyApp(ui = ui, server = server)
