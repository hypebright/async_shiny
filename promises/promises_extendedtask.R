library(shiny) # at least version 1.8.1
library(ggplot2)
library(httr)
library(jsonlite)
library(bslib) # at least version 0.7.0
library(future)
library(promises)

# Options for asynchronous strategies: multisession,
# multicore (not Windows/RStudio), cluster
plan(multisession)

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
  
  titlePanel("Shiny 1.8.1 and bslib 0.7.0: ExtendedTask"),
  sidebarLayout(
    sidebarPanel(
      selectInput("company", "Select Company", choices = c("ADYEN.AS", "ASML.AS", "UNA.AS", "HEIA.AS", "INGA.AS", "RDSA.AS", "PHIA.AS", "ABN.AS", "KPN.AS")),
      dateRangeInput("dates", "Select Date Range", start = Sys.Date() - 365, end = Sys.Date()),
      input_task_button("task", "Get stock data (5 seconds)")
    ),
    mainPanel(
      p("This example uses ExtendedTask to run a long-running task in a non-blocking way. 
        It solves the problem of blocking the app while waiting for the task to complete.
        It handles cross-session asynchronicity, and inner-session asynchronicity."),
      textOutput("status"),
      textOutput("time"),
      plotOutput("stock_plot")
    )
  )
  
)

server <- function(input, output, session) {
  
  # reactive values
  reactive_status <- reactiveVal("No task submitted yet")

  # outputs
  output$stock_plot <- renderPlot(stock_results$result())
  
  output$status <- renderText(reactive_status())
  
  output$time <- renderText({
    invalidateLater(1000, session)
    as.character(Sys.time())
  })
  
  # Note that this is not run in a reactive context
  # By putting it at the top level of the server function, 
  # it’s created once per Shiny session; 
  # it “belongs” to an individual visitor to the app, 
  # and is not shared across visitors.
  stock_results <- ExtendedTask$new(function(symbol, start_date, end_date) {
    # Run the task in a non-blocking way
    # You can choose anything here, as long as it returns a promise
    future_promise({
      run_task(symbol, start_date, end_date)
    })
  }) |> bind_task_button("task")
  
  
  # ExtendedTask object does not automatically run when you try to access its 
  # results for the first time. Instead, you need to explicitly call its invoke method
  observeEvent(input$task, {
    reactive_status("Running 🏃")
    stock_results$invoke(input$company, input$dates[1], input$dates[2])
  })
  
  observeEvent(stock_results$result(), {
    reactive_status("Task completed ✅")
  })
  
}

shinyApp(ui = ui, server = server)
