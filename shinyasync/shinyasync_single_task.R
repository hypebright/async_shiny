library(shiny) # at least version 1.8.1
library(bslib) # at least version 0.7.0
library(shinyasync) # pak::pak("dawiegriesel/shinyasync@v0.4.0")
# attached in the session so shinyasync ships them to the worker for run_task()
library(httr)
library(jsonlite)
library(ggplot2)

# Function to retrieve stock data. Same as in extendedTask/extendedtask_mirai.R:
# shinyasync ships it to the worker, with the packages it uses, so nothing
# is preloaded by hand.
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
  
  titlePanel("shinyasync"),
  sidebarLayout(
    sidebarPanel(
      selectInput("company", "Select Company", choices = c("ADYEN.AS", "ASML.AS", "UNA.AS", "HEIA.AS", "INGA.AS", "RDSA.AS", "PHIA.AS", "ABN.AS", "KPN.AS")),
      dateRangeInput("dates", "Select Date Range", start = Sys.Date() - 365, end = Sys.Date()),
      input_task_button("task", "Get stock data (5 seconds)")
    ),
    mainPanel(
      p("This example uses shinyasync to run a long-running task in a non-blocking way.
        reactiveAsync() is written like reactive() but evaluates on a mirai worker.
        It handles cross-session asynchronicity, and inner-session asynchronicity."),
      textOutput("status"),
      textOutput("time"),
      plotOutput("stock_plot")
    )
  )
  
)

server <- function(input, output, session) {
  
  # An async reactive, written as reactive() would be. Reactive reads are
  # evaluated in the session, the rest runs on a worker. bindEvent() makes it
  # run on the button only, as with any reactive, and bind_task_button() keeps
  # the button busy while the task runs, as it does for an ExtendedTask.
  stock_results <- reactiveAsync({
    run_task(input$company, input$dates[1], input$dates[2])
  }) |> bindEvent(input$task) |> bind_task_button("task")
  
  # outputs
  # Reading the async reactive halts, like req(), until it has a value.
  output$stock_plot <- renderPlot(stock_results())
  
  # The status is one of "idle", "running", "success" and "error", as for
  # ExtendedTask, so no reactiveVal is needed to track it.
  output$status <- renderText({
    switch(status_async(stock_results),
           idle = "No task submitted yet",
           running = "Running 🏃",
           success = "Task completed ✅",
           error = "Task failed ❌")
  })
  
  output$time <- renderText({
    invalidateLater(1000, session)
    format(Sys.time(), "%H:%M:%S %p")
  })
  
}

app <- shinyApp(ui = ui, server = server)

# set 4 persistent workers, shared by every session of this process
# if you would use pool_async(url = ...) you can set a remote worker
# this call is optional: the first task would start a pool with the defaults
# helper functions and packages are shipped to the workers automatically,
# so there is no everywhere()
pool_async(workers = 4)
runApp(app)
