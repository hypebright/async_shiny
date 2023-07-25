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
  
  titlePanel("Calling an API synchronously and get AEX stock data 🐌"),
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
  
  # outputs
  output$stock_plot <- renderPlot(reactive_result())
  
  output$status <- renderText(reactive_status())
  
  output$time <- renderText({
    invalidateLater(1000, session)
    as.character(Sys.time())
  })
  
  # button to submit a task
  observeEvent(input$task, {
    
    reactive_status("Running 🏃")
    
    reactive_result(run_task(symbol = input$company,
                             start_date = input$dates[1],
                             end_date = input$dates[2]))
    
    reactive_status("Done ✅ ")
    
  })
  
}

shinyApp(ui = ui, server = server)
