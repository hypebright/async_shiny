library(shiny)
library(promises)
library(mirai)
library(mirai.promises)

# Function to retrieve stock data
run_task <- function(symbol, start_date, end_date) {

  # simulate long retrieval time
  Sys.sleep(5)

  print(symbol)

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

# set 4 persistent workers
# if you would use url = ... you can set a remote worker
daemons(n = 4L)

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
      actionButton("task", "Get stock data (5 seconds each)")
    ),
    mainPanel(
      fluidRow(
        plotOutput("stock_plot1")
      ),
      fluidRow(
        plotOutput("stock_plot2")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # reactive values
  mirai_args <- reactiveValues(args1 = NULL,
                               args2 = NULL)
  
  # check daemon status
  print(daemons())
  
  # button to submit a task
  observeEvent(input$task, {
    
    req(input$company)
    
    # create arguments list dynamically
    for (i in 1:length(input$company)) {
      
      symbol <- input$company[i] 
      
      print(symbol)
      
      args <- list(run_task = run_task, 
                   symbol = symbol,
                   start_date = input$dates[1],
                   end_date = input$dates[2]
      )
      
      mirai_args[[paste0("args", i)]] <- args
      
    }
    
  })
  
  # Note: this code is not dynamic and would need more work
  # put req() outside renderPlot(), otherwise mirai.promises doesn't work properly
  observe({
    
    req(mirai_args$args1)
    
    output$stock_plot1 <- renderPlot(
      mirai(
        {
          library("ggplot2")
          library("jsonlite")
          library("httr")
          run_task(symbol, start_date, end_date)
        },
        .args = mirai_args$args1
      )
      %...>% plot())
    
  })
  
  observe({
    
    req(mirai_args$args2)
    
    output$stock_plot2 <- renderPlot(
      mirai(
        {
          library("ggplot2")
          library("jsonlite")
          library("httr")
          run_task(symbol, start_date, end_date)
        },
        .args = mirai_args$args2
      )
      %...>% plot())
    
  })
  
  # reset daemons on stop
  onStop(function() daemons(0))  
  
}

shinyApp(ui = ui, server = server)
