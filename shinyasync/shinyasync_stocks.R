# This is the shinyasync version of extendedTask/extendedtask_stocks.R, with the same custom UI.
# It also tracks the duration of the last task, and shows a status badge.
# The chart is rendered in the main process, so we only return the data frame from the worker.

library(shiny) # at least version 1.8.1
library(bslib) # at least version 0.7.0
library(shinyasync) # pak::pak("dawiegriesel/shinyasync@v0.3.0")
library(httr)
library(jsonlite)
library(echarts4r)

# Function to retrieve stock data. shinyasync ships it to the worker, with the
# packages it uses, so nothing is preloaded by hand.
run_task <- function(symbol, start_date, end_date) {
  # simulate long retrieval time
  Sys.sleep(5)

  # get stock data
  url <- paste0(
    "https://query1.finance.yahoo.com/v8/finance/chart/",
    symbol,
    "?period1=",
    as.numeric(as.POSIXct(start_date)),
    "&period2=",
    as.numeric(as.POSIXct(end_date)),
    "&interval=1d"
  )

  response <- GET(url)
  json_data <- fromJSON(content(response, as = "text"))
  prices <- json_data$chart$result$indicators$quote[[1]]$close[[1]]
  dates <- as.Date(as.POSIXct(
    json_data$chart$result$timestamp[[1]],
    origin = "1970-01-01"
  ))

  # return data frame — chart is rendered in the main process
  data.frame(Date = dates, Close = prices, stringsAsFactors = FALSE)
}

custom_css <- "
  .metric-label {
    font-size: 0.7rem;
    text-transform: uppercase;
    letter-spacing: 0.07em;
    color: #999;
    margin-bottom: 6px;
  }
  .metric-value {
    font-size: 1.5rem;
    font-weight: 700;
    font-family: monospace;
  }
  .clock-value {
    font-size: 1.5rem;
    font-weight: 700;
    font-family: monospace;
    color: #3d85c8;
  }
  .status-badge {
    display: inline-block;
    padding: 4px 14px;
    border-radius: 20px;
    font-size: 0.85rem;
    font-weight: 600;
  }
  .status-idle     { background: #f0f0f0; color: #888; }
  .status-running  { background: #fff3cd; color: #856404; }
  .status-success  { background: #d1e7dd; color: #0a3622; }
  .status-error    { background: #f8d7da; color: #58151c; }

  .card { border: none; box-shadow: 0 1px 4px rgba(0,0,0,0.07); }
  .sidebar { background: #f8f9fa !important; }
"

ui <- page_sidebar(
  title = "Stock Tracker: shinyasync Demo",
  theme = bs_theme(bootswatch = "flatly"),
  tags$head(tags$style(custom_css)),

  sidebar = sidebar(
    width = 270,
    selectInput(
      "company",
      "Company",
      choices = c(
        "ADYEN.AS",
        "ASML.AS",
        "UNA.AS",
        "HEIA.AS",
        "INGA.AS",
        "RDSA.AS",
        "PHIA.AS",
        "ABN.AS",
        "KPN.AS"
      )
    ),
    dateRangeInput(
      "dates",
      "Date range",
      start = Sys.Date() - 365,
      end = Sys.Date()
    ),
    hr(),
    p(
      "This example uses shinyasync to run a long-running task without blocking the session.
      Watch the clock keep ticking while the task runs.",
      class = "text-muted small"
    ),
    hr(),
    input_task_button("task", "Get stock data (5s)", class = "w-100")
  ),

  layout_column_wrap(
    width = 1 / 3,
    heights_equal = "row",

    card(
      card_body(
        div(class = "metric-label", "Status"),
        uiOutput("status_badge")
      )
    ),

    card(
      card_body(
        div(class = "metric-label", "Session time"),
        div(class = "clock-value", textOutput("time", inline = TRUE))
      )
    ),

    card(
      card_body(
        div(class = "metric-label", "Last task duration"),
        div(
          class = "metric-value text-muted",
          textOutput("duration", inline = TRUE)
        )
      )
    )
  ),

  card(
    card_header("Price chart"),
    echarts4rOutput("stock_plot", height = "420px")
  )
)

server <- function(input, output, session) {
  task_start <- reactiveVal(NULL)
  task_duration <- reactiveVal(NULL)

  # An async reactive, written as reactive() would be. Reactive reads are
  # evaluated in the session, the rest runs on a worker. bindEvent() makes it
  # run on the button only, as with any reactive; there is no invoke().
  stock_results <- reactiveAsync({
    run_task(input$company, input$dates[1], input$dates[2])
  }) |>
    bindEvent(input$task)

  # input_task_button() stays busy until it is told the task is done. For an
  # ExtendedTask, bind_task_button() does this; here the task status does.
  observe({
    running <- status_async(stock_results) == "running"
    update_task_button("task", state = if (running) "busy" else "ready")
  })

  observe({
    task_start(Sys.time())
  }) |>
    bindEvent(input$task)

  # Track duration when task completes
  observe({
    if (status_async(stock_results) == "success") {
      elapsed <- round(
        as.numeric(difftime(Sys.time(), task_start(), units = "secs")),
        1
      )
      task_duration(paste0(elapsed, "s"))
    }
  }) |>
    bindEvent(status_async(stock_results))

  # Reading the async reactive halts, like req(), until it has a value.
  output$stock_plot <- renderEcharts4r({
    df <- stock_results()
    df |>
      e_charts(Date) |>
      e_area(
        Close,
        smooth = TRUE,
        symbol = "none",
        itemStyle = list(color = "#3d85c8"),
        areaStyle = list(color = "rgba(61,133,200,0.15)")
      ) |>
      e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
      e_datazoom(type = "slider", bottom = 10) |>
      e_y_axis(
        name = "Closing Price",
        nameLocation = "middle",
        nameGap = 50,
        splitLine = list(lineStyle = list(type = "dashed", color = "#eee"))
      ) |>
      e_x_axis(splitLine = list(show = FALSE)) |>
      e_grid(left = 70, right = 20, bottom = 60) |>
      e_theme("walden")
  })

  # The status is one of "idle", "running", "success" and "error", as for
  # ExtendedTask, except that "initial" is called "idle".
  output$status_badge <- renderUI({
    s <- status_async(stock_results)
    label <- switch(
      s,
      "idle" = "Waiting",
      "running" = "Running...",
      "success" = "Done",
      "error" = "Error"
    )
    div(class = paste("status-badge", paste0("status-", s)), label)
  })

  output$time <- renderText({
    invalidateLater(1000, session)
    format(Sys.time(), "%H:%M:%S")
  })

  output$duration <- renderText({
    d <- task_duration()
    if (is.null(d)) "—" else d
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
