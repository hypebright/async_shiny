# This is a more fancy version of the stock example, with a custom UI.
# It also tracks the duration of the last task, and shows a status badge.
# The chart is rendered in the main process, so we only return the data frame from the ExtendedTask.

library(shiny) # at least version 1.8.1
library(bslib) # at least version 0.7.0
library(mirai) # at least version 1.0.0
library(echarts4r)

# Function to retrieve stock data
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
  .status-initial  { background: #f0f0f0; color: #888; }
  .status-running  { background: #fff3cd; color: #856404; }
  .status-success  { background: #d1e7dd; color: #0a3622; }
  .status-error    { background: #f8d7da; color: #58151c; }

  .card { border: none; box-shadow: 0 1px 4px rgba(0,0,0,0.07); }
  .sidebar { background: #f8f9fa !important; }
"

ui <- page_sidebar(
  title = "Stock Tracker: ExtendedTask Demo",
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
      "This example uses ExtendedTask to run a long-running task without blocking the session.
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

  # Note that this is not run in a reactive context
  # By putting it at the top level of the server function,
  # it's created once per Shiny session;
  # it "belongs" to an individual visitor to the app,
  # and is not shared across visitors.
  stock_results <- ExtendedTask$new(
    function(...) mirai(run(symbol, start_date, end_date), ...)
  ) |>
    bind_task_button("task")

  # ExtendedTask does not automatically run when you access its results.
  # You need to explicitly call invoke().
  observe({
    task_start(Sys.time())
    stock_results$invoke(
      symbol = input$company,
      start_date = input$dates[1],
      end_date = input$dates[2]
    )
  }) |>
    bindEvent(input$task)

  # Track duration when task completes
  observe({
    if (stock_results$status() == "success") {
      elapsed <- round(
        as.numeric(difftime(Sys.time(), task_start(), units = "secs")),
        1
      )
      task_duration(paste0(elapsed, "s"))
    }
  }) |>
    bindEvent(stock_results$status())

  output$stock_plot <- renderEcharts4r({
    df <- stock_results$result()
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

  output$status_badge <- renderUI({
    s <- stock_results$status()
    label <- switch(
      s,
      "initial" = "Waiting",
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

# set 4 persistent workers
# if you would use daemons(url = ...) you can set a remote worker
with(
  daemons(4),
  {
    # pre-load packages and helper function on all workers
    everywhere(
      {
        library("jsonlite")
        library("httr")
      },
      run = run_task
    )
    runApp(app)
  }
)
