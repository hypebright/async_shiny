
library(promises)
library(future)

# Options for asynchronous strategies: multisession,
# multicore (not Windows/RStudio), cluster
plan(multisession)

ui <- fluidPage(
  titlePanel("Using promises in Shiny (inner-session)"),

  p("Note that clicking 'Start Expensive Job' does not seem
    like a revolution, but the long computation (our complex Sys.sleep() 😉)
    is not blocking for the current or other users in the same session.
    This is both cross-session AND inner-session asynchronicity."),

  selectInput(
    inputId = "rows",
    label = "Number of rows",
    choices = c(10, 50, 100, 150),
    selected = 10
  ),

  actionButton(inputId = "start_job",
               label = "Start Expensive Job",
               icon = icon("bolt")),

  tableOutput("result_table")
)

server <- function(input, output, session) {

  # initiate reactive values
  table_dt <- reactiveVal(NULL)

  # start expensive calculation
  observeEvent(input$start_job, {

    print("Starting job now")

    # update actionButton to show we are busy
    updateActionButton(inputId = "start_job",
                       label = "Start Expensive Job",
                       icon = icon("sync", class = "fa-spin"))

    # reactive values and reactive expressions cannot be read from
    # within a future, therefore, you need to read any reactive
    # values/expressions in advance of launching the future
    rows <- input$rows

    future_promise({
      # long computation
      Sys.sleep(5)
      # filter
      iris %>% head(rows)
    }) %...>%
      table_dt()

    print("This will execute immediately,
          even though our promise isn't resolved yet")

  })

  # Display the table data
  output$result_table <- renderTable({

    table_dt()

  })

  observe({

    req(table_dt())

    # update actionButton to show data is available and
    # we're ready for another calculation
    updateActionButton(inputId = "start_job",
                       label = "Start Expensive Job",
                       icon = icon("bolt"))

  })

}

shinyApp(ui = ui, server = server)
