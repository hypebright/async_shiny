
library(coro)
library(httr)
library(promises)
library(later)

ui <- fluidPage(
  titlePanel("Using co-routines to run jobs concurrently in Shiny"),

  p("This demo demonstrates the coro package. It provides the async() function.
    Async() allows cooperative concurrency. You can run multiple async() 
    functions at the same time (so concurrently) and they are all managed by a
    scheduler in the background. Async() is cooperative because it decides
    whether or not it has to await a result. Await() basically suspends async()
    and gives control back to the scheduler. The scheduler constantly
    monitors whether async() operations are ready to make progress or not.
    In the meantime, your Shiny app will just keep running and is not
    unnecessarily waiting ✨.\n  If you would run this code synchronously,
    it would have taken at least 6 + 5 = 11 seconds. Now it runs
    two jobs concurrently, getting it down to 6 seconds."),

  actionButton(inputId = "start_job",
               label = "Start Expensive Job",
               icon = icon("bolt")),

  br(),

  h4(strong("Result of first async operation:")),

  tableOutput("table_1"),

  h4(strong("Result of second async operation:")),

  tableOutput("table_2")

)

server <- function(input, output) {

  # initiate reactive values
  table_dt <- reactiveVal(NULL)
  times <- reactiveValues(start = NULL,
                          end = NULL)

  # this is the asynchronous function that gets some data
  async_data <- async(function(dataset, seconds) {

    print(paste("Getting ", dataset))

    # await takes an awaitable value, like a promise
    # we use resolve to satisfy a promises after some seconds using later
    # later() simply executes something after a delay, mimicking
    # a long computation
    await(
      promise(function(resolve, reject) {
        later(~resolve(NULL), delay = seconds)
      })
    )

    head(get(dataset), 10)

  })

  # start expensive calculation
  observeEvent(input$start_job, {

    print("Starting job now")

    times$start <- Sys.time()

    # you can also use promise_race() to wait for the first promise object
    # to be fulfilled instead of waiting for all promise objects to be
    # fulfilled, like promise_all() does.
    # This will require more code re-writing than simply changing the
    # function though.
    promise_all(p1 = async_data(dataset = "mtcars",
                                seconds = 6),
                p2 = async_data(dataset = "iris",
                                seconds = 5)) %...>%
      table_dt()

  })

  # Calculate time that passed
  observe({

    req(table_dt())

    times$end <- Sys.time()
    total_time <- times$end - isolate(times$start)

    print(paste("Total time elapsed:", total_time, "seconds"))

  })

  # Display the table data
  output$table_1 <- renderTable({

    table_dt()$p1

  })

  # Display the table data
  output$table_2 <- renderTable({

    table_dt()$p2

  })

}

shinyApp(ui = ui, server = server)
