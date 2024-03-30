library(shiny)
library(promises)
library(future)

# Options for asynchronous strategies: multisession,
# multicore (not Windows/RStudio), cluster
plan(multisession)

ui <- fluidPage(
  titlePanel("Using promises in Shiny (cross-session)"),
  
  textOutput("time"),
  
  br(),
  
  p("Note that clicking 'Start Expensive Job' does not seem
    like a revolution, but the long computation (our complex Sys.sleep() 😉)
    is not blocking for the other users in the same session. However,
    it does block the app for the current user until the promise is resolved.
    You can see that because the clock stops ticking.
    This example demonstrates that by rendering two outputs: a table and text.
    The table uses a promise, the text does not. Yet, we have to wait for the
    table to render until the text appears. So in this example, we
    demonstrate cross-session asynchronicity, not inner-session."),
  
  selectInput(
    inputId = "rows",
    label = "Number of rows",
    choices = c(10, 50, 100, 150),
    selected = 10
  ),
  
  actionButton(inputId = "start_job",
               label = "Start Expensive Job",
               icon = icon("bolt")),
  
  tableOutput("result_table"),
  
  textOutput("text")
)

server <- function(input, output, session) {
  
  output$time <- renderText({
    invalidateLater(1000, session)
    as.character(Sys.time())
  })

  # Display the table data
  output$result_table <- renderTable({
    
    req(input$start_job)
    
    print("Starting job now")
    
    # reactive values and reactive expressions cannot be read from
    # within a future, therefore, you need to read any reactive
    # values/expressions in advance of launching the future
    rows <- as.numeric(input$rows)
    
    future_promise({
      # long computation
      Sys.sleep(5)
      # filter
      iris %>% head(rows)
    })
    
  })
  
  # Display some text to demonstrate inner-session blocking
  output$text <- renderText({
    
    req(input$start_job)
    
    "Only when the promise is resolved, this message appears"
    
  })
  
}

shinyApp(ui = ui, server = server)
