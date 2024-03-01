library(shiny)
library(callr)

ui <- fluidPage(
  titlePanel("Using callR in Shiny"),
  p("Notice how the clock keeps ticking during the expensive job:"),
  textOutput("time"), 
  br(),
  p("You can also start quick jobs during the expensive job!"),
  hr(),
  actionButton("start_job", "Start Expensive Job"),
  actionButton("start_job2", "Start Quick Job"),
  tableOutput("result_table2"),
  tableOutput("result_table")
)

server <- function(input, output, session) {
  
  # initiate reactive values
  bg_proc <- reactiveVal(NULL)
  check_finished <- reactiveVal(FALSE)
  table_dt <- reactiveVal(NULL)
  
  # render quick task
  output$result_table2 <- renderTable({
    req(input$start_job2)
    iris[sample(nrow(iris), size = 10), ]
  })
  
  # set whatever arguments you want to use
  some_argument <- "virginica"
  
  # callR demonstration
  observeEvent(input$start_job, {
    
    p <-
      
      r_bg(
        
        func =
          function(my_argument) {
            
            # long computation
            Sys.sleep(10)
            
            # using your supplied argument to demonstrate how to use arguments in background process
            iris <- subset(iris, Species == my_argument)
            
            # the result
            return(iris)
            
          },
        
        supervise = TRUE, args = list(my_argument = some_argument)
        
      )
    
    # update reactive vals
    bg_proc(p)
    check_finished(TRUE)
    table_dt(NULL)
  })
  
  # this part can be useful if you want to update your UI during the process
  # think about doing an expensive calculation and showing preliminary results
  observe({
    
    req(check_finished())
    
    invalidateLater(millis = 1000)
    
    # do something while waiting
    cat(paste0("\nStill busy at ", Sys.time()))
    
    p <- isolate(bg_proc())
    
    # whenever the background job is finished the value of is_alive() will be FALSE
    if (p$is_alive() == FALSE) {
      
      cat("\nFinished!")
      
      check_finished(FALSE)
      bg_proc(NULL)
      
      # update the table data with results
      # (do not nest setting `output` directly in observe methods)
      table_dt(p$get_result())
      
    }
    
  })
  
  # Display the table data
  output$result_table <- renderTable(table_dt())
  
  # Display time
  output$time <- renderText({
    invalidateLater(1000, session)
    as.character(Sys.time())
  })
  
}

shinyApp(ui = ui, server = server)
