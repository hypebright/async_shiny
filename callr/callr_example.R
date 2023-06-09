library(shiny)
library(callr)

ui <- fluidPage(
  titlePanel("Using callR in Shiny"),
  actionButton("start_job", "Start Expensive Job"),
  tableOutput("result_table")
)

server <- function(input, output, session) {
  
  # initiate reactive values
  bg_proc <- reactiveVal(NULL)
  check_finished <- reactiveVal(FALSE)
  table_dt <- reactiveVal(NULL)
  
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
    print(paste0("Still busy at ", Sys.time()))
    
    p <- isolate(bg_proc())
    
    # whenever the background job is finished the value of is_alive() will be FALSE
    if (p$is_alive() == FALSE) {
      
      print("Finished!")
      
      check_finished(FALSE)
      bg_proc(NULL)
      
      # update the table data with results
      # (do not nest setting `output` directly in observe methods)
      table_dt(p$get_result())
      
    }
    
  })

  # Display the table data
  output$result_table <- renderTable(table_dt())

}

shinyApp(ui = ui, server = server)
