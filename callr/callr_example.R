library(shiny)
library(callr)

ui <- fluidPage(
  titlePanel("Using callR in Shiny"),
  actionButton("start_job", "Start Expensive Job"),
  tableOutput("result_table")
)

server <- function(input, output, session) {
  
  # initiate reactive values
  check_finished <- reactiveValues(value = FALSE)
  result <- reactiveValues(data = NULL)
  
  # set whatever arguments you want to use
  some_argument <- "virginica"
  
  # callR demonstration
  observeEvent(input$start_job, {
    result$data <-
      
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
    
    check_finished$value <- TRUE 
  })
  
  # this part can be useful if you want to update your UI during the process
  # think about doing an expensive calculation and showing preliminary results
  observe({
    
    if (check_finished$value == TRUE) {
      
      invalidateLater(millis = 1000)
      
      # do something while waiting
      
      # whenever the background job is finished the value of is_alive() will be FALSE
      if (result$data$is_alive() == FALSE) {
        
        
        check_finished$value <- FALSE
        
        output$result_table <- renderTable(result$data$get_result())
        
      }
      
      print(paste0("Still busy at ", Sys.time()))
        print("Finished!")
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
