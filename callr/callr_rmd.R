library(shiny)
library(callr)
library(rmarkdown)
library(shinyjs)

ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  titlePanel("Using callR in Shiny to render a markdown report"),
  
  # note that we use an action button, not a download button
  actionButton(
    inputId = "download_doc",
    label = "Knit markdown in background process"
  ),
  
  # we would still need a hidden download button, to make download functionality work
  downloadButton("download_doc_2", "Download", style = "visibility: hidden;"),
  
  textOutput("success_message")
)

server <- function(input, output, session) {
  
  # initiate reactive values
  result <- reactiveValues(
    check_finished = FALSE,
    result = NULL,
    bg_proc = NULL,
    report_location = NULL
  )
  
  # this part can be useful if you want to update your UI during the process
  # think about doing an expensive calculation and showing preliminary results
  # not a requirement to have, but it shows some extended capabilities of using callR
  observe({
    req(result$check_finished)
      
    # this will invalidate every second
    invalidateLater(millis = 1000)
    
    # do something while waiting
    print(paste0("Still busy at ", Sys.time()))
    
    # Make sure that you read out the stdout and stderr. I.e. you need to call $read_output() and $read_error()
    # See this issue: https://github.com/r-lib/callr/issues/204
    result$bg_proc$read_output()
    result$bg_proc$read_error()
    
    # whenever the background job is finished the value of is_alive() will be FALSE
    if (result$bg_proc$is_alive() == FALSE) {
      
      print("Finished!")
      
      result$check_finished <- FALSE
      result$bg_proc <- NULL
      result$result <- "Ready 🚀"
      
      # simulate a click on the download button, to trigger the actual download
      shinyjs::runjs("document.getElementById('download_doc_2').click();")
      
    }
    
  })
  
  output$success_message <- renderText(result$result)
  
  observeEvent(input$download_doc, {
    
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    # this also makes the file available to the callR background process
    temp_report_in <- file.path(tempdir(), paste0(as.integer(Sys.time()), "-markdown-doc.Rmd"))
    file.copy("./markdown-doc.Rmd", temp_report_in, overwrite = TRUE)
    
    temp_report_out <- file.path(tempdir(), paste0(as.integer(Sys.time()), "-markdown-doc.pdf"))
    
    # used in the downloadHandler
    result$report_location <- temp_report_out
    
    # Set up parameters to pass to Rmd document, can be any input coming from the app
    # just hard coded here
    input_params <- list(title = "My R Markdown report!")
    
    # create a background process
    result$bg_proc <-
      
      r_bg(
        
        func =
          function(my_params, input_location, output_location) {
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            # note that all of these are function arguments and need to be specified using args = ...!
            rmarkdown::render(input = input_location,
                              output_file = output_location,
                              output_format = "pdf_document",
                              params = my_params
            )
            
            return("finished")
            
          },
        
        supervise = TRUE, args = list(my_params = input_params,
                                      output_location = temp_report_out,
                                      input_location = temp_report_in)
        
      )
    
    result$result <- "Rendering ⏳"
    result$check_finished <- TRUE
    
  })
  
  # this gets triggered by the JS click event
  output$download_doc_2 <- downloadHandler(
    
    filename = function() {
      "markdown-doc.pdf"
    },
    
    content = function(file) {
      # copy the file from the temporary location that we set in result$report_location
      file.copy(result$report_location, file)
    }
    
  )
  
}

shinyApp(ui = ui, server = server)
