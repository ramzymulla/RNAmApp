step2ModuleUI <- function(id) {
  ns <- NS(id)
  # Step 2 Page
  tabItem(tabName = "step_2",
          h3("Step 2: Map Variants"),
          p("After uploading your files, proceed with the processing of the data."),
          actionButton("process_button", "Run Data Processing"),
          uiOutput("process_status")
  )
}

step2Module <- function(input,output,session) {
  observeEvent(input$process_button, {
    output$process_status <- renderUI({
      p("Processing data...")
    })
  })
}
