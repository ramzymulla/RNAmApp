step3ModuleUI <- function(id) {
  ns <- NS(id)
  # Step 3 Page
  tabItem(tabName = "step_3",
          h3("Step 3: Results"),
          p("View the results of the data processing here."),
          tableOutput("results_table")
  )
}

step3Module <- function(input,output,session,unified_data) {
  observeEvent(input$process_button, {
    output$process_status <- renderUI({
      p("Processing data...")
    })
  })
}
