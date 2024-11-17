step2ModuleUI <- function(id) {
  ns <- NS(id)
  # Step 2 Page
  tabItem(tabName = "step_2",
          h3("Step 2: Map Variants"),
          p('Here, you can view your mapped variant markers in order to identify regions of homozygosity.
            Once you have identified your chromosomes and/or scaffolds of interest, you can indicate them
            by selecting the corresponding boxes.'),
          actionButton("process_button", "Predict Variant Effects"),
          uiOutput("process_status"),
          tags$br(),
          
          box(
            checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
                               choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                               selected = 1)
          ),
          box(
            p("Graphs will go here!")
          )
  )
}

step2Module <- function(input,output,session) {
  observeEvent(input$process_button, {
    output$process_status <- renderUI({
      p("Processing data...")
    })
  })
}
