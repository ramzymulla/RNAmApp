tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   -webkit-column-count: auto; /* Chrome, Safari, Opera */
                                   -webkit-column-width: 6rem;
                                   -moz-column-count: auto;    /* Firefox */
                                   -moz-column-width: 6rem;
                                   column-count: auto;
                                   column-width: 6rem;
                                   -moz-column-fill: balance;
                                   -column-fill: balance;
                                 } 
                                 ")) 
  ))

step2ModuleUI <- function(id) {
  ns <- NS(id)
  # Step 2 Page
  tabItem(tabName = "step_2",
          h3("Step 2: Map Variants"),
          p('Here, you can view your mapped variant markers in order to identify regions of homozygosity.
            Once you have identified your chromosomes and/or scaffolds of interest, you can indicate them
            by selecting the corresponding boxes.'),
          actionButton(ns("process_button"), "Predict Variant Effects"),
          uiOutput(ns("process_status")),
          tags$br(),
          
          fluidRow(
            box(tweaks,width=4,
                h3("Chromosomes"),
                tags$div(class = 'multicol',
                         checkboxGroupInput(ns("chromosomes"), 
                                            label = NULL, 
                                            choices = paste("chr",as.character(1:25),sep=''),
                                            selected = paste("chr",as.character(1:25),sep=''),
                                            inline=F)
                )
            ),
            box(width=8,
              h3("Graphs will go here!")
            )
          )
          
  )
}

step2Module <- function(input,output,session,unified_data) {
  
  observeEvent(unified_data$chr_list, {
    updateCheckboxGroupInput(inputId="chromosomes",
                             choices = unified_data$chr_list,
                             selected = unified_data$chr_list,
                             inline=T)
  }, ignoreInit = T)
  
  observeEvent(input$process_button, {
    output$process_status <- renderUI({
      p("Processing data...")
    })
  })
}
