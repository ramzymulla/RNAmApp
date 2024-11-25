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
          # Header
          h3("Step 2: Map Variants"),
          # Description
          p('Here, you can view your mapped variant markers in order to identify regions of homozygosity.
            Once you have identified your chromosomes and/or scaffolds of interest, you can indicate them
            by selecting the corresponding boxes.'),
          # Input .vcf files to load as graphs
          fileInput(ns("atMarker_files"), "Please indicate your marker files of interest.",
                    multiple = FALSE, accept = c(".vcf")
                    ),
          # Load button
          actionButton(ns("load_button"), "Load files"),
          uiOutput(ns("load_status")),
          # Break
          tags$br(),
          
          # Checkboxes
          fluidRow(
            box(tweaks,width=4,
                h3("Mapping Marker Files"),
                tags$div(class = 'multicol',
                         checkboxGroupInput(ns("markers"), 
                                            label = NULL,
                                            inline=F)
                )
            ),
            # Graphs
            box(width=8,
                actionButton(ns("graph_button"), "Graph selected"),
                uiOutput(ns("graph_status")),
                uiOutput(ns("graphs"))
            )
          )
          
  )
}

step2Module <- function(input, output, session, unified_data) {

  # Load file inputs button
  observeEvent(input$load_button, {
    # Loading message to make sure user gets output if its slow
    output$load_status <- renderUI({
      p("Loading data...")})
    # Load input into markerfiles vector
      markerfiles <- input$atMarker_files
    # Update checkboxes to show marker file names
    updateCheckboxGroupInput(inputId="markers",
                             choices=markerfiles$name,
                             inline=T)
    # Show data has been loaded for user output
    output$load_status <- renderUI({
      p("Data loaded!")})
  })
  
  # Graph markers button
  observeEvent(input$graph_button, {
    # Make a message appear in graphs
    output$graph_status <- renderUI({
      p(input$markers)})
    # Make a plot for the graph
    output$graphs <- renderTable(markerfiles)
  })
  
}
