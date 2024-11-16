library(shiny)
library(shinydashboard)

# Define UI using shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "RNA Mapper"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Step 1: File Upload", tabName = "step_1", icon = icon("upload")),
      menuItem("Step 2: Data Processing", tabName = "step_2", icon = icon("cogs")),
      menuItem("Step 3: Results", tabName = "step_3", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Home Page
      tabItem(tabName = "home", 
              h3("Welcome to the RNA Mapper Pipeline!"),
              p("This app will guide you through the process of identifying causative mutations from bulk RNA-seq data."),
              p("Please use the menu on the left to navigate through different stages of the pipeline."),
              p("Each step will allow you to upload files, process data, and view results."),
              
              h4("Instructions"),
              p("Follow these steps to use the mutant mapping pipeline:"),
              tags$ol(
                tags$li("Step 1: Configure the pipeline and upload your input files."),
                tags$li("Step 2: Process the data."),
                tags$li("Step 3: View the processed results."),
                tags$li("If you need help, consult the instructions here again.")
              ),
              p("Each step of the pipeline has file upload functionality and detailed instructions.")
      ),
      
      # Step 1 Page with Multiple Tabs
      tabItem(tabName = "step_1",
              h3("Step 1: File Upload"),
              p("In this step, upload your input files for the RNA mapping pipeline."),
              
              tabsetPanel(
                id = "file_upload_tabs",
                tabPanel("Configuration 1",
                         h4("Configuration 1: Basic File Upload"),
                         fileInput("wt_reads_1", "Upload wild type RNA-seq data (SAM or BAM format)", 
                                   multiple = FALSE,
                                   accept = c("text/.sam", "text/.bam", ".sam", ".bam")),
                         fileInput("mut_reads_1", "Upload mutant RNA-seq data (SAM or BAM format)", 
                                   multiple = FALSE,
                                   accept = c("text/.sam", "text/.bam", ".sam", ".bam")),
                         actionButton("upload_button_1", "Upload Files for Configuration 1"),
                         uiOutput("upload_status_1")
                ),
                tabPanel("Configuration 2",
                         h4("Configuration 2: VCF File Upload"),
                         fileInput("SNPdb_2", "Upload VCF containing known mutations", 
                                   multiple = FALSE,
                                   accept = c("text/.vcf", ".vcf")),
                         textInput("MUTdb_url_2", "...or provide a URL download link"),
                         actionButton("upload_button_2", "Upload Files for Configuration 2"),
                         uiOutput("upload_status_2")
                )
              )
      ),
      
      # Step 2 Page
      tabItem(tabName = "step_2",
              h3("Step 2: Data Processing"),
              p("After uploading your files, proceed with the processing of the data."),
              actionButton("process_button", "Run Data Processing"),
              uiOutput("process_status")
      ),
      
      # Step 3 Page
      tabItem(tabName = "step_3",
              h3("Step 3: Results"),
              p("View the results of the data processing here."),
              tableOutput("results_table")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Handle file upload for Configuration 1
  observeEvent(input$upload_button_1, {
    if (is.null(input$wt_reads_1) || is.null(input$mut_reads_1)) {
      output$upload_status_1 <- renderUI({
        p("Please upload all required files for Configuration 1.")
      })
    } else {
      output$upload_status_1 <- renderUI({
        p("Files successfully uploaded for Configuration 1!")
      })
    }
  })
  
  # Handle file upload for Configuration 2
  observeEvent(input$upload_button_2, {
    if (is.null(input$SNPdb_2) && input$MUTdb_url_2 == "") {
      output$upload_status_2 <- renderUI({
        p("Please upload a file or provide a URL for Configuration 2.")
      })
    } else {
      output$upload_status_2 <- renderUI({
        p("Files successfully uploaded for Configuration 2!")
      })
    }
  })
  
  # Handle data processing
  observeEvent(input$process_button, {
    output$process_status <- renderUI({
      p("Processing data...")
    })
    
    # Placeholder processing
    Sys.sleep(2)  # Simulate processing
    output$process_status <- renderUI({
      p("Done!")
    })
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
