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
      # Home Page with integrated instructions
      tabItem(tabName = "home", 
              h3("Welcome to the RNA Mapper Pipeline!"),
              p("This app will guide you through the process of identifying causitive mutations from bulk RNA-seq data."),
              p("Please use the menu on the left to navigate through different stages of the pipeline."),
              p("Each step will allow you to upload files, process data, and view results."),
              
              # Instructions now integrated into the Home page
              h4("Instructions"),
              p("Follow these steps to use the mutant mapping pipeline:"),
              tags$ol(
                tags$li("Step 1: Configure the pipeline and upload your input files."),
                tags$li("Step 2: Process the data"),
                tags$li("Step 3: View the processed."),
                tags$li("If you need help, consult the instructions here again.")
              ),
              p("Each step of the pipeline has file upload functionality and detailed instructions.")
      ),
      
      tabItem(tabName = "step_1",
              h3("Step 1: Upload Files"),
              p("In this step, upload your input files for the RNA mapping pipeline."),
              fileInput("wt_reads", "Upload wild type RNA-seq data (SAM or BAM format)", 
                        multiple = F,
                        accept=c("text/.sam","text/.bam",
                                 ".sam",".bam")),
              fileInput("mut_reads", "Upload mutant RNA-seq data (SAM or BAM format)", 
                        multiple = F,
                        accept=c("text/.vcf",
                                 ".vcf")),
              fileInput("SNPdb", "Upload VCF containing known mutations", 
                        multiple = F,
                        accept=c("text/.vcf",
                                 ".vcf")),
              textInput("MUTdb_url","...or provide a url download link"),
              actionButton("upload_button", "Upload Files"),
              uiOutput("upload_status")
      ),
      
      tabItem(tabName = "step_2",
              h3("Step 2: Data Processing"),
              p("After uploading your files, proceed with the processing of the data."),
              actionButton("process_button", "Run Data Processing"),
              uiOutput("process_status")
      ),
      
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
  
  # Reactive: Handle file upload
  observeEvent(input$upload_button, {
    if (is.null(input$input_genome)) {
      output$upload_status <- renderUI({
        p("No file selected. Please upload your input files.")
      })
    } else {
      output$upload_status <- renderUI({
        p("Files successfully uploaded!")
      })
    }
  })
  
  # Reactive: Handle data processing
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
