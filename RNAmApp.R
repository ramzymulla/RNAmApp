library(shiny)
library(shinydashboard)

# Define UI using shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "RNA Mapper"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Stage 1: File Upload", tabName = "stage_1", icon = icon("upload")),
      menuItem("Stage 2: Data Processing", tabName = "stage_2", icon = icon("cogs")),
      menuItem("Stage 3: Results", tabName = "stage_3", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Home Page with integrated instructions
      tabItem(tabName = "home", 
              h3("Welcome to the RNA Mapper Pipeline!"),
              p("This app will guide you through the process of identifying causitive mutations from bulk RNA-seq data."),
              p("Please use the menu on the left to navigate through different stages of the pipeline."),
              p("Each stage will allow you to upload files, process data, and view results."),
              
              # Instructions now integrated into the Home page
              h4("Instructions"),
              p("Follow these steps to use the mutant mapping pipeline:"),
              tags$ol(
                tags$li("Step 1: Upload your input files in the 'Stage 1' tab."),
                tags$li("Step 2: Process the data in the 'Stage 2' tab."),
                tags$li("Step 3: View the processed results in the 'Stage 3' tab."),
                tags$li("If you need help, consult the instructions here again.")
              ),
              p("Each stage of the pipeline has file upload functionality and detailed instructions.")
      ),
      
      tabItem(tabName = "stage_1",
              h3("Stage 1: File Upload"),
              p("In this stage, upload your input files for the mutant mapping pipeline."),
              fileInput("input_genome", "Choose input file", 
                        multiple = F,
                        accept=c("text/.sam","text/.bam",
                                 ".sam",".bam")),
              fileInput("MUTdb", "Upload VCF containing known mutations", 
                        multiple = F,
                        accept=c("text/.vcf",
                                 ".vcf")),
              textInput("MUTdb_url","...or url download link"),
              actionButton("upload_button", "Upload Files"),
              uiOutput("upload_status")
      ),
      
      tabItem(tabName = "stage_2",
              h3("Stage 2: Data Processing"),
              p("After uploading your files, proceed with the processing of the data."),
              actionButton("process_button", "Run Data Processing"),
              uiOutput("process_status")
      ),
      
      tabItem(tabName = "stage_3",
              h3("Stage 3: Results"),
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
    if (is.null(input$input_file)) {
      output$upload_status <- renderUI({
        p("No file selected. Please upload your input files.")
      })
    } else {
      output$upload_status <- renderUI({
        p("Files uploaded successfully!")
      })
    }
  })
  
  # Reactive: Handle data processing
  observeEvent(input$process_button, {
    output$process_status <- renderUI({
      p("Data processing started...")
    })
    
    # Placeholder processing
    Sys.sleep(2)  # Simulate processing
    output$process_status <- renderUI({
      p("Data processing complete!")
    })
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
