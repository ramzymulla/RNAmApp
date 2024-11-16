library(shiny)
library(shinydashboard)

raw_read_accepts = c("text/.fastq", "text/.fastq.gz",
                     "text/.fasta", "text/fasta.gz",
                     "text/.fa", "text/fa.gz",
                      "text/.fq.gz","text/.fq", 
                      ".fq", ".fq.gz", ".fastq", "fastq.gz",
                     ".fasta",".fasta.gz","fa","fa.gz")

genome_accepts = c("text/.sam", "text/.bam", ".sam", ".bam")

variant_accepts = c("text/.vcf", ".vcf", "text/.vcf.gz",".vcf.gz")

# Define UI using shinydashboard
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$img(src = "uo_logo_funny.png", height = "40px", style = "margin-right: 10px;"),
      "RNA Mapper"
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    tags$head(tags$style(HTML("
      .skin-blue .main-sidebar {
        background-color: #154733;
      }
      .skin-blue .main-sidebar .sidebar-menu > li > a {
        color: #FFFFFF;
      }
      .skin-blue .main-sidebar .sidebar-menu > li.active > a {
        background-color: #FCE020;
        color: #154733;
      }
      .skin-blue .main-header .navbar {
        background-color: #154733;
      }
      .skin-blue .main-header .logo {
        background-color: #154733;
        color: #FCE020;
        font-size: 18px;
        font-weight: bold;
      }
      .skin-blue .main-header .logo:hover {
        background-color: #FCE020;
        color: #154733;
      }
      .content-wrapper, .right-side {
        background-color: #FFFFFF;
      }
    "))),
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
              tags$img(src = "MillerLogo1.jpg", height = "150px", style = "display: block; margin: auto;"),
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
                tabPanel("Raw Reads",
                         h4("File Upload for Raw Reads"),
                         fileInput("wt_reads_1", "Upload wild type RNA-seq data (FASTA or FASTQ format)", 
                                   multiple = FALSE,
                                   accept = raw_read_accepts),
                         fileInput("mut_reads_1", "Upload mutant RNA-seq data (FASTA or FASTQ format)", 
                                   multiple = FALSE,
                                   accept = raw_read_accepts),
                         fileInput("SNPdb_1", "Upload VCF containing known mutations", 
                                   multiple = FALSE,
                                   accept = variant_accepts),
                         textInput("MUTdb_url_1", "...or provide a URL download link"),
                         actionButton("upload_button_1", "Upload Files for Configuration 1"),
                         uiOutput("upload_status")
                ),
                tabPanel("Aligned Reads",
                         h4("File Upload for Aligned Reads (must be aligned using HISAT2!!"),
                         fileInput("wt_reads_2", "Upload wild type RNA-seq data (SAM or BAM format)", 
                                   multiple = FALSE,
                                   accept = genome_accepts),
                         fileInput("mut_reads_2", "Upload mutant RNA-seq data (SAM or BAM format)", 
                                   multiple = FALSE,
                                   accept = genome_accepts),
                         fileInput("SNPdb_2", "Upload VCF containing known mutations", 
                                   multiple = FALSE,
                                   accept = variant_accepts),
                         textInput("MUTdb_url_2", "...or provide a URL download link"),
                         actionButton("upload_button_2", "Upload Files for Configuration 2"),
                         uiOutput("upload_status")
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
  # Unified reactive values to store inputs
  unified_inputs <- reactiveValues(
    wt_reads = NULL,
    mut_reads = NULL,
    SNPdb = NULL,
    MUTdb_url = NULL
  )
  
  # Handle file uploads for Configuration 1
  observeEvent(input$upload_button_1, {
    unified_inputs$wt_reads <- input$wt_reads_1
    unified_inputs$mut_reads <- input$mut_reads_1
    unified_inputs$SNPdb <- NULL  # Not used in this configuration
    unified_inputs$MUTdb_url <- NULL  # Not used in this configuration
    
    output$upload_status <- renderUI({
      p("Files successfully uploaded for Configuration 1!")
    })
  })
  
  # Handle file uploads for Configuration 2
  observeEvent(input$upload_button_2, {
    unified_inputs$wt_reads <- NULL  # Not used in this configuration
    unified_inputs$mut_reads <- NULL  # Not used in this configuration
    unified_inputs$SNPdb <- input$SNPdb_2
    unified_inputs$MUTdb_url <- input$MUTdb_url_2
    
    output$upload_status <- renderUI({
      p("Files successfully uploaded for Configuration 2!")
    })
  })
  
  # Handle data processing
  observeEvent(input$process_button, {
    output$process_status <- renderUI({
      p("Processing data...")
    })
    
    # Example: Access unified inputs for processing
    isolate({
      print(unified_inputs$wt_reads)
      print(unified_inputs$mut_reads)
      print(unified_inputs$SNPdb)
      print(unified_inputs$MUTdb_url)
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
