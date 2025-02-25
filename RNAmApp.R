library(shiny)
library(shinydashboard)

# File Accept Vectors
accept_vectors <- list(
  'raw_reads' = c("text/plain","text/fastq", "text/fastq.gz",
                  "text/fasta", "text/fasta.gz",
                  "text/fa", "text/fa.gz",
                  "text/fq.gz", "text/fq",
                  ".fq", ".fq.gz", ".fastq", ".fastq.gz",
                  ".fasta", ".fasta.gz", ".fa", ".fa.gz", 
                  "application/x-gzip","application/x-zip"),
  'genomes' = c("text/plain","application/x-sam", 
                "application/x-bam", ".sam", ".bam",
                "application/x-gzip","application/x-zip"),
  'variants' = c("text/plain","application/x-vcf", ".vcf", 
                 "application/x-vcf.gz", ".vcf.gz",
                 "application/x-gzip","application/x-zip"),
  'genemodels' = c("text/plain","application/x-gzip", ".gtf", ".gtf.gz",
                   "application/x-gzip","application/x-zip"),
  'folder' = c('application/x-gzip','application/x-zip', 
               'application/x-tar')
)

# Unified reactive values to store inputs
unified_data <- reactiveValues(
  wt_reads = NULL,
  mut_reads = NULL,
  MUTdb = NULL,
  MUTdb_url = NULL,
  coverage = NULL,
  zygosity = NULL,
  chr_list = NULL,
  run_flag = NULL,
  wdir = NULL
)

# Load modules
source("modules/home_module.R")
source("modules/step1_module.R")
source("modules/step2_module.R")
source("modules/step3_module.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$img(src = "miller_lab_logo_noback.png", height = "50px", style = "margin-right: 10px;"),
      "RNA Mapper"
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    tags$head(tags$style(HTML("
      .skin-blue .main-sidebar { background-color: #3a3a3a; }
      .skin-blue .main-sidebar .sidebar-menu > li > a { color: #FFFFFF; }
      .skin-blue .main-sidebar .sidebar-menu > li.active > a {
        background-color: #FCE020;
        color: #3a3a3a;
      }
      .skin-blue .main-header .navbar { background-color: #3a3a3a; }
      .skin-blue .main-header .navbar:hover { background-color: #1e282c; }
      .skin-blue .main-header .logo { background-color: #3a3a3a; color: #FCE020; }
      .skin-blue .main-header .logo:hover { background-color: #1e282c; }
      .content-wrapper, .right-side { background-color: #FFFFFF; }
    "))),
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Step 1: Upload Files", tabName = "step_1", icon = icon("upload")),
      menuItem("Step 2: Map Variants", tabName = "step_2", icon = icon("cogs")),
      menuItem("Step 3: View Candidate Mutations", tabName = "step_3", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      homeModuleUI("home"),
      step1ModuleUI("step1"),
      step2ModuleUI("step2"),
      step3ModuleUI("step3")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Set upload limit to 100 Gb
  options(shiny.maxRequestSize = 10^5 * 1024^2)
  
  observeEvent({unified_data$wdir == ''},{
    unified_data$wdir = file.path(getwd(),"working_dir")
  })
  
  # Call modules
  callModule(homeModule, "home")
  callModule(step1Module, "step1", unified_data)
  callModule(step2Module, "step2", unified_data)
  callModule(step3Module, "step3", unified_data)
}

# Run the application
shinyApp(ui = ui, server = server)
