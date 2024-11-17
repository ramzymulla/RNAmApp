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
  run_flag = NULL
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
      tags$img(src = "uo_logo_funny.png", height = "40px", style = "margin-right: 10px;"),
      "RNA Mapper"
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    tags$head(tags$style(HTML("
      .skin-blue .main-sidebar { background-color: #154733; }
      .skin-blue .main-sidebar .sidebar-menu > li > a { color: #FFFFFF; }
      .skin-blue .main-sidebar .sidebar-menu > li.active > a {
        background-color: #FCE020;
        color: #154733;
      }
      .skin-blue .main-header .navbar { background-color: #154733; }
      .skin-blue .main-header .logo { background-color: #154733; color: #FCE020; }
      .content-wrapper, .right-side { background-color: #FFFFFF; }
    "))),
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Step 1: Upload Data", tabName = "step_1", icon = icon("upload")),
      menuItem("Step 2: Map Variants", tabName = "step_2", icon = icon("cogs")),
      menuItem("Step 3: View Candidate Mutations", tabName = "step_3", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      homeModuleUI("home"),
      step1ModuleUI("step1",accept_vectors),
      step2ModuleUI("step2"),
      step3ModuleUI("step3")
    )
  )
)

# Define server
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 10^5 * 1024^2)
  
  # Call modules
  callModule(homeModule, "home")
  callModule(step1Module, "step1", unified_data)
  callModule(step2Module, "step2")
  callModule(step3Module, "step3")
}

# Run the application
shinyApp(ui = ui, server = server)
