library(shiny)
library(shinydashboard)

# Universal accept vectors
raw_read_accepts <- c("text/.fastq", "text/.fastq.gz",
                      "text/.fasta", "text/.fasta.gz",
                      "text/.fa", "text/.fa.gz",
                      "text/.fq.gz", "text/.fq",
                      ".fq", ".fq.gz", ".fastq", ".fastq.gz",
                      ".fasta", ".fasta.gz", "fa", "fa.gz")

genome_accepts <- c("text/.sam", "text/.bam", ".sam", ".bam")

variant_accepts <- c("text/.vcf", ".vcf", "text/.vcf.gz", ".vcf.gz")

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
  # Call modules
  callModule(homeModule, "home")
  callModule(step1Module, "step1")
  callModule(step2Module, "step2")
  callModule(step3Module, "step3")
}

# Run the application
shinyApp(ui = ui, server = server)
