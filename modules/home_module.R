homeModuleUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "home",
    tags$img(src = "MillerLogo1.jpg", height = "150px", style = "display: block; margin: auto;"),
    h3("Welcome to the RNA Mapper Pipeline!"),
    p("This app will guide you through the process of identifying causative mutations from bulk RNA-seq data."),
    p("Please use the menu on the left to navigate through different stages of the pipeline."),
    
    h4("Instructions"),
    p("Follow these steps to use the mutant mapping pipeline:"),
    tags$ol(
      tags$li("Step 1: Configure the pipeline and upload your input files."),
      tags$li("Step 2: View mutant markers."),
      tags$li("Step 3: View candidate mutations."),
      tags$li("If you need help, consult the README on GitHub.")
    )
  )
}

homeModule <- function(input, output, session) {}
