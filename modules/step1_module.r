step1ModuleUI <- function(id,accept_vectors) {
  ns <- NS(id)
  tabItem(
    tabName = "step_1",
    h3("Step 1: Upload Data"),
    p("In this step, upload your input files for the RNA mapping pipeline."),
    fluidRow(
      # Files Panel
      box(
        tabsetPanel(
          id = ns("file_upload_tabs"),
          tabPanel("Aligned Reads",
                   h4("File Upload for Aligned Reads"),
                   h5("Use this tab if your RNA-seq data has already been aligned (must be aligned using HISAT2!!)."),
                   fileInput(ns("wt_reads_aligned"), "Upload wild type RNA-seq data (SAM or BAM format)",
                             multiple = FALSE, accept = accept_vectors$genomes),
                   fileInput(ns("mut_reads_aligned"), "Upload mutant RNA-seq data (SAM or BAM format)",
                             multiple = FALSE, accept = accept_vectors$genomes),
                   fileInput(ns("MUTdb_aligned"), "Upload list of known wild-type mutations (VCF)",
                             multiple = FALSE, accept = accept_vectors$variants),
                   textInput(ns("MUTdb_url_aligned"), "...or provide a URL download link"),
                   actionButton(ns("upload_button_aligned"), "Upload Files"),
                   uiOutput(ns("upload_status"))
            ),
          tabPanel("Raw Reads",
                   h4("File Upload for Raw Reads"),
                   h5("Use this tab if your RNA-seq data has not yet been aligned to the species genome."),
                   fileInput(ns("wt_reads_raw"), "Upload wild type RNA-seq data (FASTA or FASTQ format)",
                             multiple = FALSE, accept = accept_vectors$raw_reads),
                   fileInput(ns("mut_reads_raw"), "Upload mutant RNA-seq data (FASTA or FASTQ format)",
                             multiple = FALSE, accept = accept_vectors$genomes),
                   fileInput(ns("reference_raw"), "Upload your reference genome (FASTA or FASTQ format)",
                             multiple = FALSE, accept = accept_vectors$raw_reads),
                   fileInput(ns("genemodels_raw"), "Upload your gene models (GTF)",
                             multiple = FALSE, accept = accept_vectors$genemodels),
                   fileInput(ns("MUTdb_raw"), "Upload list of known wild-type mutations (VCF)",
                             multiple = FALSE, accept = accept_vectors$variants),
                   textInput(ns("MUTdb_url_raw"), "...or provide a URL download link"),
                   actionButton(ns("upload_button_raw"), "Upload Files"),
                   uiOutput(ns("upload_status"))
            )
          )
        ),
      
      # Config Panel
      box(
        p("Please indicate the zygosity and coverage of your data."),
        textInput(ns("zygosity"), "Zygosity"),
        textInput(ns("coverage"), "Coverage"),
        actionButton(ns("config_button"), "Configure Pipeline"),
        uiOutput(ns("config_status")),
        tags$br(),
        actionButton(ns("run_button"),"Start Mapping"),
        uiOutput(ns("run_status"))
      ),
    )
  )
}

step1Module <- function(input, output, session, unified_data) {
  
  observeEvent(input$config_button, {
    unified_data$zygosity <- input$zygosity
    unified_data$coverage <- input$coverage
    
    # Write settings to mappeRsettings.txt
    mappersettingsfile <- file.path(unified_data$wdir,"_settings", "mappeRsettings.txt")
    write(paste("coverage=",input$coverage,sep=''), file = mappersettingsfile)
    write(paste("zygosity=",input$zygosity,sep=''), file = mappersettingsfile,
          append=TRUE)
    output$config_status <- renderUI({
      p("Configuration has been updated")
    })
  })

  # Handle file uploads for raw reads
  observeEvent(input$upload_button_1, {
    unified_data$wt_reads <- input$wt_reads_1
    unified_data$mut_reads <- input$mut_reads_1
    unified_data$MUTdb <- input$MUTdb_1
    unified_data$MUTdb_url <- input$MUTdb_url_1
    unified_data$run_flag <- "rawreads"

    output$upload_status <- renderUI({
      p("Pipeline has been successfully initialized!")
    })
  })

  # Handle file uploads for aligned data
  observeEvent(input$upload_button_2, {
    unified_data$wt_reads <- input$wt_reads_2
    unified_data$mut_reads <- input$mut_reads_2
    unified_data$MUTdb <- input$MUTdb_2
    unified_data$MUTdb_url <- input$MUTdb_url_2
    unified_data$run_flag <- "alignedreads"

    output$upload_status <- renderUI({
      p("Pipeline has been successfully initialized!")
    })
  })
  
  # Handle pipeline
  observeEvent(input$run_button, {
    output$run_status <- renderUI({
      p("Let the mapping commence!")
    })
  })
  
  observeEvent({unified_data$run_flag == "alignedreads"}, {
    setwd(unified_data$wdir)

    # # Split aligned reads into separate chromosomes
    # system("split_chroms.sh")
    
    # Get list of chromosomes using bash command
    unified_data$chr_list <-
      system(paste('ls', 
                  'grep -v "/"',
                  'grep -E "mut.*\\.vcf"',
                  'grep -E -v "mut_.*_"',
                  'sed -E "s/.*mut_(.*)\\.vcf/\\1/"', 
                  'sort -n -k1.4', sep = ' | '), 
             intern=T)
  })
}
