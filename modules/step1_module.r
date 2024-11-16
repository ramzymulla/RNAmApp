# Universal accept vectors
raw_read_accepts <- c("text/.fastq", "text/.fastq.gz",
                      "text/.fasta", "text/.fasta.gz",
                      "text/.fa", "text/.fa.gz",
                      "text/.fq.gz", "text/.fq",
                      ".fq", ".fq.gz", ".fastq", ".fastq.gz",
                      ".fasta", ".fasta.gz", "fa", "fa.gz")

genome_accepts <- c("text/.sam", "text/.bam", ".sam", ".bam")

variant_accepts <- c("text/.vcf", ".vcf", "text/.vcf.gz", ".vcf.gz")


step1ModuleUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "step_1",
    h3("Step 1: File Upload"),
    p("In this step, upload your input files for the RNA mapping pipeline."),
    
    tabsetPanel(
      id = ns("file_upload_tabs"),
      tabPanel("Raw Reads",
               h4("File Upload for Raw Reads"),
               fileInput(ns("wt_reads_1"), "Upload wild type RNA-seq data (FASTA or FASTQ format)",
                         multiple = FALSE, accept = raw_read_accepts),
               fileInput(ns("mut_reads_1"), "Upload mutant RNA-seq data (FASTA or FASTQ format)",
                         multiple = FALSE, accept = raw_read_accepts),
               fileInput(ns("SNPdb_1"), "Upload VCF containing known mutations",
                         multiple = FALSE, accept = variant_accepts),
               textInput(ns("MUTdb_url_1"), "...or provide a URL download link"),
               actionButton(ns("upload_button_1"), "Upload Files for Configuration 1"),
               uiOutput(ns("upload_status"))
      ),
      tabPanel("Aligned Reads",
               h4("File Upload for Aligned Reads"),
               fileInput(ns("wt_reads_2"), "Upload wild type RNA-seq data (SAM or BAM format)",
                         multiple = FALSE, accept = genome_accepts),
               fileInput(ns("mut_reads_2"), "Upload mutant RNA-seq data (SAM or BAM format)",
                         multiple = FALSE, accept = genome_accepts),
               fileInput(ns("SNPdb_2"), "Upload VCF containing known mutations",
                         multiple = FALSE, accept = variant_accepts),
               textInput(ns("MUTdb_url_2"), "...or provide a URL download link"),
               actionButton(ns("upload_button_2"), "Upload Files for Configuration 2"),
               uiOutput(ns("upload_status"))
      )
    )
  )
}

step1Module <- function(input, output, session) {
  observeEvent(input$upload_button_1, {
    output$upload_status <- renderUI({
      p("Files successfully uploaded for Configuration 1!")
    })
  })
  
  observeEvent(input$upload_button_2, {
    output$upload_status <- renderUI({
      p("Files successfully uploaded for Configuration 2!")
    })
  })
}
