#' Genetic data type module ui
#' @keywords internal
#' @author Ghislain Durif
genetic_data_type_module_ui <- function(id, title = NULL, note = NULL) {
    ns <- NS(id)
    out <- tagList(
        verticalLayout(
            numericInput(ns("auto_dip"), label = "Autosomal diploid (A)", 
                         value = 0, min = 0),
            numericInput(ns("auto_hap"), label = "Autosomal diploid (H)", 
                         value = 0, min = 0),
            numericInput(ns("x_linked"), label = "X-linked (X)", 
                         value = 0, min = 0),
            numericInput(ns("y_linked"), label = "X-linked (Y)", 
                         value = 0, min = 0),
            numericInput(ns("mito"), label = "Mitochondrial (M)", 
                         value = 0, min = 0)
        )
    )
    if(!is.null(note))
        out <- c(tagList(helpText(note)), out)
    if(!is.null(title))
        out <- c(tagList(h4(title)), out)
    out
}

#' Genetic data type module server
#' @keywords internal
#' @author Ghislain Durif
genetic_data_type_module_server <- function(input, output, session) {
    genetic_spec <- reactiveValues()
    
    observe({
        genetic_spec$auto_dip <- input$auto_dip
        genetic_spec$auto_hap <- input$auto_hap
        genetic_spec$x_linked <- input$x_linked
        genetic_spec$y_linked <- input$y_linked
        genetic_spec$mito <- input$mito
    })
    
    return(genetic_spec)
}

#' Genetic data module ui
#' @keywords internal
#' @author Ghislain Durif
genetic_data_module_ui <- function(id) {
    ns <- NS(id)
    out <- tagList(
        uiOutput(ns("data_type")),
        tags$hr(),
        numericInput(ns("sex_ratio"), label = "Sex ratio (SR)", 
                     value = 0.5, min = 0, max = 1, step = 0.01),
        helpText(paste0("Proportion of male in the population ",
                        "(between 0 and 1)."))
    )
}

#' Genetic data module server
#' @keywords internal
#' @author Ghislain Durif
#' @param type `"snp"`, `"mss"` for MicroSAT/sequence or `"poolseq"`.
#' @import shiny
genetic_data_module <- function(input, output, session, data_info) {
    
    genetic_data <- reactiveValues()
    
    ns <- session$ns
    
    output$data_type <- renderUI({
        tmp <- switch(
            data_info$type,
            "snp" = genetic_data_type_module_ui(
                ns("snp_loci"), title = "Type of SNP loci", 
                note = "Note: H loci are not compatible with A, X and Y loci."
            ),
            "mss" = tagList(
                flowLayout(
                    genetic_data_type_module_ui(
                        ns("microsat_loci"), title = "Microsatellite loci", 
                        note = NULL
                    ),
                    genetic_data_type_module_ui(
                        ns("dna_loci"), title = "DNA loci", 
                        note = NULL
                    )
                )
            ),
            "poolseq" = genetic_data_type_module_ui(
                ns("poolseq_loci"), title = "Type of loci for PoolSeq", 
                note = NULL
            )
        )
    })
    
    # FIXME
    # callModule(genetic_data_type_module_server, "")
}
