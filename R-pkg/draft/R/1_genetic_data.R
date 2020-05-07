#' Individual sequencing loci type module ui
#' @keywords internal
#' @author Ghislain Durif
indseq_loci_type_module_ui <- function(id, title = NULL, note = NULL) {
    ns <- NS(id)
    tagList(
        conditionalPanel(
            !is.null(title), 
            tags$h4(title)
        ),
        verticalLayout(
            numericInput(ns("auto_dip"), label = "Autosomal diploid (A)", 
                         value = 0, min = 0),
            numericInput(ns("auto_hap"), label = "Autosomal haploid (H)", 
                         value = 0, min = 0),
            numericInput(ns("x_linked"), label = "X-linked (X)", 
                         value = 0, min = 0),
            numericInput(ns("y_linked"), label = "X-linked (Y)", 
                         value = 0, min = 0),
            numericInput(ns("mito"), label = "Mitochondrial (M)", 
                         value = 0, min = 0)
        ),
        conditionalPanel(
            !is.null(note), 
            helpText(note)
        )
    )
}

#' Individual sequencing type module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyjs disable enable
indseq_loci_type_module_server <- function(input, output, session) {
    # init output reactive values
    out <- reactiveValues()
    # get input
    observe({
        out$auto_dip <- input$auto_dip
        out$auto_hap <- input$auto_hap
        out$x_linked <- input$x_linked
        out$y_linked <- input$y_linked
        out$mito <- input$mito
    })
    # output
    return(out)
}

#' Pool sequencing loci type module ui
#' @keywords internal
#' @author Ghislain Durif
poolseq_loci_type_module_ui <- function(id, title = NULL, note = NULL) {
    ns <- NS(id)
    tagList(
        conditionalPanel(
            !is.null(title), 
            tags$h4(title)
        ),
        verticalLayout(
            numericInput(ns("auto_dip"), label = "Autosomal diploid (A)", 
                         value = 0, min = 0)
        ),
        conditionalPanel(
            !is.null(note), 
            helpText(note)
        )
    )
}

#' Pool sequencing type module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyjs disable enable
poolseq_loci_type_module_server <- function(input, output, session) {
    # init output reactive values
    out <- reactiveValues()
    # get input
    observe({
        out$auto_dip <- input$auto_dip
        out$auto_hap <- NULL
        out$x_linked <- NULL
        out$y_linked <- NULL
        out$mito <- NULL
    })
    # output
    return(out)
}

#' Genetic data module ui
#' @keywords internal
#' @author Ghislain Durif
genetic_data_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        verticalLayout(
            h3("Data type"),
            splitLayout(
                radioButtons(
                    ns("data_type"), 
                    label = "Locus type",
                    choices = list("MicroSat/Sequence" = "mss", "SNP" = "snp"),
                    selected = "snp"
                ),
                radioButtons(
                    ns("seq_mode"), 
                    label = "Sequencing mode",
                    choices = list("Individual Seq." = "indseq", 
                                   "PoolSeq" = "poolseq"),
                    selected = "indseq"
                )
            ),
            tags$hr(),
            uiOutput(ns("data_pos")),
            tags$hr(),
            numericInput(ns("sex_ratio"), label = "Sex ratio (SR)", 
                         value = 0.5, min = 0, max = 1, step = 0.01),
            helpText(paste0("Proportion of male in the population ",
                            "(between 0 and 1)."))
        )
    )
}

#' Genetic data module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyjs disable enable
genetic_data_module_server <- function(input, output, session) {
    # init local reactive values
    local <- reactiveValues(loci_mode = NULL)
    # init output values
    out <- reactiveValues(loci_spec = NULL)
    # module namespace
    ns <- session$ns
    # disable seq mode if relevant
    observe({
        req(input$data_type)
        # enable/disable
        if(input$data_type != "snp") {
            updateRadioButtons(session, "seq_mode", selected = "indseq")
            shinyjs::disable("seq_mode")
        } else {
            shinyjs::enable("seq_mode")
        }
    })
    # render data pos
    observe({
        req(input$data_type)
        # ui output
        if(input$data_type == "mss") {
            local$loci_mode <- flowLayout(
                indseq_loci_type_module_ui(
                    ns("microsat_loci"), title = "Microsatellite loci",
                    note = NULL
                ),
                indseq_loci_type_module_ui(
                    ns("dna_loci"), title = "DNA loci",
                    note = NULL
                )
            )
        } else if(input$data_type == "snp") {
            req(input$seq_mode)
            if(input$seq_mode == "indseq") {
                local$loci_mode <- flowLayout(
                    indseq_loci_type_module_ui(
                        ns("ind_snp_loci"), title = "Type of SNP loci", 
                        note = "Note: H loci are not compatible with A, X and Y loci."
                    )
                )
            } else if(input$seq_mode == "poolseq") {
                local$loci_mode <- flowLayout(
                    poolseq_loci_type_module_ui(
                        ns("pool_snp_loci"), title = "Type of SNP loci", 
                        note = NULL
                    )
                )
            }
        }
        # rendering
        output$data_pos <- renderUI({
            local$loci_mode
        })
        # call server function accordingly
        if(input$data_type == "mss") {
            tmp1 <- callModule(indseq_loci_type_module_server, "microsat_loci")
            tmp2 <- callModule(indseq_loci_type_module_server, "dna_loci")
            out$loci_spec <- c(tmp1, tmp2)
        } else if(input$data_type == "snp") {
            req(input$seq_mode)
            if(input$seq_mode == "indseq") {
                out$loci_spec <- callModule(indseq_loci_type_module_server, 
                                            "ind_snp_loci")
            } else if(input$seq_mode == "poolseq") {
                out$loci_spec <- callModule(poolseq_loci_type_module_server, 
                                            "pool_snp_loci")
            }
        }
    })
    # get sex ratio
    observe({
        out$sex_ratio <- input$sex_ratio
    })
    # output
    return(out)
}
