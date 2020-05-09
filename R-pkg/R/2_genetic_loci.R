#' Sequencing locus number module ui
#' @keywords internal
#' @author Ghislain Durif
locus_nb_ui <- function(id, title = NULL) {
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
        )
    )
}

#' Sequencing locus number module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyjs disable enable
locus_nb_server <- function(input, output, session, 
                            haploid = reactive({FALSE}),
                            poolseq = reactive({FALSE})) {
    # init local
    local <- reactiveValues(haploid = NULL, poolseq = NULL)
    # get input
    observe({
        local$haploid <- haploid()
        local$poolseq <- poolseq()
    })
    # init output reactive values
    out <- reactiveValues(
        auto_dip = NULL, auto_hap = NULL,
        x_linked = NULL, y_linked = NULL,
        mito = NULL, haploid = NULL
    )
    # enable/disable input
    observe({
        if(local$haploid & !local$poolseq) {
            shinyjs::enable("auto_hap")
            shinyjs::enable("mito")
            shinyjs::disable("auto_dip")
            shinyjs::disable("x_linked")
            shinyjs::disable("y_linked")
        } else if(!local$haploid & local$poolseq) {
            shinyjs::enable("auto_hap")
            shinyjs::disable("auto_dip")
            shinyjs::disable("x_linked")
            shinyjs::disable("y_linked")
            shinyjs::disable("mito")
        } else {
            shinyjs::enable("auto_hap")
            shinyjs::enable("auto_dip")
            shinyjs::enable("x_linked")
            shinyjs::enable("y_linked")
            shinyjs::enable("mito")
        }
    })
    # get input
    observe({
        out$auto_dip <- input$auto_dip
        out$auto_hap <- input$auto_hap
        out$x_linked <- input$x_linked
        out$y_linked <- input$y_linked
        out$mito <- input$mito
    })
    # update haploid
    observeEvent(input$auto_hap, {
        req(input$auto_hap)
        if(input$auto_hap > 0) {
            out$haploid <- TRUE
        } else {
            out$haploid <- FALSE
        }
    })
    # output
    return(out)
}

#' Genetic loci module ui
#' @keywords internal
#' @author Ghislain Durif
genetic_loci_ui <- function(id) {
    ns <- NS(id)
    tagList(
        verticalLayout(
            h3("Locus type"),
            splitLayout(
                radioButtons(
                    ns("locus_type"), 
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
            uiOutput(ns("locus_nb_ui")),
            helpText(
                icon("warning"),
                "Note: H loci are not compatible with A, X and Y loci."
            ),
            tags$hr(),
            numericInput(ns("sex_ratio"), label = "Sex ratio (SR)", 
                         value = 0.5, min = 0, max = 1, step = 0.01),
            helpText(
                paste0("Proportion of male in the population ",
                       "(between 0 and 1).")
            )
        )
    )
}

#' Genetic loci module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyjs disable enable
genetic_loci_server <- function(input, output, session) {
    # module namespace
    ns <- session$ns
    # init local reactive values
    local <- reactiveValues(haploid = FALSE,
                            locus_nb_ui = NULL,
                            locus_type = NULL,
                            poolseq = FALSE,
                            seq_mode = NULL)
    # init output values
    out <- reactiveValues(dna_loci = NULL, 
                          microsat_loci = NULL, 
                          snp_loci = NULL,
                          sex_ratio = NULL)
    # disable seq mode if relevant
    observeEvent(input$locus_type, {
        req(input$locus_type)
        # enable/disable
        if(input$locus_type != "snp") {
            updateRadioButtons(session, "seq_mode", selected = "indseq")
            shinyjs::disable("seq_mode")
        } else {
            shinyjs::enable("seq_mode")
        }
    })
    # react
    observeEvent(input$locus_type, {
        req(input$locus_type)
        local$locus_type <- input$locus_type
    })
    observeEvent(input$seq_mode, {
        req(input$seq_mode)
        local$seq_mode <- input$seq_mode
        local$poolseq <- (input$seq_mode == "poolseq")
    })
    # debugging
    observe({
        logging("locus type:", local$locus_type)
        logging("seq mode:", local$seq_mode)
        logging("poolseq:", local$poolseq)
        logging("haploid:", local$haploid)
    })
    # locus number ui
    observe({
        req(local$locus_type)
        if(local$locus_type == "mss") {
            local$locus_nb_ui <- flowLayout(
                locus_nb_ui(
                    ns("microsat_loci"), title = "Microsatellite loci"
                ),
                locus_nb_ui(
                    ns("dna_loci"), title = "DNA loci"
                )
            )
        } else if(local$locus_type == "snp") {
            local$locus_nb_ui <- flowLayout(
                locus_nb_ui(
                    ns("snp_loci"), title = "SNP loci"
                )
            )
        }
    })
    # rendering
    output$locus_nb_ui <- renderUI({
        local$locus_nb_ui
    })
    # locus number server function
    dna_loci <- callModule(locus_nb_server, "dna_loci", 
                           haploid = reactive(local$haploid),
                           poolseq = reactive(local$poolseq))
    microsat_loci <- callModule(locus_nb_server, "microsat_loci", 
                                haploid = reactive(local$haploid),
                                poolseq = reactive(local$poolseq))
    snp_loci <- callModule(locus_nb_server, "snp_loci", 
                           haploid = reactive(local$haploid),
                           poolseq = reactive(local$poolseq))
    # get output
    observe({
        out$dna_loci <- dna_loci
        out$microsat_loci <- microsat_loci
        out$snp_loci <- snp_loci
    })
    # update haploid status
    observe({
        local$haploid <- any(c(dna_loci$haploid, 
                               microsat_loci$haploid,
                               snp_loci$haploid))
    })
    # get sex ratio
    observe({
        out$sex_ratio <- input$sex_ratio
    })
    # output
    return(out)
}
