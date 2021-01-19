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
            numericInput(ns("y_linked"), label = "Y-linked (Y)", 
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
    local <- reactiveValues(haploid = NULL, poolseq = NULL, 
                            auto_dip = NULL, auto_hap = NULL,
                            x_linked = NULL, y_linked = NULL,
                            mito = NULL)
    # get input
    observe({
        local$haploid <- haploid()
        local$poolseq <- poolseq()
    })
    # init output reactive values
    out <- reactiveValues(
        locus_count = NULL,
        total_count = NULL,
        haploid = FALSE
    )
    # enable/disable input
    observe({
        if(local$haploid & !local$poolseq) {
            shinyjs::enable("auto_hap")
            shinyjs::enable("mito")
            shinyjs::disable("auto_dip")
            shinyjs::disable("x_linked")
            shinyjs::disable("y_linked")
            updateNumericInput(session, "auto_dip", value = 0)
            updateNumericInput(session, "x_linked", value = 0)
            updateNumericInput(session, "y_linked", value = 0)
        } else if(!local$haploid & local$poolseq) {
            shinyjs::enable("auto_dip")
            shinyjs::disable("auto_hap")
            shinyjs::disable("x_linked")
            shinyjs::disable("y_linked")
            shinyjs::disable("mito")
            updateNumericInput(session, "auto_hap", value = 0)
            updateNumericInput(session, "x_linked", value = 0)
            updateNumericInput(session, "y_linked", value = 0)
            updateNumericInput(session, "mito", value = 0)
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
        # pprint(input$auto_dip)
        # pprint(input$auto_hap)
        # pprint(input$x_linked)
        # pprint(input$y_linked)
        # pprint(input$mito)
        local$auto_dip <- input$auto_dip
        local$auto_hap <- input$auto_hap
        local$x_linked <- input$x_linked
        local$y_linked <- input$y_linked
        local$mito <- input$mito
    })
    # update haploid
    observeEvent(input$auto_hap, {
        req(!is.null(input$auto_hap))
        if(input$auto_hap > 0) {
            local$haploid <- TRUE
        } else {
            local$haploid <- FALSE
        }
    })
    # update output
    observeEvent(local$haploid, {
        out$haploid <- local$haploid
    })
    # parse input
    observe({
        req(!is.null(local$auto_dip))
        req(!is.null(local$auto_hap))
        req(!is.null(local$x_linked))
        req(!is.null(local$y_linked))
        req(!is.null(local$mito))
        # pprint(local$auto_dip)
        # pprint(local$auto_hap)
        # pprint(local$x_linked)
        # pprint(local$y_linked)
        # pprint(local$mito)
        out$locus_count <- parse_locus_count(
            local$auto_dip, local$auto_hap,
            local$x_linked, local$y_linked,
            local$mito)
        out$total_count <- sum(c(local$auto_dip, local$auto_hap,
                                 local$x_linked, local$y_linked,
                                 local$mito))
        # pprint("-- locus count =")
        # pprint(out$locus_count)
    })
    # output
    return(out)
}

#' Data type module ui
#' @keywords internal
#' @author Ghislain Durif
data_type_ui <- function(id) {
    ns <- NS(id)
    tagList(
        splitLayout(
            radioButtons(
                ns("locus_type"), 
                label = "Locus type",
                choices = list("MicroSat/Sequence" = "mss", "SNP" = "snp"),
                selected = "snp"
            ),
            conditionalPanel(
                condition = "input.locus_type == 'snp'",
                ns = ns,
                radioButtons(
                    ns("seq_mode"), 
                    label = "Sequencing mode",
                    choices = list("Individual Seq." = "indseq", 
                                   "PoolSeq" = "poolseq"),
                    selected = "indseq"
                )
            )
        )
    )
}

#' Data type module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyjs disable enable
data_type_server <- function(input, output, session) {
    # init output
    out <- reactiveValues(
        locus_type = NULL,
        seq_mode = NULL
    )
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
        out$locus_type <- input$locus_type
    })
    observeEvent(input$seq_mode, {
        req(input$seq_mode)
        out$seq_mode <- input$seq_mode
    })
    # # debugging
    # observe({
    #     logging("locus type:", out$locus_type)
    #     logging("seq mode:", out$seq_mode)
    # })
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
    out <- reactiveValues(locus_description = NULL,
                          locus_type = NULL,
                          seq_mode = NULL,
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
    # # debugging
    # observe({
    #     logging("locus type:", local$locus_type)
    #     logging("seq mode:", local$seq_mode)
    #     logging("poolseq:", local$poolseq)
    #     logging("haploid:", local$haploid)
    # })
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
    # update haploid status
    observe({
        # pprint(dna_loci)
        # pprint(microsat_loci)
        # pprint(snp_loci)
        local$haploid <- any(c(dna_loci$haploid, 
                               microsat_loci$haploid,
                               snp_loci$haploid))
    })
    # parse numerical input and format output
    observe({
        req(!is.null(local$locus_type))
        # req(!is.null(dna_loci$locus_count))
        # req(!is.null(microsat_loci$locus_count))
        # req(!is.null(snp_loci$locus_count))
        locus_count <- switch(local$locus_type,
                              "mss" = c(dna_loci$locus_count,
                                        microsat_loci$locus_count),
                              "snp" = snp_loci$locus_count)
        out$locus_description <- parse_locus_description(locus_count,
                                                         local$locus_type)
        # pprint(out$locus_description)
    })
    # get seq mode and locus type
    observe({
        out$locus_type <- local$locus_type
        out$seq_mode <- local$seq_mode
    })
    # get sex ratio
    observe({
        out$sex_ratio <- input$sex_ratio
    })
    # output
    return(out)
}


#' Parse locus count
#' @keywords internal
#' @author Ghislain Durif
parse_locus_count <- function(auto_dip, auto_hap, 
                              x_linked, y_linked, 
                              mito) {
    out <- NULL
    ## numerical input
    locus_list <- list(list(type = "A", count = auto_dip),
                       list(type = "H", count = auto_hap),
                       list(type = "X", count =  x_linked),
                       list(type = "Y", count =  y_linked),
                       list(type = "M", count =  mito))
    ## check if any non-null values in input
    if(!is.null(locus_list)) {
        out <- unlist(lapply(
                1:length(locus_list), 
                function(ind) {
                    if(locus_list[[ind]]$count > 0) {
                        return(str_c(locus_list[[ind]]$count, " ",
                                     "<", locus_list[[ind]]$type, ">"))
                    }
                }
            ))
    }
    ## output
    return(out)
}


#' Parse locus description setting
#' @keywords internal
#' @author Ghislain Durif
parse_locus_description <- function(locus_count, locus_type = "snp") {
    out <- NULL
    ## check for null input
    if(!is.null(locus_count)) {
        ## snp format
        if(locus_type == "snp") {
            ## format locus description
            out <- unlist(lapply(
                1:length(locus_count), 
                function(ind) {
                    return(str_c(locus_count[ind], " ",
                                 "[P]", " ",
                                 "G", ind))
                }
            ))
        } else {
            warning("other locus type not supported at the moment.")
        }
    }
    ## output
    return(out)
}
