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
                            locus_type = reactive({NULL}),
                            haploid = reactive({FALSE}),
                            poolseq = reactive({FALSE})) {
    
    # init local
    local <- reactiveValues(locus_type = NULL,
                            haploid = FALSE, poolseq = FALSE, 
                            auto_dip = 0, auto_hap = 0,
                            x_linked = 0, y_linked = 0,
                            mito = 0)
    
    # get input
    observe({
        local$locus_type <- locus_type()
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
        req(length(local$poolseq) > 0)
        req(length(local$haploid) > 0)
        if(!local$poolseq) {
            
            req(input$auto_dip)
            req(input$auto_hap)
            req(input$x_linked)
            req(input$y_linked)
            
            if(input$auto_hap > 0 || local$haploid) {
                shinyjs::enable("auto_hap")
                shinyjs::enable("mito")
                shinyjs::disable("auto_dip")
                shinyjs::disable("x_linked")
                shinyjs::disable("y_linked")
                updateNumericInput(session, "auto_dip", value = 0)
                updateNumericInput(session, "x_linked", value = 0)
                updateNumericInput(session, "y_linked", value = 0)
            } else if(input$auto_dip > 0 || input$x_linked > 0 || 
                      input$y_linked > 0) {
                shinyjs::disable("auto_hap")
                shinyjs::enable("mito")
                shinyjs::enable("auto_dip")
                shinyjs::enable("x_linked")
                shinyjs::enable("y_linked")
                updateNumericInput(session, "auto_hap", value = 0)
            } else {
                shinyjs::enable("auto_hap")
                shinyjs::enable("auto_dip")
                shinyjs::enable("x_linked")
                shinyjs::enable("y_linked")
                shinyjs::enable("mito")
            }
            
        } else {
            shinyjs::enable("auto_dip")
            shinyjs::disable("auto_hap")
            shinyjs::disable("x_linked")
            shinyjs::disable("y_linked")
            shinyjs::disable("mito")
            updateNumericInput(session, "auto_hap", value = 0)
            updateNumericInput(session, "x_linked", value = 0)
            updateNumericInput(session, "y_linked", value = 0)
            updateNumericInput(session, "mito", value = 0)
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
    
    # update haploid output
    observeEvent(input$auto_hap, {
        req(input$auto_hap)
        
        if(input$auto_hap > 0) {
            out$haploid <- TRUE
        } else {
            out$haploid <- FALSE
        }
    })
    
    # parse input
    observe({
        req(local$auto_dip)
        req(local$auto_hap)
        req(local$x_linked)
        req(local$y_linked)
        req(local$mito)
        req(local$locus_type)
        # pprint(local$auto_dip)
        # pprint(local$auto_hap)
        # pprint(local$x_linked)
        # pprint(local$y_linked)
        # pprint(local$mito)
        out$locus_count <- parse_locus_count(
            local$auto_dip, local$auto_hap,
            local$x_linked, local$y_linked,
            local$mito, local$locus_type)
        out$total_count <- sum(out$locus_count$count)
        # pprint("-- locus count =")
        # pprint(out$locus_count)
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
            h3("Locus number"),
            uiOutput(ns("locus_nb_ui")),
            helpText(
                icon("warning"),
                "Note: H loci are not compatible with A, X and Y loci."
            ),
            hr(),
            uiOutput(ns("mss_setup")),
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
genetic_loci_server <- function(input, output, session,
                                locus_type = reactive({NULL}),
                                seq_mode = reactive({NULL})) {
    # module namespace
    ns <- session$ns
    
    # init local reactive values
    local <- reactiveValues(
        haploid = FALSE,
        locus_nb_ui = NULL,
        poolseq = FALSE,
        locus_count = NULL,
        mss_locus_info = NULL,
        # input
        locus_type = NULL,
        seq_mode = NULL
    )
    
    # get input
    observe({
        local$locus_type <- locus_type()
        local$seq_mode <- seq_mode()
    })
    observe({
        req(local$seq_mode)
        local$poolseq <- (local$seq_mode == "poolseq")
    })
    
    # init output values
    out <- reactiveValues(locus_description = NULL, sex_ratio = NULL)
    
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
                           locus_type = reactive("sequence"),
                           haploid = reactive(local$haploid),
                           poolseq = reactive(local$poolseq))
    
    microsat_loci <- callModule(locus_nb_server, "microsat_loci", 
                                locus_type = reactive("microsat"),
                                haploid = reactive(local$haploid),
                                poolseq = reactive(local$poolseq))
    
    snp_loci <- callModule(locus_nb_server, "snp_loci", 
                           locus_type = reactive(local$locus_type),
                           haploid = reactive(local$haploid),
                           poolseq = reactive(local$poolseq))
    
    # update haploid status
    observe({
        # pprint(str_c("dna_loci$haploid = ", dna_loci$haploid))
        # pprint(str_c("microsat_loci$haploid = ", microsat_loci$haploid))
        # pprint(str_c("snp_loci$haploid = ", snp_loci$haploid))
        local$haploid <- any(c(dna_loci$haploid, 
                               microsat_loci$haploid,
                               snp_loci$haploid))
    })
    
    # parse numerical input and format output
    observe({
        req(local$locus_type)
        # req(!is.null(dna_loci$locus_count))
        # req(!is.null(microsat_loci$locus_count))
        # req(!is.null(snp_loci$locus_count))
        local$locus_count <- switch(
            local$locus_type,
            "mss" = rbind(
                dna_loci$locus_count,
                microsat_loci$locus_count
            ),
            "snp" = snp_loci$locus_count
        )
        
        # # debugging
        # pprint("locus count df")
        # pprint(local$locus_count)
        
        if(local$locus_type == "mss") {
            req(is.data.frame(local$locus_count))
            local$mss_data_info <- parse_mss_locus(local$locus_count)
            
            # pprint("mss data info")
            # pprint(local$mss_data_info)
        }
        # pprint(locus_count)
        # out$locus_description <- parse_locus_description(locus_count,
        #                                                  local$locus_type)
        # pprint(out$locus_description)
    })
    
    ## MSS setup
    output$mss_setup <- renderUI({
        if(local$locus_type == "mss") {
            tagList(
                mss_config_setup_ui(ns("mss_config")),
                br(),
                mss_group_prior_ui(ns("mss_prior"))
            )
        } else {
            NULL
        }
    })

    mss_config <- callModule(
        mss_config_setup_server, "mss_config",
        mss_data_info = reactive(local$mss_data_info),
        datagen_mode = TRUE
    )
    
    # mss_prior <- callModule(
    #     mss_group_prior_server,
    #     "mss_prior",
    #     group_info = reactive(mss_group$group_info)
    # )
    
    # get sex ratio
    observe({
        req(input$sex_ratio)
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
                              mito, locus_type) {
    out <- data.frame(
        type = c("A", "H", "X", "Y", "M"),
        count = c(auto_dip, auto_hap, x_linked, y_linked, mito),
        mode = rep(locus_type, 5),
        stringsAsFactors = FALSE
    )
    
    ## output
    return(out)
}

#' Parse MSS loci
#' @keywords internal
#' @author Ghislain Durif
#' @param locus_count data.frame with attributes `type` (among 
#' `"A"`, `"H"`, `"X"`, `"Y"`, `"M"`), `count` (number of corresponding locus),
#' `locus_mode` (among `"microsat"` or `sequence`).
#' @importFrom dplyr full_join
#' @importFrom rlang duplicate
#' @return data.frame 
parse_mss_locus <- function(locus_count) {
    out <- data.frame(
        type = character(0),
        mode = character(0),
        name = character(0),
        group = integer(0),
        motif = numeric(0),
        range = numeric(0),
        length = numeric(0),
        perc_A = numeric(0),
        perc_C = numeric(0),
        perc_G = numeric(0),
        perc_T = numeric(0),
        stringsAsFactors = FALSE
    )
    # note : some columns will be NAs
    #   for microsat loci: length, perc_A, perc_C, perc_G, perc_T
    #   for sequence loci: motif, range
    
    tmp_microsat_out <- rlang::duplicate(out)
    tmp_seq_out <- rlang::duplicate(out)
    
    if(nrow(locus_count) > 0) {
        
        # microsat locus
        microsat_locus_ind <- str_detect(locus_count$mode, "microsat") &
            locus_count$count > 0
        if(sum(microsat_locus_ind) > 0) {
            microsat_locus_count <- locus_count[microsat_locus_ind,]
            tmp_microsat_out <- Reduce("bind_rows", lapply(
                split(microsat_locus_count, seq(nrow(microsat_locus_count))),
                function(item) {
                    locus_mode <- "M"
                    tmp <- data.frame(
                        type = item$type,
                        mode = locus_mode,
                        name = str_c(
                            "Locus_", locus_mode, "_", item$type, "_",
                            1:item$count
                        ),
                        group = as.integer(1),
                        motif = 2,
                        range = 40,
                        stringsAsFactors = FALSE
                    )
                    return(tmp)
                }
            ))
        }
        
        # seq locus
        seq_locus_ind <- str_detect(locus_count$mode, "seq") &
            locus_count$count > 0
        if(sum(seq_locus_ind) > 0) {
            seq_locus_count <- locus_count[seq_locus_ind,]
            tmp_seq_out <- Reduce("bind_rows", lapply(
                split(seq_locus_count, seq(nrow(seq_locus_count))),
                function(item) {
                    locus_mode <- "S"
                    tmp <- data.frame(
                        type = item$type,
                        mode = locus_mode,
                        name = str_c(
                            "Locus_", locus_mode, "_", item$type, "_",
                            1:item$count
                        ),
                        group = as.integer(2),
                        length = 1000,
                        perc_A = 25,
                        perc_C = 25,
                        perc_G = 25,
                        perc_T = 25,
                        stringsAsFactors = FALSE
                    )
                    return(tmp)
                }
            ))
        }
    }
    
    # merge
    out <- suppressMessages(full_join(tmp_microsat_out, tmp_seq_out))
    
    # output
    return(out)
}

#' MSS setup for data generation module ui
#' @keywords internal
#' @author Ghislain Durif
mss_setup_ui <- function(id) {
    ns <- NS(id)
    tagList(
        mss_config_setup_ui(ns("mss_config"))
    )
}

#' MSS setup for data generation module server
#' @keywords internal
#' @author Ghislain Durif
mss_setup_server <- function(input, output, session) {

}

#' MSS locus confguration setup ui
#' @keywords internal
#' @author Ghislain Durif
mss_config_setup_ui <- function(id, datagen_mode = FALSE) {
    ns <- NS(id)
    tagList(
        hr(),
        h3(icon("object-group"), "Microsat/Sequence locus configuration"),
        helpText(
            "By default, all Microsat loci are grouped together,",
            "same for all Sequence loci."
        ),
        br(),
        h4("Microsat Loci"),
        helpText(
            "By default, all Microsat loci are assumed to be dinucleid",
            "(motif = 2) with a range of 40."
        ),
        actionButton(
            ns("enable_microsat_setup"),
            label = tags$b("Show/hide Microsat locus grouping configuration"),
            width = '100%'
        ),
        uiOutput(ns("microsat_setup")),
        br(),
        br(),
        h4("Sequence Loci"),
        actionButton(
            ns("enable_seq_setup"),
            label = tags$b("Show/hide Sequence locus grouping configuration"),
            width = '100%'
        ),
        uiOutput(ns("seq_setup")),
        hr()
    )
}

#' MSS locus configuration setup server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom dplyr left_join
mss_config_setup_server <- function(input, output, session, 
                                    mss_data_info = reactive({NULL}),
                                    datagen_mode = FALSE) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        microsat_data_info = NULL,
        seq_data_info = NULL,
        # input
        mss_data_info = NULL
    )
    
    # get input
    observe({
        local$mss_data_info <- mss_data_info()
        # # debugging
        # pprint("data info")
        # pprint(local$data_info)
    })
    
    # init output
    out <- reactiveValues(
        mss_data_info = NULL
    )
    
    # setup output
    observe({
        req(local$mss_data_info)
        req(is.data.frame(local$mss_data_info))
        req(nrow(local$mss_data_info) > 0)
        out$mss_data_info <- local$mss_data_info 
    })
    
    ## show/hide microsat setup
    observeEvent(input$enable_microsat_setup, {
        req(!is.null(input$enable_microsat_setup))
        if(input$enable_microsat_setup %% 2 == 1) {
            shinyjs::hide(id = "microsat_setup")
        } else{
            shinyjs::show(id = "microsat_setup")
        }
    })
    
    ## show/hide seq setup
    observeEvent(input$enable_seq_setup, {
        req(!is.null(input$enable_seq_setup))
        if(input$enable_seq_setup %% 2 == 1) {
            shinyjs::hide(id = "seq_setup")
        } else{
            shinyjs::show(id = "seq_setup")
        }
    })
    
    # specific mss data info for microsat or sequence
    observe({
        req(local$mss_data_info)
        req(is.data.frame(local$mss_data_info))
        req(nrow(local$mss_data_info) > 0)
        
        microsat_locus_ind <- local$mss_data_info$mode == "M"
        if(any(microsat_locus_ind)) {
            local$microsat_data_info <- 
                local$mss_data_info[microsat_locus_ind,]
        } else {
            local$microsat_data_info <- NULL
        }
        
        seq_locus_ind <- local$mss_data_info$mode == "S"
        if(any(seq_locus_ind)) {
            local$seq_data_info <- 
                local$mss_data_info[seq_locus_ind,]
        } else {
            local$seq_data_info <- NULL
        }
    })
    
    # setup microsat
    output$microsat_setup <- renderUI({
        if(isTruthy(local$microsat_data_info)) {
            mss_locus_setup_ui(ns("microsat_setup"))
        } else {
            helpText(
                icon("warning"), "No Microsat locus in data."
            )
        }
    })
    
    microsat_setup <- callModule(
        mss_locus_setup_server, "microsat_setup",
        locus_mode = reactive({"M"}),
        mss_data_info = reactive(local$microsat_data_info),
        n_existing_group = reactive({0}),
        datagen_mode = datagen_mode
    )
    
    # observe({
    #     pprint("nb of microsat group")
    #     pprint(microsat_group$n_group)
    # })
    
    # setup seq
    output$seq_setup <- renderUI({
        if(isTruthy(local$seq_data_info)) {
            mss_locus_setup_ui(ns("seq_setup"))
            # helpText(
            #     "The default length for the Sequences is 1000bp",
            #     "with an equiprobability situation (25% each)", 
            #     "regarding nucleotids 'A', 'C', 'G', 'T'."
            # ),
        } else {
            helpText(
                icon("warning"), "No Sequence locus in data."
            )
        }
    })
    
    seq_setup <- callModule(
        mss_locus_setup_server, "seq_setup",
        locus_mode = reactive({"S"}),
        mss_data_info = reactive(local$seq_data_info),
        n_existing_group = reactive(microsat_setup$n_group),
        datagen_mode = datagen_mode
    )
    
    # observe({
    #     pprint("nb of seq group")
    #     pprint(seq_group$n_group)
    #     pprint(reactiveValuesToList(seq_group))
    # })
    
    # ## format microsat locus
    # observe({
    #     req(length(local$microsat_locus) > 0)
    #     req(length(microsat_group$locus_group) > 0)
    #     req(all(str_length(microsat_group$locus_group) > 0))
    #     req(is.data.frame(local$microsat_locus_motif_range))
    #     req(nrow(local$microsat_locus_motif_range) > 0)
    #     
    #     tmp_microsat_group <- data.frame(
    #         name = local$microsat_locus,
    #         type = str_c("<", local$microsat_locus_type, ">"),
    #         mode = rep("[M]", length(local$microsat_locus)),
    #         group = str_c("G", microsat_group$locus_group),
    #         stringsAsFactors = FALSE
    #     )
    #     
    #     tmp_microsat <- dplyr::left_join(tmp_microsat_group, 
    #                                      local$microsat_locus_motif_range,
    #                                      by = "name")
    #     
    #     # pprint("microsat locus")
    #     # pprint(tmp_microsat)
    #     
    #     local$raw_microsat_locus <- apply(
    #         tmp_microsat,
    #         1,
    #         str_c, collapse = " "
    #     )
    #     
    #     # pprint("raw microsat locus")
    #     # pprint(local$raw_microsat_locus)
    #     
    # })
    # 
    # ## format seq locus
    # observe({
    #     req(length(local$seq_locus) > 0)
    #     req(length(seq_group$locus_group) > 0)
    #     req(all(str_length(seq_group$locus_group) > 0))
    #     req(length(local$seq_length) > 0)
    #     
    #     # pprint("seq locus group")
    #     # pprint(seq_group$locus_group)
    #     
    #     tmp_seq_group <- data.frame(
    #         name = local$seq_locus,
    #         type = str_c("<", local$seq_locus_type, ">"),
    #         mode = rep("[S]", length(local$seq_locus)),
    #         group = str_c("G", seq_group$locus_group),
    #         length = local$seq_length,
    #         stringsAsFactors = FALSE
    #     )
    #     
    #     # pprint("seq locus group")
    #     # pprint(tmp_seq_group)
    #     
    #     local$raw_seq_locus <- apply(
    #         tmp_seq_group,
    #         1,
    #         str_c, collapse = " "
    #     )
    #     
    #     # pprint("raw seq locus")
    #     # pprint(local$raw_seq_locus)
    # })
    # 
    # ## output
    # observe({
    #     # pprint("raw microsat locus")
    #     # pprint(local$raw_microsat_locus)
    #     # pprint("raw seq locus")
    #     # pprint(local$raw_seq_locus)
    #     # pprint("locus name")
    #     # pprint(local$data_info$locus_name)
    #     
    #     req(length(local$raw_microsat_locus) + length(local$raw_seq_locus) > 0)
    #     req(!is.null(local$data_info$locus_name))
    #     
    #     tmp_raw_locus <- unlist(c(local$raw_microsat_locus, 
    #                               local$raw_seq_locus))
    #     
    #     out$raw_locus <- left_join(
    #         data.frame(
    #             name = local$data_info$locus_name, 
    #             stringsAsFactors = FALSE
    #         ),
    #         data.frame(
    #             name = str_extract(tmp_raw_locus, "^[A-Za-z0-9_\\-]+(?= )"),
    #             info = tmp_raw_locus, 
    #             stringsAsFactors = FALSE
    #         ),
    #         by = "name"
    #     )$info
    #     
    #     # pprint("raw locus")
    #     # pprint(out$raw_locus)
    # })
    # 
    # # group info
    # observe({
    #     req(length(out$raw_locus) > 0)
    #     
    #     tmp_group_info <- Reduce(
    #         "rbind",
    #         unique(
    #             str_extract_all(
    #                 out$raw_locus,
    #                 "(\\[[MS]\\])|(G[0-9]+)"
    #             )
    #         )
    #     )
    #     
    #     # issue when a single group
    #     if(length(tmp_group_info) == 2) {
    #         tmp_group_info <- data.frame(c1 = tmp_group_info[1],
    #                                      c2 = tmp_group_info[2])
    #     } else {
    #         tmp_group_info <- as.data.frame(tmp_group_info)
    #     }
    #     
    #     row.names(tmp_group_info) <- NULL
    #     colnames(tmp_group_info) <- c("mode", "group")
    #     out$group_info <- tmp_group_info
    #     
    #     # pprint("group info")
    #     # pprint(out$group_info)
    # })
    
    return(out)
}


#' mss locus setup ui
#' @keywords internal
#' @author Ghislain Durif
mss_locus_setup_ui <- function(id) {
    ns <- NS(id)
    tagList(
        hr(),
        uiOutput(ns("n_group")),
        actionGroupButtons(
            inputIds = c(ns("add_group"), ns("rm_group")),
            labels = list(
                tags$span(icon("plus"), "Add group"),
                tags$span(icon("minus"), "Remove group")
            ),
            fullwidth = TRUE
        ),
        helpText(
            icon("warning"), 
            "Configure the number of groups before assigning loci to them."
        ),
        uiOutput(ns("locus_setup")),
        hr()
    )
}

#' mss locus setup server
#' @keywords internal
#' @author Ghislain Durif
mss_locus_setup_server <- function(input, output, session, 
                                   locus_mode = reactive({NULL}),
                                   mss_data_info = reactive({NULL}),
                                   n_existing_group = reactive({0}),
                                   datagen_mode = FALSE) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        n_group = 1,
        possible_groups = list(),
        # input
        locus_mode = NULL,
        mss_data_info = NULL,
        n_existing_group = 0
    )
    
    # get input
    observe({
        local$locus_mode <- locus_mode()
        local$mss_data_info <- mss_data_info()
        local$n_existing_group <- n_existing_group()
        if(!isTruthy(local$n_existing_group)) {
            local$n_existing_group <- 0
        }
    })
    
    # init output
    out <- reactiveValues(
        n_group = 0,
        mss_data_info = NULL
    )
    
    # possible groups
    observe({
        req(local$n_existing_group)
        req(local$n_group)
        
        local$possible_groups <- as.integer(
            (1:local$n_group) + local$n_existing_group
        )
    })
    
    # configuration ui
    output$locus_setup <- renderUI({
        
        req(local$mss_data_info)
        req(is.data.frame(local$mss_data_info))
        req(nrow(local$mss_data_info) > 0)
        req(local$locus_mode)
        
        locus_ind <- local$mss_data_info$mode == local$locus_mode
        if(any(locus_ind)) {
            tmp_mss_data_info <- local$mss_data_info[locus_ind,]
            
            tag_list <- lapply(
                split(tmp_mss_data_info, seq(nrow(tmp_mss_data_info))),
                function(item) {
                    return(
                        render_locus_ui(session, item, local$possible_groups, 
                                        datagen_mode)
                    )
                }
            )
            names(tag_list) <- NULL
            do.call(tagList, tag_list)
        }
    })
    
    # number of group
    output$n_group <- renderUI({
        req(!is.null(local$n_group))
        tags$h5(
            tags$b(
                "Number of groups = ", 
                as.character(local$n_group)
            ),
            style = "text-align: center;"
        )
    })
    
    # add group
    observeEvent(input$add_group, {
        req(local$mss_data_info)
        req(is.data.frame(local$mss_data_info))
        req(nrow(local$mss_data_info) > 0)
        req(local$n_group <= nrow(local$mss_data_info))
        local$n_group <- local$n_group + 1
    })
    
    # remove group
    observeEvent(input$rm_group, {
        req(local$n_group > 1)
        local$n_group <- local$n_group - 1
    })
    
    ## output
    observe({
        req(local$mss_data_info)
        req(is.data.frame(local$mss_data_info))
        req(nrow(local$mss_data_info) > 0)
        req(local$locus_mode)
        
        for(ind in 1:nrow(local$mss_data_info)) {
            item_name <- local$mss_data_info$name[ind]
            # group
            tmp_id <- str_c(item_name, "_group")
            if(isTruthy(input[[ tmp_id ]])) {
                current_value <- as.integer(input[[ tmp_id ]])
                if(current_value != local$mss_data_info$group[ind]) {
                    local$mss_data_info$group[ind] <- current_value
                }
            }
            ## parameters
            tmp_param_val <- character(0)
            if(local$locus_mode == "M") {
                tmp_param_val <- c("motif", "range")
            } else if(local$locus_mode == "S" && datagen_mode) {
                tmp_param_val <- c("length", "perc_A", "perc_C", "perc_G", 
                                   "perc_T")
            }
            for(tmp_param in tmp_param_val) {
                tmp_id <- str_c(item_name, tmp_param, sep = "_")
                if(isTruthy(input[[ tmp_id ]])) {
                    current_value <- input[[ tmp_id ]]
                    if(current_value != local$mss_data_info[ind, tmp_param]) {
                        local$mss_data_info[ind, tmp_param] <- current_value
                    }
                }
            }
        }
    })
    
    ## number of group
    observe({
        req(local$mss_data_info)
        req(is.data.frame(local$mss_data_info))
        req(nrow(local$mss_data_info) > 0)
        
        out$n_group <- max(as.integer(local$mss_data_info$group))
        
        # pprint("nb locus group")
        # pprint(out$n_group)
    })
    
    ## update output with input
    observe({
        out$mss_data_info <- local$mss_data_info
    })
    
    
    return(out)
}


#' Render locus configuration setup ui
#' @keywords internal
#' @author Ghislain Durif
render_locus_ui <- function(session, item, possible_groups, 
                            datagen_mode = FALSE) {

    ns <- session$ns
    
    col_width <- 3
    if(item$mode == "S" && datagen_mode) {
        col_width <- 2
    }
    
    # name and group
    # tmp_input1 <- list()
    tmp_input1 <- list(
        column(
            width = col_width,
            shinyjs::disabled(
                textInput(
                    ns(str_c(item$name, "_name")),
                    label = "Locus",
                    value = item$name
                )
            )
        ),
        column(
            width = col_width,
            selectInput(
                ns(str_c(item$name, "_group")),
                label = "Group",
                choices = possible_groups,
                selected = item$group
            )
        )
    )
    
    # motif/range for microsat or length/nucleotid proba for sequences
    tmp_input2 <- list()
    if(item$mode == "M") {
        tmp_input2 <- list(
            column(
                width = col_width,
                numericInput(
                    ns(str_c(item$name, "_motif")),
                    label = "Motif",
                    value = item$motif,
                    min = 0,
                    max = 10
                )
            ),
            column(
                width = col_width,
                numericInput(
                    ns(str_c(item$name, "_range")),
                    label = "Range",
                    value = item$range,
                    min = 10,
                    max = 100
                )
            )
        )
    } else if(item$mode == "S") {
        if(!datagen_mode) {
            tmp_input2 <- list(
                column(
                    width = col_width,
                    shinyjs::disabled(
                        numericInput(
                            ns(str_c(item$name, "_length")),
                            label = "Length",
                            value = item$length,
                            min = 0
                        )
                    )
                )
            )
        } else {
            tmp_input2 <- list(
                column(
                    width = col_width,
                    numericInput(
                        ns(str_c(item$name, "_length")),
                        label = "Length",
                        value = item$length,
                        min = 0
                    ),
                ),
                column(
                    width = 6,
                    fluidRow(
                        column(
                            width = 3,
                            numericInput(
                                ns(str_c(item$name, "_perc_A")),
                                label = "%A",
                                value = item$perc_A,
                                min = 0,
                                max = 100
                            )
                        ),
                        column(
                            width = 3,
                            numericInput(
                                ns(str_c(item$name, "_perc_C")),
                                label = "%C",
                                value = item$perc_C,
                                min = 0,
                                max = 100
                            )
                        ),
                        column(
                            width = 3,
                            numericInput(
                                ns(str_c(item$name, "_perc_G")),
                                label = "%G",
                                value = item$perc_G,
                                min = 0,
                                max = 100
                            )
                        ),
                        column(
                            width = 3,
                            numericInput(
                                ns(str_c(item$name, "_perc_T")),
                                label = "%T",
                                value = item$perc_T,
                                min = 0,
                                max = 100
                            )
                        )
                    )
                )
            )
        }
    }
    
    out <- c(tmp_input1, tmp_input2)
    names(out) <- NULL
    return(do.call(fluidRow, out))
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
