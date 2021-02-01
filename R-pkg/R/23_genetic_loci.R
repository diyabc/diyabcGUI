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


#' MSS locus group setup ui
#' @keywords internal
#' @author Ghislain Durif
mss_group_setup_ui <- function(id) {
    ns <- NS(id)
    tagList(
        hr(),
        h3(icon("object-group"), "Microsat/Sequence locus grouping"),
        helpText(
            "By default, all Microsat loci are grouped together,",
            "same for all Sequence loci."
        ),
        br(),
        h4("Microsat Loci"),
        actionButton(
            ns("enable_microsat_setup"),
            label = tags$b("Show/hide Microsat locus grouping configuration"),
            width = '100%'
        ),
        uiOutput(ns("microsat_setup")),
        br(),
        br(),
        actionButton(
            ns("enable_microsat_setup_motif"),
            label = tags$b("Show/hide Microsat locus motif and range configuration"),
            width = '100%'
        ),
        helpText(
            "By default, all Microsat loci are assumed to be dinucleid",
            "(motif = 2) with a range of 40."
        ),
        uiOutput(ns("microsat_setup_motif")),
        br(),
        hr(),
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

#' MSS locus group setup server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom dplyr left_join
mss_group_setup_server <- function(input, output, session, 
                                   data_info = reactive({NULL})) {
    
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        microsat_locus = list(),
        microsat_locus_type = list(),
        microsat_locus_motif_range = list(),
        seq_locus = list(),
        seq_locus_type = list(),
        seq_length = NULL,
        raw_microsat_locus = list(),
        raw_seq_locus = list(),
        # input
        data_info = NULL
    )
    # get input
    observe({
        local$data_info <- data_info()
        # # debugging
        # pprint("data info")
        # pprint(local$data_info)
    })
    
    # init output
    out <- reactiveValues(
        raw_locus = list(),
        group_info = list()
    )
    
    ## show/hide microsat grouping
    observeEvent(input$enable_microsat_setup, {
        req(!is.null(input$enable_microsat_setup))
        if(input$enable_microsat_setup %% 2 == 1) {
            shinyjs::hide(id = "microsat_setup")
        } else{
            shinyjs::show(id = "microsat_setup")
        }
    })
    
    ## show/hide seq grouping
    observeEvent(input$enable_seq_setup, {
        req(!is.null(input$enable_seq_setup))
        if(input$enable_seq_setup %% 2 == 1) {
            shinyjs::hide(id = "seq_setup")
        } else{
            shinyjs::show(id = "seq_setup")
        }
    })
    
    # microsat
    observe({
        req(!is.null(local$data_info))
        req(!is.null(local$data_info$locus_mode))
        req(!is.null(local$data_info$locus_name))
        req(!is.null(local$data_info$locus))
        
        microsat_locus <- str_detect(local$data_info$locus_mode, "microsat")
        if(sum(microsat_locus) > 0) {
            local$microsat_locus <- local$data_info$locus_name[microsat_locus]
            local$microsat_locus_type <- local$data_info$locus[microsat_locus]
        } else {
            local$microsat_locus <- list()
            local$microsat_locus_type <- list()
        }
        
        seq_locus <- str_detect(local$data_info$locus_mode, "seq")
        if(sum(seq_locus) > 0) {
            local$seq_locus <- local$data_info$locus_name[seq_locus]
            local$seq_locus_type <- local$data_info$locus[seq_locus]
            local$seq_length <- local$data_info$seq_length[seq_locus]
        } else {
            local$seq_locus <- list()
            local$seq_locus_type <- list()
            local$seq_length <- list()
        }
    })
    
    # observe({
    #     pprint("microsat locus")
    #     pprint(local$microsat_locus)
    #     pprint("seq locus")
    #     pprint(local$seq_locus)
    # })
    
    # setup microsat
    output$microsat_setup <- renderUI({
        req(!is.null(length(local$microsat_locus)))
        if(length(local$microsat_locus) > 0) {
            locus_group_setup_ui(ns("microsat_grouping"))
        } else {
            helpText(
                icon("warning"), "No Microsat locus in data."
            )
        }
    })
    
    microsat_group <- callModule(
        locus_group_setup_server,
        "microsat_grouping",
        locus_name = reactive(local$microsat_locus)
    )
    
    # observe({
    #     pprint("nb of microsat group")
    #     pprint(microsat_group$n_group)
    # })
    
    # setup seq
    output$seq_setup <- renderUI({
        req(!is.null(length(local$seq_locus)))
        if(length(local$seq_locus) > 0) {
            locus_group_setup_ui(ns("seq_grouping"))
        } else {
            helpText(
                icon("warning"), "No Sequence locus in data."
            )
        }
    })
    
    seq_group <- callModule(
        locus_group_setup_server,
        "seq_grouping",
        locus_name = reactive(local$seq_locus),
        n_existing_group = reactive(microsat_group$n_group)
    )
    
    # observe({
    #     pprint("nb of seq group")
    #     pprint(seq_group$n_group)
    #     pprint(reactiveValuesToList(seq_group))
    # })
    
    ## show/hide microsat motif/range set up
    observeEvent(input$enable_microsat_setup_motif, {
        req(!is.null(input$enable_microsat_setup_motif))
        if(input$enable_microsat_setup_motif %% 2 == 1) {
            shinyjs::hide(id = "microsat_setup_motif")
        } else {
            shinyjs::show(id = "microsat_setup_motif")
        }
    })
    
    # microsat motif/range setup
    output$microsat_setup_motif <- renderUI({
        req(length(local$microsat_locus) > 0)
        tag_list <- lapply(
            local$microsat_locus,
            function(item) {
                fluidRow(
                    column(
                        width = 4,
                        shinyjs::disabled(
                            textInput(
                                ns(str_c(item, "_name2")),
                                label = "Locus",
                                value = item
                            )
                        )
                    ),
                    column(
                        width = 4,
                        numericInput(
                            ns(str_c(item, "_motif")),
                            label = "Motif",
                            value = 2,
                            min = 0,
                            max = 10
                        )
                    ),
                    column(
                        width = 4,
                        numericInput(
                            ns(str_c(item, "_range")),
                            label = "Range",
                            value = 40,
                            min = 10,
                            max = 100
                        )
                    )
                )
            }
        )
        do.call(
            tagList,
            tag_list
        )
    })
    
    ## format microsat motif/range setup
    observe({
        req(length(local$microsat_locus) > 0)
        
        local$microsat_locus_motif_range <- Reduce(
            "rbind",
            lapply(
                local$microsat_locus, 
                function(item) {
                    if(!is.null(input[[ str_c(item, "_motif") ]]) &
                       !is.null(input[[ str_c(item, "_range") ]])) {
                        return(
                            data.frame(
                                name = item,
                                motif = input[[ str_c(item, "_motif") ]],
                                range = input[[ str_c(item, "_range") ]],
                                stringsAsFactors = FALSE
                            )
                        )
                    } else {
                        return(NULL)
                    }
                    
                }
            )
        )
        
        # pprint("microsat_locus_motif_range")
        # pprint(local$microsat_locus_motif_range)
    })
    
    ## format microsat locus
    observe({
        req(length(local$microsat_locus) > 0)
        req(length(microsat_group$locus_group) > 0)
        req(all(str_length(microsat_group$locus_group) > 0))
        req(is.data.frame(local$microsat_locus_motif_range))
        req(nrow(local$microsat_locus_motif_range) > 0)
        
        tmp_microsat_group <- data.frame(
            name = local$microsat_locus,
            type = str_c("<", local$microsat_locus_type, ">"),
            mode = rep("[M]", length(local$microsat_locus)),
            group = str_c("G", microsat_group$locus_group),
            stringsAsFactors = FALSE
        )
        
        tmp_microsat <- dplyr::left_join(tmp_microsat_group, 
                                         local$microsat_locus_motif_range,
                                         by = "name")
        
        # pprint("microsat locus")
        # pprint(tmp_microsat)
        
        local$raw_microsat_locus <- apply(
            tmp_microsat,
            1,
            str_c, collapse = " "
        )
        
        # pprint("raw microsat locus")
        # pprint(local$raw_microsat_locus)
        
    })
    
    ## format seq locus
    observe({
        req(length(local$seq_locus) > 0)
        req(length(seq_group$locus_group) > 0)
        req(all(str_length(seq_group$locus_group) > 0))
        req(length(local$seq_length) > 0)
        
        # pprint("seq locus group")
        # pprint(seq_group$locus_group)
        
        tmp_seq_group <- data.frame(
            name = local$seq_locus,
            type = str_c("<", local$seq_locus_type, ">"),
            mode = rep("[S]", length(local$seq_locus)),
            group = str_c("G", seq_group$locus_group),
            length = local$seq_length,
            stringsAsFactors = FALSE
        )
        
        # pprint("seq locus group")
        # pprint(tmp_seq_group)
        
        local$raw_seq_locus <- apply(
            tmp_seq_group,
            1,
            str_c, collapse = " "
        )
        
        # pprint("raw seq locus")
        # pprint(local$raw_seq_locus)
    })
    
    ## output
    observe({
        # pprint("raw microsat locus")
        # pprint(local$raw_microsat_locus)
        # pprint("raw seq locus")
        # pprint(local$raw_seq_locus)
        # pprint("locus name")
        # pprint(local$data_info$locus_name)
        
        req(length(local$raw_microsat_locus) + length(local$raw_seq_locus) > 0)
        req(!is.null(local$data_info$locus_name))
        
        tmp_raw_locus <- unlist(c(local$raw_microsat_locus, 
                                  local$raw_seq_locus))
        
        out$raw_locus <- left_join(
            data.frame(
                name = local$data_info$locus_name, 
                stringsAsFactors = FALSE
            ),
            data.frame(
                name = str_extract(tmp_raw_locus, "^[A-Za-z0-9_\\-]+(?= )"),
                info = tmp_raw_locus, 
                stringsAsFactors = FALSE
            ),
            by = "name"
        )$info
        
        # pprint("raw locus")
        # pprint(out$raw_locus)
    })
    
    # group info
    observe({
        req(length(out$raw_locus) > 0)
        
        tmp_group_info <- Reduce(
            "rbind",
            unique(
                str_extract_all(
                    out$raw_locus,
                    "(\\[[MS]\\])|(G[0-9]+)"
                )
            )
        )
        
        # issue when a single group
        if(length(tmp_group_info) == 2) {
            tmp_group_info <- data.frame(c1 = tmp_group_info[1],
                                         c2 = tmp_group_info[2])
        } else {
            tmp_group_info <- as.data.frame(tmp_group_info)
        }
        
        row.names(tmp_group_info) <- NULL
        colnames(tmp_group_info) <- c("mode", "group")
        out$group_info <- tmp_group_info
        
        # pprint("group info")
        # pprint(out$group_info)
    })
    
    return(out)
    
}

#' locus group setup ui
#' @keywords internal
#' @author Ghislain Durif
locus_group_setup_ui <- function(id) {
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
        uiOutput(ns("locus_group")),
        hr()
    )
}

#' locus group setup server
#' @keywords internal
#' @author Ghislain Durif
locus_group_setup_server <- function(input, output, session, 
                                     locus_name = reactive({NULL}),
                                     n_existing_group = reactive({0})) {
    
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        n_group = 1,
        possible_groups = list(),
        # input
        locus_name = NULL,
        n_existing_group = 0
    )
    # get input
    observe({
        local$locus_name <- locus_name()
        local$n_existing_group <- n_existing_group()
        
        # pprint("input locus group setup")
        # pprint(local$locus_name)
        # pprint(local$n_existing_group)
    })
    
    # init output
    out <- reactiveValues(
        n_group = 0,
        locus_group = list()
    )
    
    # possible groups
    observe({
        req(!is.null(local$n_existing_group))
        req(!is.null(local$n_group))
        
        local$possible_groups <- (1:local$n_group) + local$n_existing_group
    })
    
    # group making
    output$locus_group <- renderUI({
        req(!is.null(local$locus_name))
        req(length(local$locus_name) > 0)
        req(length(local$possible_groups) > 0)
        
        tag_list <- lapply(
            local$locus_name,
            function(item) {
                fluidRow(
                    column(
                        width = 6,
                        shinyjs::disabled(
                            textInput(
                                ns(str_c(item, "_name")),
                                label = "Locus",
                                value = item
                            )
                        )
                    ),
                    column(
                        width = 6,
                        selectInput(
                            ns(str_c(item, "_group")),
                            label = "Group",
                            choices = as.character(local$possible_groups),
                            selected = 1
                        )
                    )
                )
            }
        )
        do.call(tagList, tag_list)
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
        req(local$n_group <= length(local$locus_name))
        local$n_group <- local$n_group + 1
    })
    
    # remove group
    observeEvent(input$rm_group, {
        req(local$n_group > 1)
        local$n_group <- local$n_group - 1
    })
    
    ## output
    observe({
        req(!is.null(local$locus_name))
        out$locus_group <- unlist(lapply(
            local$locus_name,
            function(item) {
                req(!is.null(input[[ str_c(item, "_group") ]]))
                return(input[[ str_c(item, "_group") ]])
            }
        ))
        
        # pprint("locus group")
        # pprint(out$locus_group)
    })
    
    observe({
        req(length(out$locus_group) > 0)
        req(all(str_length(out$locus_group) > 0))
        out$n_group <- max(as.integer(out$locus_group))
        
        # pprint("nb locus group")
        # pprint(out$n_group)
    })
    
    return(out)
}

#' Group prior ui
#' @keywords internal
#' @author Ghislain Durif
group_prior_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        tags$h5(textOutput(
            ns("param_name")
        )),
        fluidRow(
            column(
                width = 4,
                uiOutput(ns("input_prior_type"))
            ),
            column(
                width = 8,
                fluidRow(
                    column(
                        width = 3,
                        splitLayout(
                            tags$h5(
                                "Min.", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            textInput(
                                ns("min"), label = NULL, value = "1e-05"
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    ),
                    column(
                        width = 3,
                        splitLayout(
                            tags$h5(
                                "Max.", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            textInput(
                                ns("max"), label = NULL, value = "1e-03"
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    ),
                    column(
                        width = 3,
                        splitLayout(
                            tags$h5(
                                "Mean", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            textInput(
                                ns("mean"), label = NULL, value = "1e-04"
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    ),
                    column(
                        width = 3,
                        splitLayout(
                            tags$h5(
                                "Shape", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            numericInput(
                                ns("shape"), label = NULL,
                                value = 2, step = 0.0001, min = 0
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    )
                )
            )
        ),
        hr()
    )
}

#' Group prior server
#' @keywords internal
#' @author Ghislain Durif
group_prior_server <- function(input, output, session,
                               gamma = reactive({FALSE}), 
                               group_name = reactive({NULL}),
                               locus_mode = reactive({NULL}),
                               mean_value = reactive({NULL}),
                               min_def_value = reactive({NULL}),
                               max_def_value = reactive({NULL}),
                               mean_def_value = reactive({NULL}),
                               note = reactive({NULL}),
                               param_name = reactive({NULL}),
                               param_desc = reactive({NULL}))  {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        # input
        gamma = FALSE,
        group_name = NULL,
        locus_mode = NULL,
        mean_value = NA,
        min_def_value = NULL,
        max_def_value = NULL,
        mean_def_value = NULL,
        note = NULL,
        param_name = NULL,
        param_desc = NULL
    )
    # get input
    observe({
        local$gamma <- gamma()
        local$group_name <- group_name()
        local$locus_mode <- locus_mode()
        local$mean_value <- mean_value()
        local$min_def_value <- min_def_value()
        local$max_def_value <- max_def_value()
        local$mean_def_value <- mean_def_value()
        local$note <- note()
        local$param_name <- param_name()
        local$param_desc <- param_desc()
    })
    
    # debugging
    # observe({
    #     pprint("---- param :")
    #     pprint(local$param_name)
    #     pprint("gamma :")
    #     pprint(local$gamma)
    #     pprint("mean value =")
    #     pprint(local$mean_value)
    #     pprint("mean def value =")
    #     pprint(local$mean_def_value)
    #     pprint("min def value =")
    #     pprint(local$min_def_value)
    #     pprint("max def value =")
    #     pprint(local$max_def_value)
    # })
    
    # init output
    out <- reactiveValues(
        group = NULL,
        raw = NULL, 
        valid = TRUE
    )
    
    # update group name
    observe({
        out$group <- local$group_name
    })
    
    # update param name output
    output$param_name <- renderText({
        req(!is.null(local$param_desc))
        local$param_desc
    })
    
    # update prior type input
    output$input_prior_type <- renderUI({
        req(!is.null(local$gamma))
        if(local$gamma) {
            radioGroupButtons(
                ns("prior_type"),
                label = NULL,
                choices = list("Gamma" = "GA"),
                selected = "GA",
                justified = TRUE
            )
        } else {
            radioGroupButtons(
                ns("prior_type"),
                label = NULL,
                choices = list("Uniform" = "UN", "Log-Unif." = "LU",
                               "Gamma" = "GA"),
                selected = "UN",
                justified = TRUE
            )
        }
    })
    
    # update min input
    observe({
        # pprint("test update min")
        # pprint(local$min_def_value)
        req(!is.null(local$min_def_value))
        updateTextInput(
            session, "min", 
            value = local$min_def_value
        )
    })
    
    # update max input
    observe({
        # pprint("test update max")
        # pprint(local$max_def_value)
        req(!is.null(local$max_def_value))
        updateTextInput(
            session, "max", 
            value = local$max_def_value
        )
    })
    
    # update mean input
    observe({
        # pprint("test update mean")
        # pprint(local$mean_value)
        # pprint(local$mean_def_value)
        req(!is.null(local$mean_value))
        if(is.na(local$mean_value)) {
            shinyjs::enable("mean")
            req(!is.null(local$mean_def_value))
            req(!is.na(local$mean_def_value))
            updateTextInput(
                session, "mean", 
                value = local$mean_def_value
            )
        } else {
            req(!is.null(local$mean_value))
            req(!is.na(local$mean_value))
            updateTextInput(
                session, "mean", 
                value = local$mean_value
            )
            shinyjs::disable("mean")
        }
    })
    
    ## check for min
    observe({
        req(local$param_name)
        req(!is.null(input$min))
        # pprint("input min")
        # pprint(input$min)
        tmp_min <- as.numeric(input$min)
        if(is.na(tmp_min)) {
            out$valid <- FALSE
            showNotification(
                id = ns("issue_min"),
                type = "warning",
                closeButton = TRUE,
                duration = 10,
                tags$p(
                    icon("warning"),
                    str_c(
                        "For parameter `", local$param_name, "`: ",
                        "min should be a numeric value."
                    )
                )
            )
        } else {
            out$valid <- TRUE
        }
    })
    
    ## check for max
    observe({
        req(local$param_name)
        req(!is.null(input$max))
        tmp_max <- as.numeric(input$max)
        if(is.na(tmp_max)) {
            out$valid <- FALSE
            showNotification(
                id = ns("issue_max"),
                type = "warning",
                closeButton = TRUE,
                duration = 10,
                tags$p(
                    icon("warning"),
                    str_c(
                        "For parameter `", local$param_name, "`: ",
                        "max should be a numeric value."
                    )
                )
            )
        } else {
            out$valid <- TRUE
        }
    })
    
    ## check for min/max
    observe({
        req(local$param_name)
        req(!is.null(input$min))
        tmp_min <- as.numeric(input$min)
        req(!is.na(tmp_min))
        req(!is.null(input$max))
        tmp_max <- as.numeric(input$max)
        req(!is.na(tmp_max))
        if(tmp_min >= tmp_max) {
            out$valid <- FALSE
            showNotification(
                id = ns("issue_min_max"),
                type = "warning",
                closeButton = TRUE,
                duration = 10,
                tags$p(
                    icon("warning"),
                    str_c(
                        "For parameter `", local$param_name, "`: ",
                        "min should be lower than max."
                    )
                )
            )
        } else {
            out$valid <- TRUE
        }
    })
    
    ## disable mean if gamma
    observeEvent(local$gamma, {
        req(!is.null(local$gamma))
        if(local$gamma) {
            shinyjs::disable("mean")
        } else {
            shinyjs::enable("mean")
        }
    })
    
    ## disable mean and shape if uniform or log-uniform
    observeEvent(input$prior_type, {
        req(!is.null(local$gamma))
        req(input$prior_type)
        if(input$prior_type %in% c("UN", "LU")) {
            shinyjs::disable("mean")
            shinyjs::disable("shape")
        } else {
            if(!local$gamma) {
                shinyjs::enable("mean")
            }
            shinyjs::enable("shape")
        }
    })
    
    ## check for gamma parameter setting
    observe({
        req(!local$gamma)
        req(local$param_name)
        req(input$prior_type)
        
        req(is.na(local$mean_value))
        
        req(!is.null(input$min))
        tmp_min <- as.numeric(input$min)
        req(!is.na(tmp_min))
        req(!is.null(input$max))
        tmp_max <- as.numeric(input$max)
        req(!is.na(tmp_max))
        req(!is.null(input$mean))
        tmp_mean <- as.numeric(input$mean)
        req(!is.na(tmp_mean))
        
        if(input$prior_type %in% c("GA")) {
            if(tmp_mean < tmp_min | tmp_mean > tmp_max) {
                out$valid <- FALSE
                showNotification(
                    id = ns("issue_gamma"),
                    type = "warning",
                    closeButton = TRUE,
                    duration = 10,
                    tags$p(
                        icon("warning"),
                        str_c(
                            "For parameter `", local$param_name, "`: ",
                            "mean should be between max and min values."
                        )
                    )
                )
            } else {
                out$valid <- TRUE
            }
        }
    })
    
    # observe({
    #     logging("parameter: ", local$param_name)
    #     logging("min = ", input$min)
    #     logging("max = ", input$max)
    #     logging("mean = ", input$mean)
    #     logging("shape = ", input$shape)
    # })
    
    ## encode output
    observe({
        req(local$param_name)
        req(input$prior_type)
        req(!is.null(input$min))
        req(!is.null(input$max))
        req(!is.null(input$mean))
        req(is.numeric(input$shape))
        out$raw <- str_c(local$param_name, " ",
                         input$prior_type, "[",
                         input$min, ",", input$max, ",",
                         input$mean, ",", input$shape, "]")
        # logging("raw prior def = ", out$raw)
    })
    ## output
    return(out)
}

#' MSS group prior ui
#' @keywords internal
#' @author Ghislain Durif
mss_group_prior_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        hr(),
        h3(icon("signal"), "Microsat/Sequence group priors"),
        br(),
        h4("Microsat loci"),
        uiOutput(ns("microsat_group_prior")),
        br(),
        h4("Sequence loci"),
        uiOutput(ns("seq_group_prior"))
    )
}

#' MSS group prior server
#' @keywords internal
#' @author Ghislain Durif
mss_group_prior_server <- function(input, output, session,
                                   group_info = reactive({NULL})) {
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        microsat_group = list(),
        n_microsat_group = 0,
        microsat_param = data.frame(
            param = c("MEANMU", "GAMMU", "MEANP", "GAMP", "MEANSNI", "GAMSNI"),
            meaning = c(
                "Mean mutation rate (per site, per generation)",
                "Individual locus mutation rate",
                "Mean coefficient P",
                "Individual locus coefficient P",
                "Mean SNI rate",
                "Individual locus SNI rate"
            ),
            gamma = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
            min_def_value = c("1e-4", "1e-5", "1e-1", "1e-2", "1e-8", "1e-9"),
            max_def_value = c("1e-3", "1e-2", "3e-1", "9e-1", "1e-5", "1e-4"),
            mean_def_value = c("5e-4", NA, "2.2e-1", NA, "1e-7", NA),
            mean_value = c(NA, "Mean_u", NA, "Mean_P", NA, "Mean_u_SNI"),
            note = c(NA, 1, 2, 1, 3, 1),
            stringsAsFactors = FALSE
        ),
        microsat_param_group = list(),
        microsat_prior_list = list(),
        raw_microsat_prior_list = list(),
        seq_group = list(),
        n_seq_group = 0,
        seq_param = data.frame(
            param = c("MEANMU", "GAMMU", "MEANK1", "GAMK1", "MEANK2", "GAMK2"),
            meaning = c(
                "Mean mutation rate (per site, per generation)",
                "Individual locus mutation rate",
                "Mean coefficient k_C/T",
                "Individual locus coefficient k_C/T",
                "Mean coefficient k_A/G",
                "Individual locus coefficient k_A/G"
            ),
            gamma = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
            min_def_value = c("1e-9", "1e-9", "0.05", "0.05", "0.05", "0.05"),
            max_def_value = c("1e-7", "1e-6", "20", "20", "20", "20"),
            mean_def_value = c("5e-9", NA, "10", NA, "10", NA),
            mean_value = c(NA, "Mean_u", NA, "Mean_k1", NA, "Mean_k2"),
            stringsAsFactors = FALSE
        ),
        seq_param_group = list(),
        seq_prior_list = list(),
        seq_model_list = list(),
        raw_seq_prior_list = list(),
        # rf col name
        microsat_col_name = NULL,
        seq_col_name = NULL,
        # input
        group_info = NULL
    )
    
    # get input
    observe({
        local$group_info <- group_info()
        # pprint("MSS group info")
        # pprint(local$group_info)
    })
    
    # init output
    out <- reactiveValues(
        raw_group_prior_list = list(),
        rf_col_name = NULL
    )
    
    # parse input
    observe({
        # pprint("group info")
        # pprint(local$group_info)
        
        req(is.data.frame(local$group_info))
        req(nrow(local$group_info) > 0)
        
        req(!is.null(local$group_info$mode))
        req(!is.null(local$group_info$group))
        
        if(any(local$group_info$mode == "[M]")) {
            local$microsat_group <- 
                local$group_info$group[local$group_info$mode == "[M]"]
            local$n_microsat_group <- sum(local$group_info$mode == "[M]")
        }
        
        if(any(local$group_info$mode == "[S]")) {
            local$seq_group <- 
                local$group_info$group[local$group_info$mode == "[S]"]
            local$n_seq_group <- sum(local$group_info$mode == "[S]")
        }
    })
    
    # microsat parameter
    observe({
        req(local$n_microsat_group > 0)
        
        microsat_param_group <- local$microsat_param[
            rep(seq(nrow(local$microsat_param)), 
                each = local$n_microsat_group),
        ]
        microsat_param_group$group <- local$microsat_group
        local$microsat_param_group <- microsat_param_group
    })
    
    # seq parameter
    observe({
        req(local$n_seq_group > 0)
        
        seq_param_group <- local$seq_param[
            rep(seq(nrow(local$seq_param)), 
                each = local$n_seq_group),
        ]
        seq_param_group$group <- local$seq_group
        local$seq_param_group <- seq_param_group
    })
    
    ## set up microsat group prior
    output$microsat_group_prior <- renderUI({
        if(length(local$microsat_group) == 0) {
            helpText(
                "No Microsat locus in data."
            )
        } else {
            tag_list <- lapply(
                local$microsat_group,
                function(item) {
                    tag_list1 <- lapply(
                        split(local$microsat_param, 
                              seq(nrow(local$microsat_param))),
                        function(item1) {
                            group_prior_ui(
                                ns(str_c("M_group_", item, "_", 
                                         item1$param, "_prior"))
                            )
                        }
                    )
                    tagList(
                        tags$ul(tags$li(tags$h5("Group", tags$b(item)))),
                        do.call(tagList, tag_list1)
                    )
                }
            )
            do.call(tagList, tag_list)
        }
    })
    # server side
    observe({
        req(length(local$microsat_group) > 0)
        local$microsat_prior_list <<- lapply(
            split(local$microsat_param_group, 
                  seq(nrow(local$microsat_param_group))),
            function(item) {
                callModule(
                    group_prior_server, 
                    str_c("M_group_", item$group, "_", item$param, "_prior"),
                    gamma = reactive(item$gamma), 
                    group_name = reactive(item$group),
                    locus_mode = reactive({"[M]"}),
                    mean_value = reactive(item$mean_value),
                    min_def_value = reactive(item$min_def_value),
                    max_def_value = reactive(item$max_def_value),
                    mean_def_value = reactive(item$mean_def_value),
                    note = reactive(item$note),
                    param_name = reactive(item$param),
                    param_desc = reactive(item$meaning)
                )
            }
        )
    })
    
    # observe({
    #     pprint("microsat prior list")
    #     pprint(local$microsat_prior_list)
    # })
    
    ## get output
    observe({
        local$raw_microsat_prior_list <- Reduce("rbind", lapply(
            local$microsat_prior_list,
            function(item) {
                req(!is.null(item$valid))
                req(!is.null(item$raw))
                req(!is.null(item$group))
                if(item$valid) {
                    return(data.frame(group = item$group, prior=item$raw,
                                      stringsAsFactors = FALSE))
                }
            }
        ))
    })
    
    # observe({
    #     pprint("raw microsat prior list")
    #     pprint(local$raw_microsat_prior_list)
    # })
    
    ## set up seq mutational model and seq group prior
    output$seq_group_prior <- renderUI({
        if(length(local$seq_group) == 0) {
            helpText(
                "No seq locus in data."
            )
        } else {
            tag_list <- lapply(
                local$seq_group,
                function(item) {
                    tag_list1 <- lapply(
                        split(local$seq_param, seq(nrow(local$seq_param))),
                        function(item1) {
                            group_prior_ui(
                                ns(str_c("S_group_", item, "_", 
                                         item1$param, "_prior"))
                            )
                        }
                    )
                    
                    tagList(
                        tags$ul(tags$li(tags$h5("Group", tags$b(item)))),
                        tagList(
                            selectInput(
                                ns(str_c("group_", item, "_mutation_model")),
                                label = "Mutation model",
                                choices = list(
                                    "Jukes Kantor (1969)" = "JK",
                                    "Kimura-2-parameters (1980)" = "K2P",
                                    "Hasegawa-Kishino-Yano (1985)" = "HKY",
                                    "Tamura Nei (1993)" = "TN"
                                ),
                                selected = "K2P"
                            ),
                            numericInput(
                                ns(str_c("group_", item, 
                                         "_perc_invariant_site")),
                                label = "Percentage of invariant site",
                                min = 0, max = 100, value = 10, step = 0.5
                            ),
                            numericInput(
                                ns(str_c("group_", item, "_gamma_shape")),
                                label = "Shape of the gamma",
                                min = 0, value = 2, step = 0.001
                            ),
                            br()
                        ),
                        do.call(tagList, tag_list1)
                    )
                }
            )
            do.call(tagList, tag_list)
        }
    })
    # server side
    observe({
        req(length(local$seq_group) > 0)
        local$seq_prior_list <<- lapply(
            split(local$seq_param_group, 
                  seq(nrow(local$seq_param_group))),
            function(item) {
                callModule(
                    group_prior_server, 
                    str_c("S_group_", item$group, "_", item$param, "_prior"),
                    gamma = reactive(item$gamma), 
                    group_name = reactive(item$group),
                    locus_mode = reactive({"[S]"}),
                    mean_value = reactive(item$mean_value),
                    min_def_value = reactive(item$min_def_value),
                    max_def_value = reactive(item$max_def_value),
                    mean_def_value = reactive(item$mean_def_value),
                    note = reactive({NULL}),
                    param_name = reactive(item$param),
                    param_desc = reactive(item$meaning)
                )
            }
        )
    })
    
    # observe({
    #     pprint("seq prior list")
    #     pprint(local$seq_prior_list)
    # })
    
    ## get output
    observe({
        local$raw_seq_prior_list <- Reduce("rbind", lapply(
            local$seq_prior_list,
            function(item) {
                req(!is.null(item$valid))
                req(!is.null(item$raw))
                req(!is.null(item$group))
                if(item$valid) {
                    return(data.frame(group = item$group, prior=item$raw,
                                      stringsAsFactors = FALSE))
                }
            }
        ))
    })
    
    # observe({
    #     pprint("raw seq prior list")
    #     pprint(local$raw_seq_prior_list)
    # })
    
    ## get mutation model for sequence
    observe({
        req(length(local$seq_group) > 0)
        local$seq_model_list <- Reduce("rbind", lapply(
            local$seq_group,
            function(item) {
                req(!is.null(input[[
                    str_c("group_", item, "_mutation_model")
                ]]))
                req(!is.null(input[[
                    str_c("group_", item,
                          "_perc_invariant_site")
                ]]))
                req(!is.null(input[[
                    str_c("group_", item, "_gamma_shape")
                ]]))
                
                tmp_model_id <- input[[
                    str_c("group_", item, "_mutation_model")
                ]]
                
                tmp_model <- str_c(
                    "MODEL",
                    input[[
                        str_c("group_", item, "_mutation_model")
                    ]],
                    input[[
                        str_c("group_", item,
                              "_perc_invariant_site")
                    ]],
                    input[[
                        str_c("group_", item, "_gamma_shape")
                    ]],
                    sep = " "
                )
                
                return(data.frame(group = item, 
                                  model_id = tmp_model_id, 
                                  model = tmp_model,
                                  stringsAsFactors = FALSE))
            }
        ))
    })
    
    # observe({
    #     pprint("seq model list")
    #     pprint(local$seq_model_list)
    # })
    
    ### reftable column names
    # microsat
    observe({
        local$microsat_col_name <- NULL
        req(length(local$microsat_group) > 0)
        rf_col <- c("mumic", "pmic", "snimic")
        group_id <- str_extract(local$microsat_group, "[0-9]+")
        if(length(group_id) > 0) {
            local$microsat_col_name <- apply(
                expand.grid(
                    rf_col, group_id, 
                    stringsAsFactors = FALSE, 
                    KEEP.OUT.ATTRS = FALSE
                ),
                1,
                str_c, collapse = "_"
            )
        }
        
        # pprint("microsat rf col name")
        # pprint(local$microsat_col_name)
    })
    
    # seq
    observe({
        local$seq_col_name <- NULL
        req(length(local$seq_group) > 0)
        req(is.data.frame(local$seq_model_list))
        req(nrow(local$seq_model_list) > 0)
        
        # "Jukes Kantor (1969)" = "JK" (MU)
        # "Kimura-2-parameters (1980)" = "K2P" (MU, K1)
        # "Hasegawa-Kishino-Yano (1985)" = "HKY" (MU, K1)
        # "Tamura Nei (1993)" = "TN" (MU, K1, K2)
        rf_col <- c("museq", "k1seq", "k2seq")
        
        local$seq_col_name <- unname(unlist(
            lapply(
                split(local$seq_model_list, seq(nrow(local$seq_model_list))),
                function(item) {
                    tmp_rf_col <- switch(
                        item$model_id,
                        "JK" = rf_col[1],
                        "K2P" = rf_col[1:2],
                        "HKY" = rf_col[1:2],
                        "TN" = rf_col[1:3]
                    )
                    tmp_group_id <- str_extract(
                        item$group, "[0-9]+"
                    )
                    return(
                        str_c(
                            tmp_rf_col, tmp_group_id, sep = "_"
                        )
                    )
                }
            )
        ))
        
        # pprint("seq rf col name")
        # pprint(local$seq_col_name)
    })
    
    
    ### output
    observe({
        
        # pprint("raw microsat prior list")
        # pprint(local$raw_microsat_prior_list)
        # 
        # pprint("raw seq prior list")
        # pprint(local$raw_seq_prior_list)
        # 
        # pprint("seq model list")
        # pprint(local$seq_model_list)
        
        tmp_microsat_prior <- NULL
        if(local$n_microsat_group > 0) {
            req(is.data.frame(local$raw_microsat_prior_list))
            req(nrow(local$raw_microsat_prior_list) > 0)
            
            tmp_microsat_prior <- lapply(
                local$microsat_group,
                function(item) {
                    if(any(local$raw_microsat_prior_list$group == item)) {
                        tmp <- c(
                            str_c("group", item, "[M]", sep = " "),
                            local$raw_microsat_prior_list$prior[
                                local$raw_microsat_prior_list$group == item
                            ]
                        )
                        return(tmp)
                    } else {
                        return(NULL)
                    }
                }
            )
        }
        # pprint(tmp_microsat_prior)
        
        tmp_seq_prior <- NULL
        if(local$n_seq_group > 0) {
            req(is.data.frame(local$raw_seq_prior_list))
            req(nrow(local$raw_seq_prior_list) > 0)
            
            req(is.data.frame(local$seq_model_list))
            req(nrow(local$seq_model_list) > 0)
            
            tmp_seq_prior <- lapply(
                local$seq_group,
                function(item) {
                    if(any(local$raw_seq_prior_list$group == item) &
                       any(local$seq_model_list$group == item)) {
                        tmp <- c(
                            str_c("group", item, "[S]", sep = " "),
                            local$raw_seq_prior_list$prior[
                                local$raw_seq_prior_list$group == item
                            ],
                            head(
                                local$seq_model_list$model[
                                    local$seq_model_list$group == item
                                ],
                                1
                            )
                        )
                        return(tmp)
                    } else {
                        return(NULL)
                    }
                }
            )
        }
        # pprint(tmp_seq_prior)
        
        out$raw_group_prior_list <- unlist(
            c(tmp_microsat_prior, tmp_seq_prior)
        )
        
        # pprint("raw_group_prior_list")
        # pprint(out$raw_group_prior_list)
    })
    
    # rf col name
    observe({
        local$rf_col_name <- NULL
        req(length(local$microsat_col_name) + length(local$seq_col_name) > 0)
        
        out$rf_col_name <- c(
            local$microsat_col_name, 
            local$seq_col_name
        )
        
        # pprint("rf col name")
        # pprint(out$rf_col_name)
    })
    
    return(out)
}
