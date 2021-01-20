#' Training set sub-module ui
#' @keywords internal
#' @author Ghislain Durif
training_set_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("enable_def")),
        uiOutput(ns("enable_control"))
    )
}

#' Training set sub-module server
#' @keywords internal
#' @author Ghislain Durif
training_set_server <- function(input, output, session,
                                data_file = reactive({NULL}),
                                data_info = reactive({NULL}),
                                locus_type = reactive({NULL}),
                                seq_mode = reactive({NULL}),
                                new_proj = reactive({TRUE}),
                                proj_dir = reactive({NULL}), 
                                proj_file_list = reactive({NULL}), 
                                valid_proj = reactive({TRUE})) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        proj_header_content = NULL,
        # input
        data_file = NULL,
        data_info = NULL,
        locus_type = NULL,
        seq_mode = NULL,
        new_proj = NULL,
        proj_dir = NULL,
        proj_file_list = NULL,
        valid_proj = NULL
    )
    # get input
    observe({
        local$data_file <- data_file()
        local$data_info <- data_info()
        local$locus_type <- locus_type()
        local$seq_mode <- seq_mode()
        local$new_proj <- new_proj()
        local$proj_dir <- proj_dir()
        local$proj_file_list <- proj_file_list()
        local$valid_proj <- valid_proj()
        
        # # debugging
        # pprint("=== training set: project directory")
        # pprint(local$proj_dir)
    })
    
    # init output
    out <- reactiveValues(
        valid_def = FALSE
    )
    
    # # debugging
    # observe({
    #     pprint("data info")
    #     pprint(local$data_info)
    # })
    
    # check project directory
    proj_file_list <- function() return(list())
    observe({
        req(!is.null(local$proj_dir))
        
        proj_file_list <<- reactivePoll(
            2000, session,
            checkFunc = function() {
                if(dir.exists(local$proj_dir)) {
                    list.files(local$proj_dir)
                } else {
                    list()
                }
            },
            valueFunc = function() {
                if(dir.exists(local$proj_dir)) {
                    list.files(local$proj_dir)
                } else {
                    list()
                }
            }
        )
    })
        
    observe({
        local$proj_file_list <- proj_file_list()
    })
    
    ## check headerRF.txt file (if present)
    proj_header_content <- function() return(list())
    observe({
        req(!is.null(local$locus_type))
        req(!is.null(local$proj_dir))
        
        proj_header_content <<- reactiveFileReader(
            1000, session,
            file.path(local$proj_dir, "headerRF.txt"),
            function(file) {
                if(file.exists(file))
                    parse_diyabc_header(
                        file_name = file, 
                        file_type = "text/plain",
                        locus_type = local$locus_type
                    )
                else
                    list()
            }
        )
    })
    
    observe({
        local$proj_header_content <- proj_header_content()
    })
    
    # ## debugging
    # observeEvent(local$proj_header_content, {
    #     pprint("project header content")
    #     pprint(local$proj_header_content)
    # })
    
    ## training set def or show
    output$enable_def <- renderUI({
        
        # pprint("###### debug enable def")
        # logging("new proj =", local$new_proj)
        # logging("valid proj =", local$valid_proj)
        
        req(!is.null(local$new_proj))
        req(!is.null(local$valid_proj))
        
        if(!local$valid_proj) {
            helpText(
                icon("warning"), "Project set up is not valid."
            )
        } else if(local$new_proj) {
            training_set_def_ui(ns("def"))
        } else {
            show_existing_proj_ui(ns("show"))
        }
    })
    
    ## training set def
    training_set_def <- callModule(
        training_set_def_server, "def",
        data_file = reactive(local$data_file),
        data_info = reactive(local$data_info),
        locus_type = reactive(local$locus_type),
        seq_mode = reactive(local$seq_mode),
        proj_dir = reactive(local$proj_dir),
        raw_scenario_list = reactive({NULL}),
        valid_proj = reactive(local$valid_proj)
    )
    
    ## training set show (if existing)
    show_existing_proj <- callModule(
        show_existing_proj_server, "show",
        proj_header_content = reactive(local$proj_header_content),
        valid_proj = reactive(local$valid_proj)
    )
    
    ## valid def ?
    observe({
        req(!is.null(local$new_proj))
        req(!is.null(local$proj_header_content))
        
        if(local$new_proj) {
            out$valid_def <- training_set_def$valid_def
        } else {
            if(!is.null(local$proj_header_content$valid)) {
                out$valid_def <- local$proj_header_content$valid
            } else {
                out$valid_def <- FALSE
            }
        }
    })
    
    ## enable control
    output$enable_control <- renderUI({
        
        # pprint("###### debug enable control")
        # logging("valid proj =", local$valid_proj)
        
        req(!is.null(local$valid_proj))
        
        if(!local$valid_proj) {
            tagList(
                h3(icon("gear"), "Run"),
                helpText(icon("warning"), "Project is not ready.")
            )
        } else {
            training_set_action_ui(ns("control"))
        }
    })
    
    ## action
    callModule(
        training_set_action_server, "control", 
        proj_dir = reactive(local$proj_dir),
        proj_file_list = reactive(local$proj_file_list),
        valid_def = reactive(out$valid_def),
        valid_proj = reactive(local$valid_proj)
    )
    
    # # update output
    # observe({
    #     out$valid_proj <- local$valid_proj
    # })
    
    # output
    return(out)
}

#' Show existing project set up module ui
#' @keywords internal
#' @author Ghislain Durif
show_existing_proj_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        actionButton(
            ns("edit"),
            label = "Edit configuration",
            icon = icon("edit")
        ),
        helpText(
            "By editing the training set simulation configuration,", 
            "it will", tags$b("erase"), "the corresponding training set file",
            "(", tags$code("reftableRF.bin"), ")", 
            "if existing."
        ),
        hr(),
        h3(icon("history"), "Historical models"),
        helpText(
            "Historical scenarii defined in the provided header file."
        ),
        uiOutput(ns("show_scenarii")),
        hr(),
        h3(icon("chart-bar"), "Priors and conditions"),
        helpText(
            "Priors defined in the provided header file."
        ),
        uiOutput(ns("show_priors")),
        hr(),
        h4("Condition setting"),
        helpText(
            "Conditions defined in the provided header file."
        ),
        uiOutput(ns("show_conditions")),
        hr(),
        h3(icon("dna"), "Number of loci to simulate"),
        helpText(
            "Locus settings defined in the provided header file."
        ),
        uiOutput(ns("show_loci")),
        hr()
    )
}

#' Show existing project set up module server
#' @keywords internal
#' @author Ghislain Durif
show_existing_proj_server <- function(input, output, session,
                                      proj_header_content = reactive({NULL}),
                                      valid_proj = reactive({FALSE})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        # input
        proj_header_content = NULL,
        valid_proj = NULL
    )
    # get input
    observe({
        local$proj_header_content <- proj_header_content()
        local$valid_proj <- valid_proj()
    })
    
    ## edit
    observeEvent(input$edit, {
        show_alert(
            title = "Not available",
            text = "This functionality will be soon available.",
            type = "error"
        )
    })
    
    # show historical model
    output$show_scenarii <- renderUI({
        req(!is.null(local$valid_proj))
        req(!is.null(local$proj_header_content$raw_scenario_list))
        
        if(local$valid_proj & 
           length(local$proj_header_content$raw_scenario_list) > 0) {
            tagList(
                do.call(
                    flowLayout,
                    lapply(
                        local$proj_header_content$raw_scenario_list, 
                        function(item) tags$pre(item)
                    )
                )
            )
        } else {
            NULL
        }
    })
    
    # show priors
    output$show_priors <- renderUI({
        req(!is.null(local$valid_proj))
        req(!is.null(local$proj_header_content$raw_prior_list))
        
        if(local$valid_proj & 
           length(local$proj_header_content$raw_prior_list) > 0) {
            tagList(
                do.call(
                    tags$ul,
                    lapply(
                        local$proj_header_content$raw_prior_list,
                        function(item) tags$li(tags$code(item))
                    )
                )
            )
        } else {
            NULL
        }
    })
    
    # show conditions
    output$show_conditions <- renderUI({
        req(!is.null(local$valid_proj))
        req(!is.null(local$proj_header_content$raw_cond_list))
        
        if(local$valid_proj) { 
            if(length(local$proj_header_content$raw_cond_list) > 0) {
                tagList(
                    do.call(
                        tags$ul,
                        lapply(
                            local$proj_header_content$raw_cond_list, 
                            function(item) tags$li(tags$code(item))
                        )
                    )
                )
            } else {
                helpText("No condition in header file.")
            }
        } else {
            NULL
        }
    })
    
    # show locus description
    output$show_loci <- renderUI({
        req(!is.null(local$valid_proj))
        req(!is.null(local$proj_header_content$loci_description))
        
        if(local$valid_proj & 
           length(local$proj_header_content$loci_description) > 0) {
            tagList(
                do.call(
                    tags$ul,
                    lapply(
                        local$proj_header_content$loci_description, 
                        function(item) tags$li(tags$code(item))
                    )
                )
            )
        } else {
            NULL
        }
    })
    
}

#' Training set sub-module ui
#' @keywords internal
#' @author Ghislain Durif
training_set_def_ui <- function(id) {
    ns <- NS(id)
    tagList(
        hist_model_panel_ui(ns("hist_model_panel")),
        br(),
        hr(),
        prior_cond_set_ui(ns("prior_cond")),
        br(),
        hr(),
        locus_setup_ui(ns("locus_setup")),
        br(),
        br(),
        actionBttn(
            ns("validate"),
            label = "Validate",
            style = "fill",
            block = TRUE
        ),
        uiOutput(ns("feedback")),
        br(),
        hr()
    )
}

#' Training set sub-module server
#' @keywords internal
#' @author Ghislain Durif
training_set_def_server <- function(input, output, session, 
                                    data_file = reactive({NULL}),
                                    data_info = reactive({NULL}),
                                    locus_type = reactive({NULL}),
                                    seq_mode = reactive({NULL}),
                                    proj_dir = reactive({NULL}),
                                    raw_scenario_list = reactive({NULL}),
                                    valid_proj = reactive({TRUE})) {
    
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        cond_list = list(),
        param_list = list(),
        param_count_list = list(),
        raw_cond_list = list(),
        raw_param_list = list(),
        scenario_list = NULL,
        # input
        data_file = NULL,
        data_info = NULL,
        locus_type = NULL,
        seq_mode = NULL,
        proj_dir = NULL,
        raw_scenario_list = NULL,
        valid_proj = NULL
    )
    # get input
    observe({
        local$data_file <- data_file()
        local$data_info <- data_info()
        local$locus_type <- locus_type()
        local$seq_mode <- seq_mode()
        local$proj_dir <- proj_dir()
        # local$raw_scenario_list <- raw_scenario_list()
        local$valid_proj <- valid_proj()
    })
    
    # init output
    out <- reactiveValues(
        cond_list = list(),
        param_list = list(),
        raw_param_list = list(),
        trigger = NULL,
        valid_def = FALSE
    )
    
    ## historic model panel
    hist_model_def <- callModule(
        hist_model_panel_server, "hist_model_panel",
        project_dir = reactive(local$proj_dir), 
        raw_scenario_list = reactive(local$raw_scenario_list)
    )
    
    # # debugging
    # observe({
    #     pprint("hist_model_def")
    #     pprint(hist_model_def$scenario_list)
    # })
    
    # update local
    observe({
        local$scenario_list <- hist_model_def$scenario_list
        # extract input
        if(length(local$scenario_list) > 0) {
            # param list
            local$param_list <- Reduce("c", lapply(
                local$scenario_list,
                function(item) {
                    c(lapply(
                        item$param$Ne_param,
                        function(subitem) return(list(type = "N", 
                                                      name = subitem))
                    ),
                    lapply(
                        item$param$time_param,
                        function(subitem) return(list(type = "T", 
                                                      name = subitem))
                    ),
                    lapply(
                        item$param$rate_param,
                        function(subitem) return(list(type = "A", 
                                                      name = subitem))
                    ))
                }
            ))
            
            # pprint("--- param list")
            # pprint(local$param_list)
            
            # param count list
            local$param_count_list <- lapply(
                local$scenario_list,
                function(item) {
                    # pprint(item$param)
                    return(length(item$param$Ne_param) + 
                               length(item$param$time) + 
                               length(item$param$rate))
                }
            )
            
            # pprint("--- param count list")
            # pprint(local$param_count_list)
            
            # pprint("----")
            # condition
            local$cond_list <- Reduce("c", lapply(
                local$scenario_list,
                function(item) return(item$cond)
            ))
            # raw scenario
            local$raw_scenario_list <- Reduce("c", lapply(
                local$scenario_list,
                function(item) return(item$raw)
            ))
        }
    })
    
    ## parameter prior and conditon setting
    prior_cond_set <- callModule(
        prior_cond_set_server, "prior_cond",
        cond_list = reactive(local$cond_list),
        param_list = reactive(local$param_list)
    )
    
    # update param list
    observe({
        req(!is.null(prior_cond_set$raw_prior_list))
        req(!is.null(prior_cond_set$raw_cond_list))
        # pprint(prior_cond_set$raw_prior_list)
        local$raw_param_list <- unique(prior_cond_set$raw_prior_list)
        local$raw_cond_list <- prior_cond_set$raw_cond_list
    })
    
    ## locus setup
    locus_setup <- callModule(
        locus_setup_server, "locus_setup",
        data_info = reactive(local$data_info),
        locus_type = reactive(local$locus_type),
        seq_mode = reactive(local$seq_mode)
    )
    
    ## write header file (if necessary) and check it
    observeEvent(input$validate, {
        
        # pprint("proj_dir =")
        # pprint(local$proj_dir)
        # pprint("raw_param_list =")
        # pprint(local$raw_param_list)
        # pprint("param_count_list =")
        # pprint(local$param_count_list)
        # pprint("scenario_list =")
        # pprint(local$raw_scenario_list)
        # pprint("cond_list =")
        # pprint(local$raw_cond_list)
        # pprint("data_file =")
        # pprint(local$data_file)
        # pprint("locus_type =")
        # pprint(local$locus_type)
        # pprint("seq_mode =")
        # pprint(local$seq_mode)
        # pprint("locus =")
        # pprint(locus_setup$locus)
        # pprint("mss_locus =")
        # pprint(locus_setup$mss_locus)
        # pprint("mss_group_prior =")
        # pprint(locus_setup$mss_group_prior)
        # pprint("mss_rf_col_name")
        # pprint(locus_setup$mss_rf_col_name)
        
        ready <- TRUE
        msg <- list()
        
        if(length(local$raw_scenario_list) == 0) {
            ready <- FALSE
            msg <- append(
                msg,
                "Missing historical scenario."
            )
        }
        
        if(length(local$raw_param_list) == 0) {
            ready <- FALSE
            msg <- append(
                msg,
                "Missing priors."
            )
        }
        
        # if(length(local$raw_cond_list) == 0) {
        #     ready <- FALSE
        #     msg <- append(
        #         msg,
        #         "Missing conditions."
        #     )
        # }
        
        output$feedback <- renderUI({
            if(length(msg) > 0) {
                tag_list <- lapply(
                    msg,
                    function(item) return(tags$li(item))
                )
                helpText(
                    icon("warning"), "Settings are not ready.",
                    do.call(
                        tags$ul,
                        tag_list
                    )
                )
            } else {
                NULL
            }
        })
        
        if(ready) {
            req(!is.null(local$proj_dir))
            req(!is.null(local$data_file))
            req(!is.null(local$raw_scenario_list))
            req(!is.null(local$param_count_list))
            req(!is.null(local$raw_param_list))
            req(!is.null(local$raw_cond_list))
            req(!is.null(local$locus_type))
            req(!is.null(local$seq_mode))
            req(!is.null(locus_setup$locus))
            
            if(local$locus_type == "mss") {
                req(!is.null(locus_setup$mss_locus))
                req(!is.null(locus_setup$mss_group_prior))
                req(!is.null(locus_setup$mss_rf_col_name))
            }
            
            write_header(local$proj_dir, local$data_file, 
                         local$raw_scenario_list, local$param_count_list, 
                         unlist(local$raw_param_list), 
                         unlist(local$raw_cond_list), 
                         local$locus_type, local$seq_mode, locus_setup$locus,
                         locus_setup$mss_locus, locus_setup$mss_group_prior,
                         locus_setup$mss_rf_col_name)
            
            file_check <- parse_diyabc_header(
                file_name = file.path(local$proj_dir, "header.txt"),
                file_type = "text/plain",
                locus_type = local$locus_type
            )
            
            # # debugging
            # pprint("is header ok")
            # pprint(file_check$valid)
            
            out$valid_def <- file_check$valid
            
            if(out$valid_def) {
                showNotification(
                    id = ns("headerfile_ok"),
                    duration = 5,
                    closeButton = TRUE,
                    type = "message",
                    tagList(
                        tags$p(
                            icon("check"),
                            paste0("Project is ready to run simulations.")
                        )
                    )
                )
            } else {
                showNotification(
                    id = ns("headerfile_ok"),
                    duration = 5,
                    closeButton = TRUE,
                    type = "error",
                    tagList(
                        tags$p(
                            icon("warning"),
                            paste0("Project is not ready to run simulations.")
                        )
                    )
                )
            }
            
            # out$trigger <- ifelse(!is.null(out$trigger), out$trigger, 0) + 1
        }
    })
    

    # get output
    observe({
        out$cond_list = local$cond_list
        out$param_list = local$param_list
        out$raw_param_list = local$raw_param_list
    })
    ## output
    return(out)
}

#' Historical model input panel module ui
#' @keywords internal
#' @author Ghislain Durif
hist_model_panel_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("history"), "Define historical models"),
        br(),
        fluidRow(
            column(
                width = 2,
                actionButton(
                    ns("add"), 
                    label = tags$span(icon("plus"), "Add")
                )
            ),
            column(
                width = 4,
                uiOutput(ns("scenario_nb"))
            )
        ),
        br(),
        tabsetPanel(id = ns("scenario_tabs"))
    )
}

#' Historical model input panel module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyjs onclick
#' @importFrom shinyWidgets actionBttn
#' @importFrom stringr str_c
hist_model_panel_server <- function(input, output, session,
                                    project_dir = reactive({NULL}), 
                                    raw_scenario_list = reactive({NULL})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        count = NULL,
        project_dir = NULL,
        raw_scenario_list = NULL,
        scenario_list = list(),
        scenario_id = list(),
        scenario_nb = 0
    )
    # get input
    observe({
        local$project_dir <- project_dir()
        # FIXME raw_scenario_list
    })
    # init output
    out <- reactiveValues(scenario_list = NULL)
    
    # add scenario
    observeEvent(input$add, {
        # increment count
        local$count <- ifelse(is.null(local$count), 0, local$count) + 1
        local$scenario_nb <- ifelse(is.null(local$scenario_nb), 
                                    0, local$scenario_nb) + 1
        # id
        id <- local$count
        local$scenario_id[[ as.character(id) ]] <<- id
        # add new tab
        appendTab(
            inputId = "scenario_tabs",
            tabPanel(
                title = closable_tab_title(id, 
                                           label = "scenario",
                                           ns = ns),
                value = id,
                tags$br(),
                hist_model_ui(ns(str_c("model", id)))
            ),
            select = TRUE
        )
        # observe closing
        observe({
            shinyjs::onclick(id = str_c("close", id), {
                local$scenario_nb <<- ifelse(!is.null(local$scenario_nb), 
                                             local$scenario_nb - 1, 0)
                removeTab(inputId = "scenario_tabs", target = str_c(id))
                local$scenario_list[[ id ]] <<- NULL
                local$scenario_id[[ as.character(id) ]] <<- NULL
            })
        })
        # server function
        local$scenario_list[[ id ]] <<- callModule(
            hist_model_server, str_c("model", id),
            project_dir = reactive(local$project_dir), 
            raw_scenario = reactive(local$raw_scenario)
        )
    })
    
    # update scenario number
    output$scenario_nb <- renderUI({
        helpText(
            str_c("Current number of scenarii = ", local$scenario_nb)
        )
    })
    
    # update scenario title
    observe({
        req(local$scenario_nb>0)
        lapply(
            1:local$scenario_nb,
            function(ind) {
                id <- local$scenario_id[[ind]]
                output[[ str_c("scenario_title", id) ]] <- renderText(
                    str_c("Scenario ", ind)
                )
            }
        )
    })
    
    # update output
    observe({
        out$scenario_list <- local$scenario_list
        # if(length(local$scenario_list) > 0) {
        #     pprint(local$scenario_list[[1]]$raw)
        #     pprint(local$scenario_list[[1]]$param)
        # }
    })
    
    ## output
    return(out)
}

#' Return closable panel title
#' @keywords internal
#' @author Ghislain Durif
closable_tab_title <- function(id, label, ns) {
    tags$span(
        textOutput(ns(str_c("scenario_title", id)), inline = TRUE),
        HTML("&nbsp;"),
        tags$span(
            actionButton(
                ns(str_c("close", id)), 
                label = NULL,
                icon = icon("close")
            )
        )
    )
}

#' Parameter prior and condition setting module ui
#' @keywords internal
#' @author Ghislain Durif
prior_cond_set_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("chart-bar"), "Priors and conditions"),
        uiOutput(ns("param_prior_def")),
        hr(),
        h4("Condition setting"),
        textAreaInput(
            ns("cond_set"), 
            label = NULL, 
            rows = 4,
            resize = "none"
        ),
        helpText(
            icon("warning"),
            "You might need to impose some conditions on historical parameters",
            "(e.g. to avoid genealogical problems)",
            "or to constraint simulation settings.",
            br(), br(),
            "For instance, there can be two time parameters with overlapping",
            "prior distributions. However, you might want",
            "the first one, say t1, to always be larger than",
            "the second one, say t2.",
            br(), br(),
            "To do so, you just need to set 't1 > t2' in the 'Condition",
            "setting' panel above. Such a condition should concern",
            "two parameters of the same type",
            "(i.e. two effective sizes, two times or two admixture rates).",
            br(), br(),
            "Enter a single condition per line.",
            "Conditions should have the following format: 'XX<YY'", 
            "(without the quotes),", 
            "where 'XX' and 'YY' are parameters of the same type.",
            "You can use the standard comparison signs: '>', '>=', '<', '=<'."
        ),
        uiOutput(ns("cond_format"))
    )
}

#' Parameter prior and condition setting module server
#' @keywords internal
#' @author Ghislain Durif
#' @param cond_list list of conditions on parameters, as a `reactive`.
#' @param param_list list of scenario parameters, as a `reactive`.
#' @importFrom stringr str_c
prior_cond_set_server <- function(input, output, session, 
                                  cond_list = reactive({list()}),
                                  param_list = reactive({list()})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        cond_list = NULL,
        cond_check = NULL,
        param_list = NULL,
        prior_list = NULL
    )
    # get input
    observe({
        local$cond_list <- cond_list()
        local$param_list <- unique(param_list())
    })
    # init output
    out <- reactiveValues(
        raw_prior_list = list(),
        raw_cond_list = list()
    )
    
    # # debugging
    # observe({
    #     pprint("param list")
    #     pprint(unique(local$param_list))
    # })
    
    # render ui param prior setting
    observeEvent(local$param_list, {
        output$param_prior_def <- renderUI({
            tag_list <- lapply(
                local$param_list,
                function(item) return(prior_ui(ns(str_c("prior_", item$name))))
            )
            do.call(tagList, tag_list)
        })
    })
    # server side
    observe({
        local$prior_list <<- lapply(
            local$param_list,
            function(item) {
                req(item$name)
                req(item$type)
                callModule(prior_server, str_c("prior_", item$name),
                           param_name = reactive({item$name}),
                           param_type = reactive({item$type}))
            }
        )
    })
    observe({
        out$raw_prior_list <- unlist(lapply(
            local$prior_list,
            function(item) return(item$raw)
        ))
        # pprint(out$raw_prior_list)
    })
    # get input condition
    observeEvent(input$cond_set, {
        req(!is.null(input$cond_set))
        req(!is.null(local$param_list))
        # FIXME
        # input (remove last empty line)
        # logging("input cond set = ", input$cond_set)
        if(str_length(input$cond_set) > 0) {
            input_cond_list <- str_split(
                str_replace(
                    string = input$cond_set,
                    pattern = "\\n$",
                    replacement = ""
                ), 
                pattern = "\\n"
            )
            # check
            local$cond_check <- check_cond(input_cond_list, local$param_list)
            # output possible issues
            output$cond_format <- renderUI({
                req(!is.null(local$cond_check$valid))
                req(!is.null(local$cond_check$msg))
                # pprint(local$cond_check)
                if(!local$cond_check$valid) {
                    helpText(
                        tags$span(icon("warning"), 
                                  "Issues with your input conditions:"),
                        do.call(
                            tags$ul,
                            lapply(local$cond_check$msg, function(item) {
                                return(tags$li(item))
                            })
                        )
                    )
                } else {
                    tagList()
                }
            })
            # output
            if(local$cond_check$valid) {
                out$raw_cond_list <- input_cond_list
            } else {
                out$raw_cond_list <- list()
            }
        } else {
            out$raw_cond_list <- list()
        }
    })
    
    ## output
    return(out)
}

#' Check condition provided by users
#' @keywords internal
#' @author Ghislain Durif
check_cond <- function(input_cond_list, param_list) {
    # init output
    msg <- NULL
    valid <- TRUE
    # check if input
    if(length(input_cond_list) > 0 & length(param_list) > 0) {
        # check condition formatting
        format_check <- str_detect(
            string = unlist(input_cond_list),
            pattern = str_c("^", single_param_regex(), "(<|=<|>|>=)",
                            single_param_regex(),  "$")
        )
        if(!all(format_check)) {
            msg <- append(
                msg, 
                str_c(
                    "Syntax issue with input conditions of following line(s):", 
                     str_c(which(!format_check), collapse = " "), 
                    sep = " "
                )
            )
            valid <- FALSE
        }
        # concerned parameters
        input_param <- str_extract_all(
            string = unlist(input_cond_list),
            pattern = single_param_regex()
        )
        # check for duplicate
        # FIXME
        if(length(unique(input_param)) < length(input_param)) {
            logging("check for duplicate")
            msg <- append(
                msg,
                "Possible duplicated conditions."
            )
            valid <- FALSE
        }
        # check for parameter validity
        # FIXME
    }
    # output
    return(lst(msg, valid))
}

#' Prior choice module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyWidgets radioGroupButtons
prior_ui <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h4(textOutput(
            ns("param_name")
        )),
        fluidRow(
            column(
                width = 6,
                radioGroupButtons(
                    ns("prior_type"),
                    label = NULL,
                    choices = list("Uniform" = "UN", "Log-Unif." = "LU",
                                   "Normal" = "NO", "Log-Norm." = "LN"),
                    selected = "UN",
                    justified = TRUE
                ),
            ),
            column(
                width = 6,
                fluidRow(
                    column(
                        width = 6,
                        splitLayout(
                            tags$h5(
                                "Min.", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            numericInput(
                                ns("min"), label = NULL,
                                value = 10, step = 1, min = 0
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    ),
                    column(
                        width = 6,
                        splitLayout(
                            tags$h5(
                                "Max.", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            numericInput(
                                ns("max"), label = NULL,
                                value = 10000, step = 1, min = 0
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    )
                ),
                fluidRow(
                    column(
                        width = 6,
                        splitLayout(
                            tags$h5(
                                "Mean", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            numericInput(
                                ns("mean"), label = NULL,
                                value = 0, step = 0.01, min = 0
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    ),
                    column(
                        width = 6,
                        splitLayout(
                            tags$h5(
                                "Std. dev.", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            numericInput(
                                ns("stdev"), label = NULL,
                                value = 0, step = 0.01, min = 0
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    )
                )
            )
        )
    )
}

#' Prior choice module server
#' @keywords internal
#' @author Ghislain Durif
#' @param param_name parameter name as a `reactive`.
#' @param param_type parameter type as a `reactive`, `'N'` for population 
#' effective size, `'T'` for time, and `'A'` for admixture rate.
#' @importFrom shinyjs disable enable
#' @importFrom stringr str_c
prior_server <- function(input, output, session, 
                         param_name = reactive({NULL}),
                         param_type = reactive({NULL})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(param_name = NULL, param_type = NULL)
    # get input
    observe({
        local$param_name <- param_name()
        local$param_type <- param_type()
    })
    # init output
    out <- reactiveValues(raw = NULL, valid = TRUE)
    # update param name output
    output$param_name <- renderText({local$param_name})
    ## check for min/max
    observe({
        req(local$param_name)
        req(input$min)
        req(input$max)
        if(input$min >= input$max) {
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
    ## disable mean and stdev if uniform or log-uniform
    observeEvent(input$prior_type, {
        req(input$prior_type)
        if(input$prior_type %in% c("UN", "LU")) {
            updateNumericInput(session, "mean", value = 0)
            updateNumericInput(session, "stdev", value = 0)
            shinyjs::disable("mean")
            shinyjs::disable("stdev")
        } else {
            shinyjs::enable("mean")
            shinyjs::enable("stdev")
        }
    })
    ## check for normal and log-normal parameter setting
    observe({
        req(local$param_name)
        req(input$prior_type)
        req(is.numeric(input$min))
        req(is.numeric(input$max))
        req(is.numeric(input$mean))
        if(input$prior_type %in% c("NO", "LN")) {
            if(input$mean < input$min | input$mean > input$max) {
                out$valid <- FALSE
                showNotification(
                    id = ns("issue_norm_lognorm"),
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
    ## deal with admixture rate
    observeEvent(local$param_type, {
        req(local$param_type)
        if(local$param_type == "A") {
            updateNumericInput(session, "min", value = 0.05, 
                               step = 0.01, min = 0, max = 1)
            updateNumericInput(session, "max", value = 0.95, 
                               min = 0, max = 1)
        } else {
            updateNumericInput(session, "min", value = 10, 
                               step = 1, min = NULL, max = NULL)
            updateNumericInput(session, "max", value = 10000, 
                               step = 1, min = NULL, max = NULL)
        }
    })
    # observe({
    #     logging("min = ", input$min)
    #     logging("max = ", input$max)
    #     logging("mean = ", input$mean)
    #     logging("stdev = ", input$stdev)
    # })
    ## encode output
    observe({
        req(local$param_name)
        req(local$param_type)
        req(input$prior_type)
        req(is.numeric(input$min))
        req(is.numeric(input$max))
        req(is.numeric(input$mean))
        req(is.numeric(input$stdev))
        out$raw <- str_c(local$param_name, " ",
                         local$param_type, " ",
                         input$prior_type, "[",
                         input$min, ",", input$max, ",",
                         input$mean, ",", input$stdev, "]")
        # logging("raw prior def = ", out$raw)
    })
    ## output
    return(out)
}

#' Locus setup ui
#' @keywords internal
#' @author Ghislain Durif
locus_setup_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("dna"), "Number of loci to simulate"),
        uiOutput(ns("setup")),
        uiOutput(ns("mss_setup"))
    )
}

#' Locus setup server
#' @keywords internal
#' @author Ghislain Durif
locus_setup_server <- function(input, output, session, 
                               data_info = reactive({NULL}),
                               locus_type = reactive({NULL}),
                               seq_mode = reactive({NULL})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        data_info = NULL,
        locus_type = NULL,
        seq_mode = NULL
    )
    # get input
    observe({
        local$data_info <- data_info()
        local$locus_type <- locus_type()
        local$seq_mode <- seq_mode()
    })
    
    # # debugging
    # observe({
    #     pprint("data info")
    #     pprint(local$data_info)
    #     pprint("locus type")
    #     pprint(local$locus_type)
    #     pprint("seq mode")
    #     pprint(local$seq_mode)
    # })
    
    # init out
    out <- reactiveValues(
        locus = NULL,
        mss_locus = NULL,
        mss_group_prior = NULL,
        mss_rf_col_name = NULL
    )
    # render ui
    output$setup <- renderUI({
        
        # # debugging
        # pprint("locus_type")
        # pprint(local$locus_type)
        # pprint("locus")
        # pprint(local$data_info$locus_type)
        
        req(local$locus_type)
        req(local$seq_mode)
        req(!is.null(local$data_info$locus))
        
        if(local$locus_type == "snp") {
            tag_list <- lapply(
                local$data_info$locus,
                function(item) {
                    return(
                        fluidRow(
                            column(
                                width = 4,
                                shinyjs::disabled(textInput(
                                    ns(str_c("type_", item)),
                                    label = "SNP loci available",
                                    value = item
                                ))
                            ),
                            column(
                                width = 4,
                                numericInput(
                                    inputId = ns(str_c(
                                        "num_",
                                        str_extract(item,
                                                    pattern = "(A|H|X|Y|M)")
                                    )),
                                    label = "Number of locus",
                                    min = 0,
                                    max = as.numeric(
                                        str_extract(item,
                                                    pattern = "^[0-9]+")
                                    ),
                                    value = as.numeric(
                                        str_extract(item,
                                                    pattern = "^[0-9]+")
                                    )
                                )
                            ),
                            column(
                                width = 4,
                                numericInput(
                                    inputId = ns(str_c(
                                        str_extract(item,
                                                    pattern = "(A|H|X|Y|M)"),
                                        "_from"
                                    )),
                                    label = "from",
                                    min = 1,
                                    max = as.numeric(
                                        str_extract(item,
                                                    pattern = "^[0-9]+")
                                    ),
                                    value = 1
                                )
                            )
                        )
                    )
                }
            )
            do.call(tagList, tag_list)
        } else if(local$locus_type == "mss") {
            # FIXME
            
            # pprint("data info")
            # pprint(local$data_info)
            # warning("not supported at the moment")
            
            # microsat locus
            microsat_locus <- str_detect(local$data_info$locus_mode, "microsat")
            microsat_locus_type <- table(local$data_info$locus[microsat_locus])
            
            # seq locus
            seq_locus <- str_detect(local$data_info$locus_mode, "seq")
            seq_locus_type <- table(local$data_info$locus[seq_locus])
            
            tagList(
                tags$ul(
                    tags$li(
                        "Number of Microsat loci:", 
                        as.character(sum(microsat_locus))
                    ),
                    tags$li(
                        "Number of Sequence loci:", 
                        as.character(sum(seq_locus))
                    )
                )
            )
        } else {
            NULL
        }
    })
    
    ## MSS setup
    output$mss_setup <- renderUI({
        if(local$locus_type == "mss") {
            tagList(
                mss_group_setup_ui(ns("mss_group")),
                br(),
                mss_group_prior_ui(ns("mss_prior"))
            )
        } else {
            NULL
        }
    })
    
    mss_group <- callModule(
        mss_group_setup_server,
        "mss_group",
        data_info = reactive(local$data_info)
    )
    
    mss_prior <- callModule(
        mss_group_prior_server,
        "mss_prior",
        group_info = reactive(mss_group$group_info)
    )
    
    ## update output
    observe({
        if(local$locus_type == "snp") {
            out$locus <- lapply(
                local$data_info$locus,
                function(item) {
                    locus <- str_extract(item, pattern = "(A|H|X|Y|M)")
                    n_loci <- input[[ str_c("num_", locus) ]]
                    start_loci <- input[[ str_c(locus, "_from") ]]
                    return(str_c(
                        n_loci,
                        str_c("<", locus, ">"),
                        "G1",
                        "from", start_loci,
                        sep = " "
                    ))
                }
            )
            # # debugging
            # pprint("locus setup")
            # pprint(out$locus)
        } else if(local$locus_type == "mss") {
            req(!is.null(mss_group$raw_locus))
            req(!is.null(mss_prior$raw_group_prior_list))
            out$mss_locus <- mss_group$raw_locus
            out$mss_group_prior <- mss_prior$raw_group_prior_list
            out$mss_rf_col_name <- mss_prior$rf_col_name
        }
    })
    
    ## output
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
            "By default, all Microsat loci are supposed to be dinucleid",
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

#' Training set simulation action module ui
#' @keywords internal
#' @author Ghislain Durif
training_set_action_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("list-ol"), "Summary statistics"),
        helpText(
            "All summary statistics implemented in the program will be used."
        ) %>% 
            helper(type = "markdown", 
                   content = "summary_stats"),
        hr(),
        h3(icon("gear"), "Run"),
        numericInput(
            ns("nrun"),
            label = "Number of simulations requested in the training set",
            value = 100,
            min = 10
        ),
        uiOutput(ns("feedback_nrun")),
        helpText(
            tags$div(
                tags$b("Note:"), br(),
                "The number of simulations depends on your analysis:"
            ),
            tags$ul(
                tags$li(
                    "1000 to 20000 simulations per scenario are needed",
                    "for" , tags$b("model choice"), "."
                ),
                tags$li(
                    "1000 to 100000 simulations under the scenario",
                    "of interest are needed for",
                    tags$b("parameter estimation"), "."
                )
            )
        ),
        hr(),
        actionBttn(
            inputId = ns("simulate"),
            label = "Simulate",
            style = "fill",
            block = TRUE,
            color = "primary"
        ),
        progressBar(
            id = ns("simu_progress"),
            value = 0,
            total = 100,
            title = "",
            display_pct = TRUE
        ),
        uiOutput(ns("feedback")),
        br(),
        actionBttn(
            inputId = ns("stop"),
            label = "Stop",
            style = "fill",
            block = TRUE,
            color = "danger"
        ),
        br(),
        h5(icon("comment"), "Run logs"),
        tags$pre(
            uiOutput(ns("run_log")),
            style = "width:60vw; overflow:scroll; overflow-y:scroll; height:100px; resize: both;"
        ),
        hr(),
        h4("Prior and scenario checking"),
        helpText(
            "This action requires a training set file",
            tags$code("reftableRF.bin"),
            "either generated when clicking on 'Simulate'",
            "or uploaded with an existing project."
        ),
        actionGroupButtons(
            inputIds = c(ns("run_prior_mod_check"), ns("stop_prior_mod_check")),
            labels = list(
                tags$span(icon("play"), "Run"),
                tags$span(icon("stop"), "Stop")
            ),
            fullwidth = TRUE
        ),
        uiOutput(ns("feedback_prior_mod_check"))
    )
}

#' Training set simulation action module server
#' @keywords internal
#' @author Ghislain Durif
training_set_action_server <- function(input, output, session,
                                       proj_dir = reactive({NULL}),
                                       proj_file_list = reactive({NULL}),
                                       valid_def = reactive({FALSE}),
                                       valid_proj = reactive({FALSE})) {
    # namespace
    ns <- session$ns
    
    # max number of rows in log
    nlog <- 5
    
    # init local
    local <- reactiveValues(
        diyabc_run_process = NULL,
        diyabc_run_result = NULL,
        feedback = NULL,
        log_file_content = NULL,
        log_start_line = NULL,
        n_rec_initial = 0,
        n_rec_final = 0,
        n_scenario = 0,
        n_stat = 0,
        prior_check_process = NULL,
        prior_check_result = NULL,
        run_diyabc = 0,
        # input
        proj_dir = NULL,
        proj_file_list = NULL,
        valid_def = NULL,
        valid_proj = NULL
    )
    # get input
    observe({
        local$proj_dir <- proj_dir()
        local$proj_file_list <- proj_file_list()
        local$valid_def <- valid_def()
        local$valid_proj <- valid_proj()
    })
    
    # # debugging
    # observe({
    #     pprint("Training set action input")
    #     pprint(local$proj_dir)
    #     pprint(local$valid_proj)
    #     pprint(local$valid_def)
    # })
    
    # # debugging
    # observe({
    #     pprint("=== training set simu run: project directory")
    #     pprint(local$proj_dir)
    # })
    
    # init output
    out <- reactiveValues(n_rec = 0, n_stat = 0, n_scenario = 0)
    
    ## read log file
    log_file_content <- function() return(rep("", nlog))
    observeEvent(local$proj_dir, {
        req(local$proj_dir)
        log_file_content <<- reactiveFileReader(
            1000, session,
            file.path(local$proj_dir, "diyabc_run_call.log"),
            function(file) {
                if(file.exists(file)) {
                    readLines(file, warn=FALSE)
                } else {
                    character(0)
                }
            }
        )
    })
    
    observe({
        local$log_file_content <- log_file_content()
    })
    
    ## run simulation
    observeEvent(input$simulate, {
        
        req(!is.null(local$valid_proj))
        req(!is.null(local$valid_def))
        req(!is.null(local$proj_file_list))
        req(length(local$proj_file_list) > 0)
        
        # # debugging
        # pprint("simulate")
        # pprint("valid proj ?")
        # pprint(local$valid_proj)
        # pprint("valid def ?")
        # pprint(local$valid_def)
        # pprint("proj file list")
        # pprint(local$proj_file_list)
        
        req(input$nrun)
        req(local$proj_dir)
        
        ## run in progress
        if(!is.null(local$diyabc_run_process)) {
            showNotification(
                id = ns("run_in_progress"),
                duration = 5,
                closeButton = TRUE,
                type = "warning",
                tagList(
                    tags$p(
                        icon("warning"),
                        "Run in progress."
                    )
                )
            )
        } else {
            ## prepare run
            req(is.null(local$diyabc_run_process))
            
            ## init progress bar
            updateProgressBar(
                session = session,
                id = "simu_progress",
                value = 0, total = input$nrun,
                title = "Running simulation:"
            )
            
            ## check if possible to run
            if(!local$valid_proj) {
                local$feedback <- helpText(
                    icon("warning"), "Project is not ready.",
                    "Check project settings."
                )
            } else if(!local$valid_def) {
                local$feedback <- helpText(
                    icon("warning"), "Simulation configuration is not ready.",
                    "Check training simulation settings."
                )
            } else if(!any(c("header.txt", "headerRF.txt") %in% 
                           list.files(local$proj_dir))) {
                local$feedback <- helpText(
                    icon("warning"), 
                    "You must configure your own project", 
                    "(historical scenario, parameter priors,", 
                    "conditions, etc.),",
                    "or upload a header file",
                    "(", 
                    tags$code("headerRF.txt"),
                    ")",
                    "from existing project."
                )
                showNotification(
                    id = ns("missing_files"),
                    duration = 5,
                    closeButton = TRUE,
                    type = "warning",
                    tagList(
                        tags$p(
                            icon("warning"), 
                            "Upload header file from existing project,",
                            "or configure your own project", 
                            "(historical scenario, parameter priors,", 
                            "conditions, etc.)."
                        )
                    )
                )
            } else {
                ## ready to run
                
                # debugging
                # pprint("check options")
                # pprint(getOption("diyabcGUI"))
                # pprint(getOption("shiny.maxRequestSize"))
                
                ## reset log
                local$log_start_line <- NULL
                
                local$feedback <- helpText(
                    icon("spinner", class = "fa-spin"),
                    "Simulations are running."
                )
                
                logging("Running simulation")
                local$diyabc_run_process <- diyabc_run_trainset_simu(
                    local$proj_dir, 
                    as.integer(input$nrun),
                    run_prior_check = FALSE
                )
            }
        }
    })
    
    ## monitor simulation run
    observeEvent(local$diyabc_run_process, {
        req(!is.null(local$diyabc_run_process))
        
        pprint("diyabc run process")
        pprint(local$diyabc_run_process)
        
        observe({
            req(!is.null(local$diyabc_run_process))
            proc <- local$diyabc_run_process
            req(!is.null(proc$is_alive()))
            if(proc$is_alive()) {
                invalidateLater(2000, session)
            } else {
                local$diyabc_run_result <- proc$get_exit_status()
            }
        })
    })
    
    ## clean up after run
    observeEvent(local$diyabc_run_result, {
        
        req(!is.null(local$diyabc_run_result))
        
        logging("diyabc simu run exit status:",
                local$diyabc_run_result)
        
        ## log
        output$run_log <- renderUI({
            req(!is.null(local$log_file_content))
            do.call(
                tagList,
                as.list(local$log_file_content)
            )
        })
        
        ## check run
        # run ok
        if(local$diyabc_run_result == 0) {
            local$run_diyabc <- local$run_diyabc + 1
            local$feedback <- helpText(
                icon("check"), "Run succeeded."
            )
            updateProgressBar(
                session = session,
                id = "simu_progress",
                value = input$nrun, total = input$nrun,
                title = "Running simulation:"
            )
            showNotification(
                id = ns("run_ok"),
                duration = 5,
                closeButton = TRUE,
                type = "message",
                tagList(
                    tags$p(
                        icon("check"),
                        "Simulations are done."
                    )
                )
            )
        } else if(local$diyabc_run_result == -1000) {
            ## stopped run
            local$feedback <- helpText(
                icon("warning"), "Simulation run was stopped."
            )
            showNotification(
                id = ns("stop_run"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"),
                        "Simulation run was stopped."
                    )
                )
            )
        } else if(local$diyabc_run_result == -2000) {
            ## stopped run
            local$feedback <- helpText(
                icon("warning"), 
                "Error in simulation process:", 
                "check your scenarios, priors and conditions."
            )
            showNotification(
                id = ns("stop_run"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"),
                        "Error in simulation process:", 
                        "check your scenarios, priors and conditions."
                    )
                )
            )
        } else {
            ## error during run
            local$feedback <- helpText(
                icon("warning"), "Issues with run (see log panel below)."
            )
            showNotification(
                id = ns("run_not_ok"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"),
                        "A problem happened during simulations."
                    )
                )
            )
        }
        
        # reset
        local$diyabc_run_result <- NULL
        local$diyabc_run_process <- NULL
    })
    
    ## stop run
    observeEvent(input$stop, {
        ## if no current run
        if(is.null(local$diyabc_run_process)) {
            local$feedback <- helpText(
                icon("warning"), "No current run to stop."
            )
            showNotification(
                id = ns("no_run"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"), "No current run to stop."
                    )
                )
            )
        } else {
            ## stop current run
            proc <- local$diyabc_run_process
            if(proc$is_alive()) {
                proc$kill()
            }
            local$diyabc_run_result <- -1000
            
            local$n_ref_final <- local$n_rec_initial
        }
    })
    
    ## progress bar
    observeEvent(local$log_file_content, {
        
        # pprint("----------- monitor progress -----------")
        # logging("objective nrun =", input$nrun)
        # logging("current log length =", length(local$log_file_content))
        
        req(input$nrun)
        
        req(!is.null(local$log_file_content))
        req(length(local$log_file_content) > 0)
        
        ## parse log
        if(is.null(local$log_start_line)) {
            pttrn <- str_c(
                "DEBUT +",
                "nrecneeded=[0-9]+ +",
                "rt\\.nrec=[0-9]+ +", 
                "rt\\.nstat=[0-9]+ +", 
                "nscenarios=[0-9]+ *"
            )
            
            find_pttrn <- str_detect(local$log_file_content, pttrn)
            # logging("found pattern =", sum(find_pttrn))
            
            if(any(find_pttrn)) {
                
                local$log_start_line <- head(which(find_pttrn), 1)
                pttrn_match <- local$log_file_content[local$log_start_line]
                # pprint("pattern match")
                # pprint(pttrn_match)
                # pprint("log_start_line")
                # pprint(local$log_start_line)
                
                local$n_rec_initial <- as.integer(str_extract(
                    pttrn_match,
                    "(?<=rt\\.nrec=)[0-9]+"
                ))
                # pprint("n_rec_initial")
                # pprint(local$n_rec_initial)
                
                local$n_rec_final <- as.integer(str_extract(
                    pttrn_match,
                    "(?<=nrecneeded=)[0-9]+"
                ))
                # pprint("n_rec_final")
                # pprint(local$n_rec_final)
                
                local$n_scenario <- as.integer(str_extract(
                    pttrn_match,
                    "(?<=nscenarios=)[0-9]+"
                ))
                # pprint("n_scenario")
                # pprint(local$n_scenario)
                
                local$n_stat <- as.integer(str_extract(
                    pttrn_match,
                    "(?<=rt\\.nstat=)[0-9]+"
                ))
                # pprint("n_stat")
                # pprint(local$n_stat)
                
                ## update progress bar
                if(input$nrun > local$n_rec_initial) {
                    updateProgressBar(
                        session = session,
                        id = "simu_progress",
                        value = local$n_rec_initial, 
                        total = input$nrun,
                        title = "Running simulation:"
                    )
                }
            }
        } else {
            last_message <- tail(
                local$log_file_content,
                length(local$log_file_content) - local$log_start_line
            )
            find_iter <- str_detect(last_message, "^[0-9]+$")
            
            if(any(find_iter)) {
                current_iter <- tail(last_message[find_iter], 1)
                # logging("Iteration:", 
                #         str(current_iter, "/", local$n_rec_final))
                
                updateProgressBar(
                    session = session,
                    id = "simu_progress",
                    value = as.numeric(current_iter), 
                    total = input$nrun,
                    title = "Running simulation:"
                )
            }
            
            ## check for infinite loop
            if(!is.null(local$diyabc_run_process)) {
                if(sum(str_detect(last_message, "^locus=0")) > 2) {
                    ## stop current run
                    proc <- local$diyabc_run_process
                    if(proc$is_alive()) {
                        proc$kill()
                    }
                    local$diyabc_run_result <- -2000
                    
                    local$n_ref_final <- local$n_rec_initial
                }
            }
        }
    })
    
    # ## debugging
    # observe({
    #     logging("log_start_line =", local$log_start_line)
    #     logging("n_rec_initial =", local$n_rec_initial)
    #     logging("n_rec_final =", local$n_rec_final)
    #     logging("n_scenario =", local$n_scenario)
    #     logging("n_stat =", local$n_stat)
    # })

    ## feedback
    observeEvent(local$feedback, {
        output$feedback <- renderUI({
            local$feedback
        })
    })
    
    ## feedback nrun
    output$feedback_nrun <- renderUI({
        tmp_text <- helpText(
            "Number of already available simulations = ",
            tags$i("unknown"),
            br(), br(),
            "To generate additional training data, you must set",
            "the number of simulations to be higher than",
            tags$i("unknown"), "."
        )
        
        if(!is.null(local$run_diyabc)) {
            if(local$run_diyabc > 0) {
                if(!is.null(local$n_rec_initial) & !is.null(local$n_rec_final)) {
                    
                    reftable_size <- max(local$n_rec_initial, local$n_rec_final)
                    
                    tmp_text <- helpText(
                        "Number of already available simulations = ",
                        tags$b(reftable_size),
                        br(), br(),
                        "To generate additional training data, you must set",
                        "the number of simulations to be higher than",
                        tags$b(reftable_size), "."
                    )
                    
                    if(is.numeric(input$nrun)) {
                        if(reftable_size >= input$nrun) {
                            tmp_text <- helpText(
                                icon("warning"), 
                                "Number of already available simulations = ",
                                tags$b(reftable_size),
                                br(), br(),
                                icon("warning"),
                                "To generate additional training data, you must set",
                                "the number of simulations to be higher than",
                                tags$b(reftable_size), "."
                            )
                        }
                    }
                }
            }
        }
        
        tmp_text
    })
    
    ## prior/model checking
    # run
    observeEvent(input$run_prior_mod_check, {
        req(!is.null(local$proj_dir))
        req(!is.null(local$proj_file_list))
        req("reftableRF.bin" %in% local$proj_file_list)

        if(!is.null(local$prior_check_process)) {
            showNotification(
                id = ns("prior_mod_check_in_progress"),
                duration = 5,
                closeButton = TRUE,
                type = "warning",
                tagList(
                    tags$p(
                        icon("warning"),
                        "Prior and scenario checking in progress."
                    )
                )
            )
        } else {
            local$prior_check_result <- NULL
            logging("Running prior and scenario checking")
            local$prior_check_process <- diyabc_run_trainset_simu(
                local$proj_dir,
                n_run = 0,
                run_prior_check = TRUE
            )
        }
    })

    # monitor run
    observeEvent(local$prior_check_process, {
        req(!is.null(local$prior_check_process))

        pprint("diyabc prior/model check process")
        pprint(local$prior_check_process)

        observe({
            req(!is.null(local$prior_check_process))
            proc <- local$prior_check_process
            req(!is.null(proc$is_alive()))
            if(proc$is_alive()) {
                invalidateLater(1000, session)
            } else {
                local$prior_check_result <- proc$get_exit_status()
            }
        })
    })

    # stop
    observeEvent(input$stop_prior_mod_check, {
        ## if no current run
        if(is.null(local$prior_check_process)) {
            showNotification(
                id = ns("prior_mod_no_run"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"), "No current run to stop."
                    )
                )
            )
        } else {
            ## stop current run
            proc <- local$prior_check_process
            proc$kill()
            local$prior_check_result <- -1000

            # reset
            local$prior_check_process <- NULL
        }
    })

    # result and clean up after run
    observeEvent(local$prior_check_result, {

        req(!is.null(local$prior_check_result))

        logging("diyabc prior/model check run exit status:",
                local$prior_check_result)

        ## check run
        # run ok
        if(local$prior_check_result == 0) {
            showNotification(
                id = ns("prior_mod_check_run_ok"),
                duration = 5,
                closeButton = TRUE,
                type = "message",
                tagList(
                    tags$p(
                        icon("check"),
                        "Prior and scenario checking is done."
                    )
                )
            )
        } else if(local$prior_check_result == -1000) {
            showNotification(
                id = ns("prior_mod_check_stop_run"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"),
                        "Prior and scenario checking was stopped."
                    )
                )
            )
        } else {
            showNotification(
                id = ns("prior_mod_check_run_not_ok"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"),
                        "A problem happened during prior and scenario checking."
                    )
                )
            )
        }

        # reset
        local$prior_check_process <- NULL
    })


    # feedback
    output$feedback_prior_mod_check <- renderUI({
        req(length(local$proj_file_list) > 0)
        
        if(!"reftableRF.bin" %in% local$proj_file_list) {
            helpText(
                icon("warning"), "No training set available."
            )
        } else if(!is.null(local$prior_check_process)) {
            helpText(
                icon("spinner", class = "fa-spin"),
                "Prior and scenario checking is running."
            )
        } else if(!is.null(local$prior_check_result)) {
            if(local$prior_check_result == -1000) {
                helpText(
                    icon("warning"), "Prior and scenario checking was stopped."
                )
            } else if(local$prior_check_result == 0) {
                helpText(
                    icon("check"), "Prior and scenario checking succeeded."
                )
            } else {
                helpText(
                    icon("warning"), "Prior and scenario checking failed",
                    "(see log panel above)."
                )
            }
        }
    })
    
    ## output
    observe({
        out$n_rec <- max(local$n_rec_initial, local$n_rec_final)
        out$n_stat <- local$n_stat
        out$n_scenario <- local$n_scenario
    })
    
    return(out)
}
