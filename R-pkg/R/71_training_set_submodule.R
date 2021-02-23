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
            raw_scenario = reactive(local$raw_scenario),
            scenario_id = reactive(id)
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
            "You might need to impose some conditions on", 
            "historical parameters",
            "(e.g. to avoid genealogical inconsistencies)",
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
        req(!is.null(local$data_info$n_loci))
        
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
                            )
                        )
                    )
                }
            )
            names(tag_list) <- NULL
            tagList(
                do.call(tagList, tag_list),
                br(),
                fluidRow(
                    column(
                        width = 4,
                        numericInput(
                            inputId = ns("locus_id_from"),
                            label = "from",
                            min = 1,
                            max = local$data_info$n_loci,
                            value = 1
                        ) %>% 
                            helper(
                                type = "inline", 
                                content = paste(
                                    "Index of the first locus in the data file",
                                    "to be included in the analysis.",
                                    "For instance,", 
                                    "if the data file contains a total of",
                                    "10 loci (whatever their type),",
                                    "and you choose to use 5 loci,",
                                    "if you set up `from = 4`",
                                    "the loci from 4 to 9 (out of 10)",
                                    "will be used in the analysis.",
                                    sep = " "
                                )
                            )
                    )
                )
            )
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
            req(length(local$data_info$locus) > 0)
            tmp <- unlist(lapply(
                1:length(local$data_info$locus),
                function(ind) {
                    item <- local$data_info$locus[ind]
                    locus <- str_extract(item, pattern = "(A|H|X|Y|M)")
                    n_loci <- input[[ str_c("num_", locus) ]]
                    return(str_c(
                        n_loci,
                        str_c("<", locus, ">"),
                        sep = " "
                    ))
                }
            ))
            # start loci
            start_locus <- input$locus_id_from
            
            # locus number encoding
            out$locus <- str_c(
                str_c(tmp, collapse = " "),
                "G1 from", start_locus, sep = " "
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
        req(!is.null(local$proj_dir))

        logging("diyabc prior/model check run exit status:",
                local$prior_check_result)

        ## check run
        # run ok
        if(local$prior_check_result == 0) {
            # notification
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
            # graph output
            tryCatch(
                scenario_check_graph_ouptut(local$proj_dir, local$proj_dir),
                error = function(e) {
                    showNotification(
                        id = ns("prior_mod_check_graph_output_fail"),
                        duration = 5,
                        closeButton = TRUE,
                        type = "error",
                        tagList(
                            tags$p(
                                icon("warning"),
                                "Graphical output generation for",
                                "scenario checking failed."
                            )
                        )
                    )
                }
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
