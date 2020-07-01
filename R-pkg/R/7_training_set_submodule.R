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
        # print("=== training set: project directory")
        # print(local$proj_dir)
    })
    
    # init output
    out <- reactiveValues(
        valid_proj = TRUE
    )
    
    # # debugging
    # observe({
    #     print("data info")
    #     print(local$data_info)
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
    #     print("project header content")
    #     print(local$proj_header_content)
    # })
    
    ## training set def (if no header file provided)
    output$enable_def <- renderUI({
        
        # print("###### debug enable def")
        # logging("new proj =", local$new_proj)
        # logging("valid proj =", local$valid_proj)
        # print("proj file list")
        # print(local$proj_file_list)
        
        req(!is.null(local$new_proj))
        req(!is.null(local$proj_file_list))
        req(!is.null(local$valid_proj))
        
        if(!local$valid_proj) {
            helpText(
                icon("warning"), "Project set up is not valid."
            )
        } else if(local$new_proj | 
                  !("headerRF.txt" %in% local$proj_file_list)) {
            tagList(
                training_set_def_ui(ns("def"))
            )
        } else {
            tagList(
                show_existing_proj_ui(ns("show"))
            )
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
    
    # get output
    observeEvent(training_set_def$trigger, {
        req(!is.null(training_set_def$trigger))
        req(!is.null(training_set_def$valid_def))
        local$valid_proj <- local$valid_proj & training_set_def$valid_def
        
        # # debugging
        # print("training set valid def")
        # print(training_set_def$valid_def)
    })
    
    ## enable control
    output$enable_control <- renderUI({
        req(!is.null(local$valid_proj))
        
        if(local$valid_proj) {
            training_set_action_ui(ns("action"))
        } else {
            tagList(
                h3(icon("gear"), "Run"),
                helpText(icon("warning"), "Project is not ready.")
            )
        }
    })
    
    ## action
    callModule(
        training_set_action_server, "action", 
        proj_dir = reactive(local$proj_dir),
        proj_file_list = reactive(local$proj_file_list),
        valid_proj = reactive(local$valid_proj)
    )
    
    # update output
    observe({
        out$valid_proj <- local$valid_proj
    })
    
    # output
    return(out)
}

#' Show existing project set up module ui
#' @keywords internal
#' @author Ghislain Durif
show_existing_proj_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
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
        h3(icon("dna"), "Number of SNP loci to simulate"),
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
        
        if(local$valid_proj & 
           length(local$proj_header_content$raw_cond_list) > 0) {
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
        data_file = NULL,
        data_info = NULL,
        locus_type = NULL,
        seq_mode = NULL,
        proj_dir = NULL,
        raw_scenario_list = NULL,
        scenario_list = NULL,
        valid_proj = NULL
    )
    # get input
    observe({
        local$data_file <- data_file()
        local$data_info <- data_info()
        local$locus_type <- locus_type()
        local$seq_mode <- seq_mode()
        local$proj_dir <- proj_dir()
        local$raw_scenario_list <- raw_scenario_list()
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
    #     print("hist_model_def")
    #     print(hist_model_def$scenario_list)
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
            
            # print("--- param list")
            # print(local$param_list)
            
            # param count list
            local$param_count_list <- lapply(
                local$scenario_list,
                function(item) {
                    # print(item$param)
                    return(length(item$param$Ne_param) + 
                               length(item$param$time) + 
                               length(item$param$rate))
                }
            )
            
            # print("--- param count list")
            # print(local$param_count_list)
            
            # print("----")
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
        # print(prior_cond_set$raw_prior_list)
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
        
        # print("proj_dir =")
        # print(local$proj_dir)
        # print("raw_param_list =")
        # print(local$raw_param_list)
        # print("param_count_list =")
        # print(local$param_count_list)
        # print("scenario_list =")
        # print(local$raw_scenario_list)
        # print("cond_list =")
        # print(local$raw_cond_list)
        # print("data_file =")
        # print(local$data_file)
        # print("locus_type =")
        # print(local$locus_type)
        # print("seq_mode =")
        # print(local$seq_mode)
        # print("locus =")
        # print(locus_setup$locus)
        
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
        
        if(length(local$raw_cond_list) == 0) {
            ready <- FALSE
            msg <- append(
                msg,
                "Missing conditions."
            )
        }
        
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
            
            write_header(local$proj_dir, local$data_file, 
                         local$raw_scenario_list, local$param_count_list, 
                         unlist(local$raw_param_list), 
                         unlist(local$raw_cond_list), 
                         local$locus_type, local$seq_mode, locus_setup$locus)
            
            file_check <- parse_diyabc_header(
                file_name = file.path(local$proj_dir, "header.txt"),
                file_type = "text/plain",
                locus_type = local$locus_type
            )
            
            # # debugging
            # print("is header ok")
            # print(file_check$valid)
            
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
            
            out$trigger <- ifelse(!is.null(out$trigger), out$trigger, 0) + 1
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
        # add new tab
        appendTab(
            inputId = "scenario_tabs",
            tabPanel(
                title = closable_tab_title(id, 
                                           label = str_c("Scenario ", id),
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
    
    # update output
    observe({
        out$scenario_list <- local$scenario_list
        # if(length(local$scenario_list) > 0) {
        #     print(local$scenario_list[[1]]$raw)
        #     print(local$scenario_list[[1]]$param)
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
        label,
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
            rows = 12,
            resize = "none"
        ),
        helpText(
            "Enter a single condition per line.",
            "Conditions should have the following format: XX<YY.", 
            "where 'XX' and 'YY' are parameters of the same type.",
            "You can use the standard comparison signs: '>', '>=', '<', '=<'."
        ),
        uiOutput(ns("cond_help")),
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
    #     print("param list")
    #     print(unique(local$param_list))
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
        # print(out$raw_prior_list)
    })
    # condition help
    output$cond_help <- renderUI({
        req(!is.null(local$cond_list))
        helpText(
            "We recommend that you enter the following conditions:",
            do.call(
                tags$ul,
                lapply(unique(local$cond_list), function(item) {
                    return(tags$li(item))
                })
            )
        )
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
                # print(local$cond_check)
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
        fluidRow(
            column(
                width = 1,
                tags$h4(textOutput(
                    ns("param_name")
                ))
            ),
            column(
                width = 4,
                radioGroupButtons(
                    ns("prior_type"),
                    label = NULL,
                    choices = list("Uniform" = "UN", "Log-Unif." = "LU",
                                   "Normal" = "NO", "Log-Norm." = "LN"),
                    selected = "UN",
                    justified = TRUE
                )
            ),
            column(
                width = 7,
                fluidRow(
                    column(
                        width = 3,
                        splitLayout(
                            tags$h5(
                                "Min.", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            numericInput(
                                ns("min"), label = NULL,
                                value = 10, step = 0.01
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
                            numericInput(
                                ns("max"), label = NULL,
                                value = 10000, step = 0.01
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
                            numericInput(
                                ns("mean"), label = NULL,
                                value = 0, step = 0.01
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    ),
                    column(
                        width = 3,
                        splitLayout(
                            tags$h5(
                                "Std. dev.", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            numericInput(
                                ns("stdev"), label = NULL,
                                value = 0, step = 0.01
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
                               min = 0, max = 1)
            updateNumericInput(session, "max", value = 0.95, 
                               min = 0, max = 1)
        } else {
            updateNumericInput(session, "min", value = 10, 
                               min = NULL, max = NULL)
            updateNumericInput(session, "max", value = 10000, 
                               min = NULL, max = NULL)
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
        h3(icon("dna"), "Number of SNP loci to simulate"),
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
    #     print("data info")
    #     print(local$data_info)
    #     print("locus type")
    #     print(local$locus_type)
    #     print("seq mode")
    #     print(local$seq_mode)
    # })
    
    # init out
    out <- reactiveValues(
        locus = NULL
    )
    # render ui
    output$setup <- renderUI({
        
        # # debugging
        # print("locus_type")
        # print(local$locus_type)
        # print("locus")
        # print(local$data_info$locus_type)
        
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
            
            # print("data info")
            # print(local$data_info)
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
            mss_group_setup_ui(ns("mss_group"))
        } else {
            NULL
        }
    })
    
    mss_group <- callModule(
        mss_group_setup_server,
        "mss_group",
        data_info = reactive(local$data_info)
    )
    
    ## update output
    # FIXME
    observe({
        if(local$locus_type == "snp") {
            out$locus <- lapply(
                local$data_info$locus,
                function(item) {
                    locus <- str_extract(item,
                                        pattern = "(A|H|X|Y|M)")
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
            # print("locus setup")
            # print(out$locus)
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
            label = "Configure Microsat locus grouping",
            width = '100%'
        ),
        shinyjs::hidden(uiOutput(ns("microsat_setup"))),
        br(),
        br(),
        actionButton(
            ns("enable_microsat_setup_motif"),
            label = "Configure Microsat locus motif and range",
            width = '100%'
        ),
        helpText(
            "By default, all Microsat loci are supposed to be dinucleid",
            "(motif = 2) with a range of 40."
        ),
        shinyjs::hidden(uiOutput(ns("microsat_setup_motif"))),
        br(),
        hr(),
        h4("Sequence Loci"),
        actionButton(
            ns("enable_seq_setup"),
            label = "Configure Sequence locus grouping",
            width = '100%'
        ),
        shinyjs::hidden(uiOutput(ns("seq_setup"))),
        hr()
    )
}

#' MSS locus group setup server
#' @keywords internal
#' @author Ghislain Durif
mss_group_setup_server <- function(input, output, session, 
                                   data_info = reactive({NULL})) {
    
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        microsat_locus = list(),
        seq_locus = list(),
        seq_length = NULL,
        # input
        data_info = NULL
    )
    # get input
    observe({
        local$data_info <- data_info()
        # # debugging
        # print("data info")
        # print(local$data_info)
    })
    
    ## show/hide microsat grouping
    observeEvent(input$enable_microsat_setup, {
        req(!is.null(input$enable_microsat_setup))
        if(input$enable_microsat_setup %% 2 == 0) {
            shinyjs::hide(id = "microsat_setup")
        } else{
            shinyjs::show(id = "microsat_setup")
        }
    })
    
    ## show/hide seq grouping
    observeEvent(input$enable_seq_setup, {
        req(!is.null(input$enable_seq_setup))
        if(input$enable_seq_setup %% 2 == 0) {
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
        
        microsat_locus <- str_detect(local$data_info$locus_mode, "microsat")
        if(sum(microsat_locus) > 0) {
            local$microsat_locus <- local$data_info$locus_name[microsat_locus]
        } else {
            list()
        }
        
        seq_locus <- str_detect(local$data_info$locus_mode, "seq")
        if(sum(seq_locus) > 0) {
            local$seq_locus <- local$data_info$locus_name[seq_locus]
        } else {
            list()
        }
    })
    
    observe({
        print("microsat locus")
        print(local$microsat_locus)
        print("seq locus")
        print(local$seq_locus)
    })
    
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
        locus_name = reactive(local$seq_locus)
    )
    
    ## show/hide microsat motif/range set up
    observeEvent(input$enable_microsat_setup_motif, {
        req(!is.null(input$enable_microsat_setup_motif))
        if(input$enable_microsat_setup_motif %% 2 == 0) {
            shinyjs::hide(id = "microsat_setup_motif")
        } else {
            shinyjs::show(id = "microsat_setup_motif")
        }
    })
    
    # microsat motif/range/setup
    output$microsat_setup_motif <- renderUI({
        req(!is.null(local$microsat_locus))
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
                                     locus_name = reactive({NULL})) {
    
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        n_group = 1,
        # input
        locus_name = NULL
    )
    # get input
    observe({
        local$locus_name <- locus_name()
    })
    
    # group making
    output$locus_group <- renderUI({
        req(!is.null(local$locus_name))
        req(length(local$locus_name) > 0)
        
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
                            choices = as.character(1:local$n_group),
                            selected = 1
                        )
                    )
                )
            }
        )
        do.call(flowLayout, tag_list)
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
    
}

#' Training set simulation action module ui
#' @keywords internal
#' @author Ghislain Durif
training_set_action_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("gear"), "Run"),
        numericInput(
            ns("nrun"),
            label = "Number of simulations",
            value = 100,
            min = 100
        ),
        helpText(
            tags$div(
                icon("warning"),
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
        checkboxInput(
            ns("prior_mod_check"),
            label = "Run prior and scenario checking",
            value = FALSE
        ),
        hr(),
        actionBttn(
            inputId = ns("simulate"),
            label = "Simulate",
            style = "fill",
            block = TRUE,
            col = "primary"
        ),
        progressBar(
            id = ns("simu_progress"),
            value = 0,
            total = 100,
            title = "",
            display_pct = TRUE
        ),
        uiOutput(ns("feedback")),
        uiOutput(ns("feedback_nrun")),
        br(),
        actionButton(
            ns("show_log"),
            label = "Show/hide logs",
            icon = icon("comment")
        ),
        shinyjs::hidden(
            uiOutput(ns("run_log"))
        ),
        br(),
        br(),
        actionBttn(
            inputId = ns("stop"),
            label = "Stop",
            style = "fill",
            block = TRUE,
            color = "danger"
        )
    )
}

#' Training set simulation action module server
#' @keywords internal
#' @author Ghislain Durif
training_set_action_server <- function(input, output, session,
                                       proj_dir = reactive({NULL}),
                                       proj_file_list = reactive({NULL}),
                                       valid_proj = reactive({FALSE})) {
    # namespace
    ns <- session$ns
    
    # max number of rows in log
    nlog <- 100
    show_nlog <- 10
    
    # init local
    local <- reactiveValues(
        diyabc_run_process = NULL,
        diyabc_run_result = NULL,
        feedback = NULL,
        log_file_content = NULL,
        # input
        proj_dir = NULL,
        proj_file_list = NULL,
        valid_proj = NULL
    )
    # get input
    observe({
        local$proj_dir <- proj_dir()
        local$proj_file_list <- proj_file_list()
        local$valid_proj <- valid_proj()
    })
    
    # # debugging
    # observe({
    #     print("Training set action input")
    #     print(local$proj_dir)
    #     print(local$valid)
    # })
    
    # # debugging
    # observe({
    #     print("=== training set simu run: project directory")
    #     print(local$proj_dir)
    # })
    
    # init output
    out <- reactiveValues()
    
    ## show/hide logs
    observeEvent(input$show_log, {
        req(!is.null(input$show_log))
        if(input$show_log %% 2 == 0) {
            shinyjs::hide(id = "run_log")
        } else{
            shinyjs::show(id = "run_log")
        }
    })
    
    ## read log file
    log_file_content <- function() return(rep("", show_nlog))
    observeEvent(local$proj_dir, {
        req(local$proj_dir)
        log_file_content <<- reactiveFileReader(
            500, session,
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
    
    ## show log messages
    observeEvent(local$log_file_content, {
        
        if(length(local$log_file_content) < show_nlog) {
            local$log_file_content <- c(
                local$log_file_content,
                rep("", show_nlog - length(local$log_file_content))
            )
        }
    })
    
    
    ## run simulation
    observeEvent(input$simulate, {
        
        req(!is.null(local$valid_proj))
        req(!is.null(local$proj_file_list))
        req(length(local$proj_file_list) > 0)
        
        # # debugging
        # print("simulate")
        # print("valid proj ?")
        # print(local$valid_proj)
        # print("proj file list")
        # print(local$proj_file_list)
        
        req(input$nrun)
        req(local$proj_dir)
        req(!is.null(input$prior_mod_check))
        
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
            ## prepre run
            req(is.null(local$diyabc_run_process))
            
            ## init progress bar
            updateProgressBar(
                session = session,
                id = "simu_progress",
                value = 0, total = 100,
                title = str_c(
                    "Running simulation ", "0/", 
                    input$nrun, sep = ""
                )
            )
            
            ## check if possible to run
            if(!local$valid_proj) {
                local$feedback <- helpText(
                    icon("warning"), "Project is not ready.",
                    "Check project settings."
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
                # print("check options")
                # print(getOption("diyabcGUI"))
                # print(getOption("shiny.maxRequestSize"))
                
                local$feedback <- helpText(
                    icon("spinner", class = "fa-spin"),
                    "Simulations are running."
                )
                
                logging("Running simulation")
                local$diyabc_run_process <- diyabc_run_trainset_simu(
                    local$proj_dir, 
                    as.integer(input$nrun),
                    input$prior_mod_check
                )
            }
        }
    })
    
    output$run_log <- renderUI({
        tagList(
            tags$pre(
                str_c(
                    tail(local$log_file_content, show_nlog),
                    collapse = "\n"
                ),
                style = "width:60vw; overflow:scroll;"
            )
        )
    })
    
    ## monitor simulation run
    observeEvent(local$diyabc_run_process, {
        req(!is.null(local$diyabc_run_process))
        
        print("diyabc run process")
        print(local$diyabc_run_process)
        
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
            tagList(
                tags$pre(
                    str_c(
                        local$log_file_content,
                        collapse = "\n"
                    ),
                    style = "width:60vw; overflow:scroll; overflow-y:scroll; min-height:100px; max-height:100px;"
                )
            )
        })
        
        ## check run
        # run ok
        if(local$diyabc_run_result == 0) {
            local$feedback <- helpText(
                icon("check"), "Run succeeded."
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
        } else {
            ## error during run
            local$feedback <- helpText(
                icon("warning"), "Issues with run (see log panel)."
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
            proc$kill()
            local$diyabc_run_result <- -1000
        }
    })
    
    ## progress bar
    observeEvent(local$log_file_content, {
        
        req(input$nrun)
        
        req(!is.null(local$log_file_content))
        req(length(local$log_file_content) > 0)
        
        last_message <- tail(local$log_file_content, nlog)
        find_adv <- str_detect(last_message, "^[0-9]+$")
        
        if(any(find_adv)) {
            final_adv <- tail(last_message[find_adv], 1)
            updateProgressBar(
                session = session,
                id = "simu_progress",
                value = 100 * as.numeric(final_adv)/input$nrun, 
                total = 100,
                title = str_c(
                    "Running simulation ", 
                    final_adv, "/", input$nrun, sep = "")
            )
        }
    })
    
    ## feedback
    observeEvent(local$feedback, {
        output$feedback <- renderUI({
            local$feedback
        })
    })
    
    ## feedback nrun
    output$feedback_nrun <- renderUI({
        req(!is.null(input$nrun))
        
        req(!is.null(local$log_file_content))
        req(length(local$log_file_content) > 0)
        
        tmp_text <- NULL
        
        # simu nrun
        if(any(str_detect(local$log_file_content, "^nrec = [0-9]+$"))) {
            nrun0 <- str_extract(
                local$log_file_content, 
                "(?<=^nrec = )[0-9]+$"
            )
            nrun0 <- tail(nrun0[!is.na(nrun0)], 1)
            if(nrun0 >= input$nrun) {
                tmp_text <- helpText(
                    icon("warning"), 
                    "Number of already available simulations",
                    tags$b(nrun0), ".",
                    "To generate additional training data, you must set",
                    "the number of simulations to be higher than",
                    tags$b(nrun0), "."
                )
            }
        }
        tmp_text
    })
}
