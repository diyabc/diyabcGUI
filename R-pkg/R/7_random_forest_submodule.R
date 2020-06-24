#' Randon forest submodule ui
#' @keywords internal
#' @author Ghislain Durif
rf_module_ui <- function(id) {
    ns <- NS(id)
    uiOutput(ns("enable_control"))
}


#' Randon forest submodule server
#' @keywords internal
#' @author Ghislain Durif
rf_module_server <- function(input, output, session, 
                             locus_type = reactive({NULL}),
                             proj_dir = reactive({NULL}),
                             proj_name = reactive({NULL}),
                             valid_proj = reactive({FALSE})) {
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        proj_header_file = NULL,
        proj_file_list = NULL,
        proj_header = NULL,
        proj_ready = FALSE,
        # input
        locus_type = NULL,
        proj_dir = NULL,
        proj_name = NULL,
        valid_proj = NULL
    )
    
    # get input
    observe({
        local$locus_type <- locus_type()
        local$proj_dir <- proj_dir()
        local$proj_name <- proj_name()
        local$valid_proj <- valid_proj()
    })
    
    # required files
    required_files <- c("headerRF.txt", "reftableRF.bin", "statobsRF.txt")
    
    # check project directory
    observe({
        req(!is.null(local$proj_dir))
        
        proj_file_list <- reactivePoll(
            1000, session,
            checkFunc = function() {
                if(dir.exists(local$proj_dir)) {
                    list.files(local$proj_dir)
                } else {
                    list()
                }
            },
            valueFunc = function() {
                if(!is.null(local$proj_dir)) {
                    if(dir.exists(local$proj_dir))
                        list.files(local$proj_dir)
                    else
                        list()
                } else {
                    list()
                }
            }
        )
        
        local$proj_file_list <- proj_file_list()
    })
    
    # # debugging
    # observe({
    #     print("proj_file_list")
    #     print(local$proj_file_list)
    # })
    
    # enable control
    output$enable_control <- renderUI({
        req(!is.null(local$valid_proj))
        req(!is.null(local$proj_file_list))
        
        if(!local$valid_proj) {
            helpText(
                icon("warning"), "Project set up is not valid."
            )
        } else if(!all(required_files %in% local$proj_file_list)) {
            helpText(
                icon("warning"), 
                "You must generate a training data set with the", 
                "'Training set simulations' sub-module,",
                "or upload training set simulations-related files",
                "(",
                do.call(
                    tagList,
                    lapply(required_files, tags$code)
                ),
                ")",
                "from an existing project."
            )
        } else {
            tagList(
                rf_parameter_ui(ns("rf_param")),
                hr(),
                rf_control_ui(ns("rf_control"))
            )
        }
    })
    
    ## rf parameter
    rf_param <- callModule(
        rf_parameter_server, "rf_param",
        proj_dir = reactive(local$proj_dir),
        locus_type = reactive(local$locus_type)
    )

    # valid proj ?
    observe({
        local$proj_ready <- local$valid_proj & rf_param$param_ready
    })
    
    ## rf control
    rf_control <- callModule(
        rf_control_server, "rf_control",
        proj_dir = reactive(local$proj_dir),
        proj_file_list = reactive(local$proj_file_list),
        proj_ready = reactive(local$proj_ready),
        valid_proj = reactive(local$valid_proj),
        group = reactive(rf_param$group),
        min_node_size = reactive(rf_param$min_node_size),
        noise_columns = reactive(rf_param$noise_columns),
        linear = reactive(rf_param$linear),
        pls_max_var = reactive(rf_param$pls_max_var),
        n_tree = reactive(rf_param$n_tree),
        chosen_scenario = reactive(rf_param$chosen_scenario),
        noob = reactive(rf_param$noob),
        parameter = reactive(rf_param$parameter),
        run_mode = reactive(rf_param$run_mode)
    )
}

#' Random forest parameter ui
#' @keywords internal
#' @author Ghislain Durif
rf_parameter_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h4("Settings"),
        radioButtons(
            ns("run_mode"), 
            label = "Mode",
            choices = list("Model choice" = "model_choice", 
                           "Parameter estimation" = "param_estim"), 
            selected = "model_choice"
        ),
        hr(),
        # numericInput(
        #     ns("min_node_size"),
        #     label = "Minimal node size",
        #     min = 0,
        #     value = 0
        # ),
        # helpText(
        #     "0 means 1 for classification or 5 for regression."
        # ),
        numericInput(
            ns("n_tree"),
            label = "Number of trees",
            min = 1,
            value = 500
        ),
        numericInput(
            ns("noise_columns"),
            label = "Number of noise columns",
            min = 0,
            value = 5
        ),
        h5(tags$b("Linear framework")),
        checkboxInput(
            ns("linear"),
            label = tags$span(
                "Enable/Disable LDA for model choice or PLS for", 
                "parameter estimation"
            ),
            value = TRUE
        ),
        # if parameter estimation
        conditionalPanel(
            condition = "input.run_mode == 'param_estim'",
            ns = ns,
            numericInput(
                ns("pls_max_var"),
                label = "PLS explained variance threshold",
                min = 0.001,
                max = 0.999,
                value = 0.9,
                step = 0.001
            ),
            helpText(
                "Percentage of maximum explained Y-variance", 
                "for retaining pls axis"
            ),
            numericInput(
                ns("chosen_scenario"),
                label = "Chosen scenario",
                value = 1,
                min = 1
            ),
            textInput(
                ns("parameter"),
                label = "Parameter to estimate"
            ),
            uiOutput(
                ns("missing_parameter")
            ),
            uiOutput(
                ns("possible_parameters")
            ),
            numericInput(
                ns("noob"),
                label = "Number of oob testing samples",
                value = 10,
                min = 1
            ) %>% 
                helper(type = "markdown", 
                       content = "noob_parameter"),
        ),
        # if model choice
        conditionalPanel(
            condition = "input.run_mode == 'model_choice'",
            ns = ns,
            textInput(
                ns("group"),
                label = "Model group"
            ),
            uiOutput(ns("help_group")),
            uiOutput(ns("feedback_group"))
        )
    )
}

#' Random forest parameter server
#' @keywords internal
#' @author Ghislain Durif
rf_parameter_server <- function(input, output, session,
                                proj_dir = reactive({NULL}),
                                proj_header_file = reactive({NULL}),
                                locus_type = reactive({NULL})) {
    # local
    local <- reactiveValues(
        proj_header_content = NULL,
        # input
        proj_dir = NULL,
        locus_type = NULL
    )
    
    # get input
    observe({
        local$proj_dir <- proj_dir()
        local$locus_type <- locus_type()
    })
    
    # output
    out <- reactiveValues(
        param_ready = TRUE,
        # parameters
        chosen_scenario = NULL,
        group = NULL,
        linear = NULL,
        min_node_size = 0,
        n_tree = NULL,
        noise_columns = NULL,
        noob = NULL,
        parameter = NULL,
        pls_max_var = NULL,
        run_mode = NULL
    )
    
    ## monitor change in headerRF.txt file
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
    
    # # debugging
    # observe({
    #     print("proj_header_file")
    #     print(local$proj_header_file)
    # })
    
    # possible scenario and possible parameters
    observeEvent(local$proj_header_content, {
        req(local$proj_header_content)
        
        file_check <- local$proj_header_content
        
        # # debugging
        # logging("number of scenario = ", 
        #         length(file_check$raw_scenario_list))
        
        # update corresponding input
        updateNumericInput(
            session, "chosen_scenario", 
            max = length(file_check$raw_scenario_list)
        )
        
        # possible parameters
        output$possible_parameters <- renderUI({
            
            param_list <- lapply(
                file_check$raw_prior_list,
                function(item) {
                    return(
                        tags$li(
                            str_extract(
                                item,
                                single_param_regex()
                            )
                        )
                    )
                }
            )
            
            helpText(
                "You can use one of the following parameter",
                "or an arithmetic combination of them, such",
                "as division, addition or multiplication of",
                "two existing parameters. like 't/N' or 'T1+T2'.",
                tags$div(
                    style = "column-count:2;",
                    do.call(tags$ul, param_list)
                )
            )
        })
        
        # possible groups
        output$help_group <- renderUI({
            helpText(
                "You may 'group' your models in several splitted groups.",
                "For example if you have six models, labeled from 1 to 6,",
                "you can specify '1,2,3;4,5,6' to make 2 groups of 3.",
                "Here you have", 
                tags$b(as.character(length(file_check$raw_scenario_list))),
                "scenarii.",
                "Leave blank to not group scenarii."
            )
        })
    })
    
    # check and feedback groups
    observe({
        req(input$group)
        req(local$proj_header_content)
        
        file_check <- local$proj_header_content
        
        # check
        group_check <- parse_abcranger_group(
            input$group,
            length(file_check$raw_scenario_list)
        )
        
        # save check
        out$param_ready <- out$param_ready & group_check$valid
        
        # feedback
        output$feedback_group <- renderUI({
            if(!group_check$valid) {
                helpText(
                    icon("warning"), "Issue with scenario grouping:",
                    do.call(
                        tags$ul,
                        lapply(group_check$msg, tags$li)
                    )
                )
            } else {
                NULL
            }
        })
    })
    
    # check parameter input
    output$missing_parameter <- renderUI({
        if(is.null(input$parameter)) {
            helpText(
                icon("warning"), "Missing parameter."
            )
        } else {
            if(str_length(input$parameter) == 0) {
                helpText(
                    icon("warning"), "Missing parameter."
                )
            } else {
                NULL
            }
        }
    })
    
    # proj not ready if no parameter in param_estim mode
    observe({
        req(input$run_mode)
        if(input$run_mode == "param_estim") {
            if(is.null(input$parameter)) {
                out$param_ready <- FALSE
            } else if(str_length(input$parameter) == 0) {
                out$param_ready <- FALSE
            } else {
                out$param_ready <- TRUE
            }
        } else {
            out$param_ready <- TRUE
        }
    })
    
    # output
    observe({
        out$group <- input$group
        out$linear <- input$linear
        out$chosen_scenario <- input$chosen_scenario
        out$run_mode <- input$run_mode
        out$min_node_size <- 0
        out$n_tree <- input$n_tree
        out$noise_columns <- input$noise_columns
        out$noob <- input$noob
        out$parameter <- input$parameter
        out$pls_max_var <- input$pls_max_var
    })
    
    ## output
    return(out)
}

#' Random forest control ui
#' @keywords internal
#' @author Ghislain Durif
rf_control_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("gear"), "Run"),
        actionBttn(
            inputId = ns("run"),
            label = "Run",
            style = "fill",
            block = TRUE,
            col = "primary"
        ),
        progressBar(
            id = ns("rf_progress"),
            value = 0,
            total = 100,
            # title = "",
            title = "Not working at the moment.",
            display_pct = TRUE
        ),
        uiOutput(ns("feedback")),
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

#' Random forest control server
#' @keywords internal
#' @author Ghislain Durif
rf_control_server <- function(input, output, session,
                              proj_dir = reactive({NULL}),
                              proj_file_list = reactive({NULL}),
                              proj_ready = reactive({FALSE}),
                              valid_proj = reactive({FALSE}),
                              # parameters
                              group = reactive({NULL}),
                              chosen_scenario = reactive({NULL}),
                              linear = reactive({NULL}),
                              min_node_size = 0,
                              n_tree = reactive({NULL}),
                              noise_columns = reactive({NULL}),
                              noob = reactive({NULL}),
                              parameter = reactive({NULL}),
                              pls_max_var = reactive({NULL}),
                              run_mode = reactive({NULL})) {
    
    # namespace
    ns <- session$ns
    
    # max number of rows in log
    nlog <- 100
    show_nlog <- 10
    
    # init local
    local <- reactiveValues(
        abcranger_run_process = NULL,
        abcranger_run_result = NULL,
        feedback = NULL,
        log_file_content = NULL,
        # input
        proj_dir = NULL,
        proj_file_list = NULL,
        proj_ready = NULL,
        valid_proj = NULL,
        # parameters
        group = NULL,
        chosen_scenario = NULL,
        linear = NULL,
        min_node_size = 0,
        n_tree = NULL,
        noise_columns = NULL,
        noob = NULL,
        parameter = NULL,
        pls_max_var = NULL,
        run_mode = NULL
    )
    
    # get input
    observe({
        local$proj_dir <- proj_dir()
        local$proj_file_list <- proj_file_list()
        local$proj_ready <- proj_ready()
        local$valid_proj <- valid_proj()
        
        local$group <- group()
        local$min_node_size <- min_node_size()
        local$noise_columns <- noise_columns()
        local$linear <- linear()
        local$pls_max_var <- pls_max_var()
        local$n_tree <- n_tree()
        local$chosen_scenario <- chosen_scenario()
        local$noob <- noob()
        local$parameter <- parameter()
        local$run_mode <- run_mode()
    })
    
    ## required files
    required_files <- c("headerRF.txt", "reftableRF.bin", "statobsRF.txt")
    
    ## show/hide logs
    observeEvent(input$show_log, {
        
        # print("click on show_log")
        # print(input$show_log)
        
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
            file.path(local$proj_dir, "abcranger_call.log"),
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
    
    # # debugging
    # observe({
    #     print("log file content")
    #     print(tail(local$log_file_content, 5))
    # })
    
    ## show log messages
    observeEvent(local$log_file_content, {
        
        if(length(local$log_file_content) < show_nlog) {
            local$log_file_content <- c(
                local$log_file_content,
                rep("", show_nlog - length(local$log_file_content))
            )
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
    
    ## run
    observeEvent(input$run, {
        
        # # debugging
        # print("run set up?")
        # print("valid proj ?")
        # print(local$valid_proj)
        # print("proj ready ?")
        # print(local$proj_ready)
        # print("proj file list")
        # print(local$proj_file_list)
        
        req(!is.null(local$proj_ready))
        req(!is.null(local$valid_proj))
        req(!is.null(local$proj_file_list))
        req(length(local$proj_file_list) > 0)
        
        ## run in progress
        if(!is.null(local$abcranger_run_process)) {
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
            req(is.null(local$abcranger_run_process))
            
            ## init progress bar
            updateProgressBar(
                session = session,
                id = "rf_progress",
                value = 0, total = 100,
                # FIXME
                # title = str_c(
                #     "RF run: tree building ", "0/", 
                #     local$n_tree, sep = ""
                # )
                title = "Not working at the moment."
            )
            
            ## check if possible to run
            if(!local$valid_proj) {
                local$feedback <- helpText(
                    icon("warning"), "Project is not ready.",
                    "Check project settings."
                )
            } else if(!all(required_files %in% local$proj_file_list)) {
                local$feedback <- helpText(
                    icon("warning"), 
                    "You must generate a training data set with the", 
                    "'Training set simulations' sub-module,",
                    "or upload training set simulations-related files",
                    "(",
                    do.call(
                        tagList,
                        lapply(required_files, tags$code)
                    ),
                    ")",
                    "from an existing project."
                )
                showNotification(
                    id = ns("missing_files"),
                    duration = 5,
                    closeButton = TRUE,
                    type = "warning",
                    tagList(
                        tags$p(
                            icon("warning"), 
                            "You must generate a training data set with the", 
                            "'Training set simulations' sub-module,",
                            "or upload training set simulations-related files",
                            "(",
                            do.call(
                                tagList,
                                lapply(required_files, tags$code)
                            ),
                            ")",
                            "from an existing project."
                        )
                    )
                )
            } else if(!local$proj_ready) {
                local$feedback <- helpText(
                    icon("warning"), "Missing parameters.",
                    "Check run settings."
                )
            } else {
                ## ready to run
                
                # debugging
                # print("check options")
                # print(getOption("diyabcGUI"))
                # print(getOption("shiny.maxRequestSize"))
                
                local$feedback <- helpText(
                    icon("spinner", class = "fa-spin"),
                    "Run in progress."
                )
                
                # # debugging
                # print("run")
                # print("valid proj ?")
                # print(local$valid_proj)
                # print("proj ready ?")
                # print(local$proj_ready)
                # print("proj file list")
                # print(local$proj_file_list)
                
                # print("abcranger args")
                # print("proj_dir =")
                # print(local$proj_dir)
                # print("mode =")
                # print(local$run_mode)
                # 
                # print("min_node_size =")
                # print(local$min_node_size)
                # print("noise_columns =")
                # print(local$noise_columns)
                # print("linear =")
                # print(local$linear)
                # print("pls_max_var")
                # print(local$pls_max_var)
                # print("n_tree")
                # print(local$n_tree)
                # 
                # if(local$run_mode == "param_estim") {
                #     print("chosen_scen =")
                #     print(local$chosen_scenario)
                #     print("noob =")
                #     print(local$noob)
                #     print("parameter =")
                #     print(local$parameter)
                # }
                
                req(!is.null(local$proj_dir))
                req(!is.null(local$run_mode))
                req(!is.null(local$min_node_size))
                req(!is.null(local$noise_columns))
                req(!is.null(local$linear))
                req(!is.null(local$pls_max_var))
                req(!is.null(local$n_tree))
                
                if(local$run_mode == "param_estim") {
                    req(!is.null(local$chosen_scenario))
                    req(!is.null(local$noob))
                    req(!is.null(local$parameter))
                    req(str_length(local$parameter) > 0)
                }
                
                logging("running abcranger")
                
                n_ref <- 0
                local$abcranger_run_process <- abcranger_run(
                    local$proj_dir, local$run_mode, n_ref, 
                    local$min_node_size, local$n_tree, local$noise_columns, 
                    !local$linear, 
                    local$pls_max_var, local$chosen_scenario, local$noob, 
                    local$parameter, 
                    local$group
                )
            }
        }
    })
    
    ## monitor simulation run
    observeEvent(local$abcranger_run_process, {
        req(!is.null(local$abcranger_run_process))
        
        print("abcranger run process")
        print(local$abcranger_run_process)
        
        observe({
            req(!is.null(local$abcranger_run_process))
            proc <- local$abcranger_run_process
            req(!is.null(proc$is_alive()))
            if(proc$is_alive()) {
                invalidateLater(2000, session)
            } else {
                local$abcranger_run_result <- proc$get_exit_status()
            }
        })
    })
    
    ## clean up after run
    observeEvent(local$abcranger_run_result, {
        
        req(!is.null(local$abcranger_run_result))
        
        logging("abcranger run exit status:",
                local$abcranger_run_result)
        
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
        if(local$abcranger_run_result == 0) {
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
        } else if(local$abcranger_run_result == -1000) {
            ## stopped run
            local$feedback <- helpText(
                icon("warning"), "Run was stopped."
            )
            showNotification(
                id = ns("stop_run"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"),
                        "Run was stopped."
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
                        "A problem happened during run."
                    )
                )
            )
        }
        
        # reset
        local$abcranger_run_result <- NULL
        local$abcranger_run_process <- NULL
    })
    
    ## stop run
    observeEvent(input$stop, {
        ## if no current run
        if(is.null(local$abcranger_run_process)) {
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
            proc <- local$abcranger_run_process
            proc$kill()
            local$abcranger_run_result <- -1000
        }
    })
    
    ## progress bar
    observeEvent(local$log_file_content, {
        
        ## FIXME
        
        # req(local$n_tree)
        # 
        # req(!is.null(local$log_file_content))
        # req(length(local$log_file_content) > 0)
        # 
        # last_message <- tail(local$log_file_content, nlog)
        # find_adv <- str_detect(last_message, "^[0-9]+$")
        # 
        # "computed: <nb d'arbres calculés>"
        # "read: <nb de lignes lues>"
        # 
        # if(any(find_adv)) {
        #     final_adv <- tail(last_message[find_adv], 1)
        #     updateProgressBar(
        #         session = session,
        #         id = "rf_progress",
        #         value = 100 * as.numeric(final_adv)/local$n_tree, 
        #         total = 100,
        #         title = str_c(
        #             "Running RF analysis: tree building ", 
        #             final_adv, "/", local$n_tree, sep = "")
        #     )
        # }
    })
    
    ## feedback
    observeEvent(local$feedback, {
        output$feedback <- renderUI({
            local$feedback
        })
    })
    
}