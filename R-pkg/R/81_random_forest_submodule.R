#' Randon forest submodule ui
#' @keywords internal
#' @author Ghislain Durif
rf_module_ui <- function(id) {
    ns <- NS(id)
    uiOutput(ns("enable_module"))
}


#' Randon forest submodule server
#' @keywords internal
#' @author Ghislain Durif
rf_module_server <- function(input, output, session) {
    # namespace
    ns <- session$ns
    
    # required files
    required_files <- c("headerRF.txt", "reftableRF.bin", "statobsRF.txt")
    
    # enable control
    output$enable_module <- renderUI({
        
        req(env$ap$proj_dir)
        req(env$ap$proj_type)
        req(env$ap$locus_type)
        req(env$ap$seq_mode)
        if(
            isTruthy(env$ap$proj_name) && 
            isTruthy(env$ap$data_file) &&
            isTruthy(env$ap$data_check) &&
            isTruthy(env$ap$data_check$valid) && 
            isTruthy(env$ap$header_check) && 
            isTruthy(env$ap$header_check$valid) &&
            isTruthy(env$ap$reftable_check) && 
            isTruthy(env$ap$reftable_check$valid) &&
            isTruthy(env$ap$statobs_check) && 
            isTruthy(env$ap$statobs_check$valid)
        ) {
            tagList(
                rf_train_set_desc_ui(ns("train_set_desc")),
                hr(),
                rf_parameter_ui(ns("rf_param")),
                hr(),
                rf_control_ui(ns("rf_control"))
            )
        } else {
            tagList(tags$div(
                h4(
                    icon("warning"), 
                    "Project set up is not ready.",
                    "You must generate a training data set with the", 
                    tags$b("Training set simulations"), "sub-module above,",
                    "or upload training set simulations-related files",
                    "(including",
                    do.call(
                        tagList,
                        lapply(required_files, tags$code)
                    ),
                    ") from an existing project."
                ),
                style = "color: #F89406;"
            ))
        }
    })
    
    ## training set description
    callModule(rf_train_set_desc_server, "train_set_desc")
    
    ## rf parameter
    callModule(rf_parameter_server, "rf_param")
    
    ## rf control
    # callModule(rf_control_server, "rf_control")
}

#' Random forest training set description reminder ui
#' @keywords internal
#' @author Ghislain Durif
rf_train_set_desc_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h4(icon("list-ol"), "Training set description"),
        uiOutput(ns("feedback"))
    )
}

#' Random forest training set description reminder server
#' @keywords internal
#' @author Ghislain Durif
rf_train_set_desc_server <- function(input, output, session) {
    ## context
    output$feedback <- renderUI({
        
        req(env$ap$file_modif)
        req(env$ap$reftable_check$valid)
        req(env$ap$reftable_check$n_rec)
        req(env$ap$reftable_check$n_rec_scen)
        req(env$ap$reftable_check$n_stat)
        req(env$ap$reftable_check$n_param_list)
        
        helpText(fluidRow(
            column(
                width = 4,
                tags$p(tags$ul(tags$li(
                    "Number of scenario =",
                    tags$b(as.character(
                        length(env$ap$reftable_check$n_param_list)
                    ))
                ))),
                tags$p(tags$ul(tags$li(
                    "Number of summary statistics =",
                    tags$b(env$ap$reftable_check$n_stat)
                )))
            ),
            column(
                width = 4,
                tags$p(tags$ul(tags$li(
                    "Total number of simulated datasets =",
                    tags$b(as.character(env$ap$reftable_check$n_rec)),
                    "including",
                    do.call(
                        tags$ul,
                        unname(lapply(
                            1:length(env$ap$reftable_check$n_rec_scen), 
                            function(ind) {
                                tags$li(
                                    tags$b(as.character(
                                        env$ap$reftable_check$n_rec_scen[ind]
                                    )), "for scenario", 
                                    tags$b(as.character(ind))
                                )
                            }
                        ))
                    )
                )))
            ),
            column(
                width = 4,
                tags$p(tags$ul(tags$li(
                    "Number of parameters:",
                    do.call(
                        tags$ul,
                        unname(lapply(
                            1:length(env$ap$reftable_check$n_param_list), 
                            function(ind) {
                                tags$li(
                                    tags$b(as.character(
                                        env$ap$reftable_check$n_param_list[ind]
                                    )), "for scenario", 
                                    tags$b(as.character(ind))
                                )
                            }
                        ))
                    )
                )))
            )
        ))
    })
}

#' Random forest parameter ui
#' @keywords internal
#' @author Ghislain Durif
rf_parameter_ui <- function(id) {
    ns <- NS(id)
    tagList(
        proj_name_ui(
            ns("proj_name"), label = "Analysis (sub-project) name"
        ),
        uiOutput(ns("feedback_proj_name")),
        hr(),
        h3("Settings"),
        radioButtons(
            ns("run_mode"), 
            label = "Mode",
            choices = list("Model choice" = "model_choice", 
                           "Parameter estimation" = "param_estim"), 
            selected = "model_choice"
        ),
        hr(),
        conditionalPanel(
            condition = "input.run_mode == 'param_estim'", ns = ns,
            uiOutput(ns("input_chosen_scenario")),
            textInput(
                ns("parameter"),
                label = "Parameter to estimate"
            ),
            uiOutput(ns("feedback_parameter")),
            uiOutput(ns("possible_parameters"))
        ),
        conditionalPanel(
            condition = "input.run_mode == 'model_choice'", ns = ns,
            textInput(
                ns("grouping"),
                label = "Scenario grouping and selection"
            ) %>% 
                helper(type = "markdown", 
                       content = "scenario_grouping_selection"),
            uiOutput(ns("feedback_grouping")),
            uiOutput(ns("help_grouping"))
        ),
        numericInput(
            ns("n_rec"),
            label = "Number of samples in the training set to use",
            value = 0, min = 0
        ),
        uiOutput(ns("feedback_nrec")),
        uiOutput(ns("help_nrec")),
        numericInput(
            ns("n_noise_columns"),
            label = "Number of noise variables to add",
            min = 0,
            value = 5
        ),
        h5(tags$b("Linear combinations of summary statistics")),
        checkboxInput(
            ns("linear"),
            label = tags$span(
                "Enable/Disable the addition of linear", 
                "combination axes",
            ),
            value = TRUE
        ),
        helpText(
            icon("info-circle"), 
            "Linear combinations of summary statistics",
            "are computed with",
            "LDA for model choice or PLS for parameter estimation."
        ),
        conditionalPanel(
            condition = "input.run_mode == 'param_estim'", ns = ns,
            numericInput(
                ns("pls_max_var"),
                label = "PLS explained variance threshold",
                min = 0.001,
                max = 0.999,
                value = 0.95,
                step = 0.001
            ),
            helpText(
                icon("info-circle"), 
                "Percentage of maximum explained Y-variance", 
                "for retaining pls axis"
            ),
            numericInput(
                ns("noob"),
                label = "Number of oob testing samples",
                value = 1000,
                min = 1
            ) %>% 
                helper(type = "markdown", content = "noob_parameter")
        ),
        numericInput(
            ns("n_tree"),
            label = "Number of trees",
            min = 1,
            value = 500
        )
    )
}

#' Random forest parameter server
#' @keywords internal
#' @author Ghislain Durif
rf_parameter_server <- function(input, output, session) {
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        max_n_rec = NULL, possible_param = NULL,
        param_check = FALSE
    )
    
    ## sub-project name
    callModule(proj_name_server, "proj_name", tag = "rf")
    # # debugging
    # observeEvent(env$rf$proj_name, {
    #     pprint(env$rf$proj_name)
    # })
    
    ## feedback on sub-project name
    output$feedback_proj_name <- renderUI({
        req(env$ap$proj_dir)
        req(env$rf$proj_name)
        req(env$rf$run_counter)
        
        existing_subproj <- list.dirs(
            env$ap$proj_dir, recursive = FALSE, full.names = FALSE
        )
        
        if(env$rf$proj_name %in% existing_subproj) {
            tagList(tags$div(
                h4(
                    icon("warning"), 
                    "Sub-project already exists,",
                    "be advised that new analysis may erase", 
                    "previous results."
                ),
                style = "color: #F89406;"
            ))
        }
    })
    
    ## get some user input
    observe({
        req(env$ap$file_modif) # react to project file modification
        
        env$rf$mode <- input$run_mode
        env$rf$min_node_size <- 0
        env$rf$linear <- input$linear
    })
    
    ## update number of tree
    observeEvent(c(env$ap$file_modif, input$n_tree), {
        feedbackWarning(
            "n_tree", !isTruthy(input$n_tree), "Missing input."
        )
        req(input$n_tree)
        env$rf$n_tree <- as.integer(input$n_tree)
    }, ignoreNULL = FALSE)
    
    ## update number of additional noise columns
    observeEvent(c(env$ap$file_modif, input$n_noise_columns), {
        feedbackWarning(
            "n_noise_columns", !isTruthy(input$n_noise_columns), 
            "Missing input."
        )
        req(input$n_noise_columns)
        env$rf$n_noise_columns <- as.integer(input$n_noise_columns)
    }, ignoreNULL = FALSE)
    
    ## update percentage of explained variance for PLS
    observeEvent(c(env$ap$file_modif, input$pls_max_var, env$rf$mode), {
        req(env$rf$mode == "param_estim")
        feedbackWarning(
            "pls_max_var", !isTruthy(input$pls_max_var), 
            "Missing input."
        )
        req(input$pls_max_var)
        env$rf$pls_max_var <- input$pls_max_var
    }, ignoreNULL = FALSE)
    
    ## update number of out-of-bags
    observeEvent(c(env$ap$file_modif, input$noob, env$rf$mode), {
        req(env$rf$mode == "param_estim")
        feedbackWarning(
            "noob", !isTruthy(input$noob), "Missing input."
        )
        req(input$noob)
        env$rf$noob <- input$noob
    }, ignoreNULL = FALSE)
    
    ## render chosen scenario choice
    output$input_chosen_scenario <- renderUI({
        
        req(env$ap$file_modif)
        req(env$rf$mode == "param_estim")
        req(env$ap$reftable_check$valid)
        req(env$ap$reftable_check$n_scen)
        
        selectInput(
            ns("chosen_scenario"), 
            label = "Choose a scenario", 
            choices = 1:env$ap$reftable_check$n_scen,
            selected = 1
        )
    })
    
    ## check and update chosen scenario
    observeEvent(c(env$ap$file_modif, input$chosen_scenario, env$rf$mode), {
        req(env$rf$mode == "param_estim")
        
        feedbackWarning(
            "chosen_scenario", !isTruthy(input$chosen_scenario), 
            "Missing input."
        )
        req(input$chosen_scenario)
        env$rf$chosen_scenario <- as.integer(input$chosen_scenario)
    }, ignoreNULL = FALSE)
    
    ## check and update grouping
    observeEvent(c(
        env$ap$file_modif, input$grouping, env$rf$mode, 
        env$ap$reftable_check$n_scen
    ), {
        req(env$rf$mode == "model_choice")
        req(env$ap$reftable_check$valid)
        req(env$ap$reftable_check$n_scen)
        
        # check
        group_check <- parse_abcranger_group(
            input$grouping, env$ap$reftable_check$n_scen
        )
        
        # update env if valid or empty
        if(!isTruthy(input$grouping) || !group_check$valid) {
            env$rf$grouping <- NULL
        } else {
            env$rf$grouping <- input$grouping
        }
        
        # feedback
        output$feedback_grouping <- renderUI({
            req(env$rf$mode == "model_choice")
            req(!group_check$valid)
            
            tags$p(tags$div(
                icon("warning"), "Issue with scenario grouping/selection:",
                do.call(
                    tags$ul,
                    lapply(group_check$msg, tags$li)
                ),
                style = "color: #F89406;"
            ))
        })
        
    }, ignoreNULL = FALSE)
    
    ## help on grouping
    output$help_grouping <- renderUI({
        req(env$rf$mode == "model_choice")
        req(env$ap$reftable_check$valid)
        req(env$ap$reftable_check$n_scen)
        
        helpText(
            icon("info-circle"), "Here you have",
            tags$b(as.character(env$ap$reftable_check$n_scen)),
            ifelse(env$ap$reftable_check$n_scen > 1, "scenarii.", "scenario."),
            "Leave blank to use all available scenarii",
            "without grouping/selection."
        )
    })
    
    ## check and update chosen parameter
    observeEvent(c(
        env$ap$file_modif, input$parameter, env$rf$mode
    ), {
        req(env$rf$mode == "param_estim")
        req(input$parameter)
        req(local$possible_param)
        
        # possible parameter pttrn
        possible_param <- str_c(
            "(",
            str_c(local$possible_param, collapse = "|"),
            ")"
        )
        # combination of parameters
        pttrn <- str_c(
            "^", possible_param, 
            "([\\+\\-\\*/]", possible_param, ")?$"
        )
        # check
        local$param_check <- str_detect(input$parameter, pttrn)
        
        req(local$param_check)
        env$rf$parameter <- input$parameter
        
    }, ignoreNULL = FALSE)
    
    # check parameter input
    output$feedback_parameter <- renderUI({
        req(env$rf$mode == "param_estim")
        
        if(!isTruthy(input$parameter)) {
            tags$p(tags$div(
                icon("warning"), "Missing parameter.",
                style = "color: #F89406;"
            ))
        } else {
            req(is.logical(local$param_check))
            if(local$param_check) {
                helpText(
                    icon("check"),
                    "Parameter to estimate is ok."
                )
            } else {
                tags$p(tags$div(
                    icon("warning"),
                    "Issue with provided parameter",
                    "or combination of parameters",
                    "(probably one or more parameters not existing", 
                    "in the selected scenario).",
                    style = "color: #F89406;"
                ))
            }
        }
    })
    
    # update list of possible parameters (depending on chosen scenario)
    observeEvent(c(
        env$ap$file_modif, env$ap$header_check$scenario_list, 
        env$ap$locus_type, env$ts$mss_reftab_colname, env$rf$mode, 
        env$rf$chosen_scenario
    ), {
        req(env$rf$mode == "param_estim")
        req(env$ap$locus_type)
        req(env$ap$header_check$scenario_list)
        req(env$ap$header_check$n_param_list)
        req(env$rf$chosen_scenario)
        
        # selected scenario
        selected_scenario <- unlist(split(
            env$ap$header_check$scenario_list[env$rf$chosen_scenario], "\n"
        ))
        
        # extract scenario parameter
        tmp_possible_param <- sort(unique(unlist(str_extract_all(
            selected_scenario, single_param_regex()
        ))))
        
        tmp_possible_param <- tmp_possible_param[
            !str_to_lower(tmp_possible_param) %in% 
                str_to_lower(c("merge", "sample", "split", "varNe"))]
        
        # pprint(tmp_possible_param)
        
        # # check number of parameter
        # expected_n_param <- 
        #     env$ap$header_check$n_param_list[env$rf$chosen_scenario]
        # n_param <- length(tmp_possible_param)
        # pprint(expected_n_param)
        # pprint(n_param)
        
        # update local env
        local$possible_param <- tmp_possible_param
        
        # additional group prior parameter for MSS data
        if(
            (env$ap$locus_type == "mss") && 
            isTruthy(env$ts$mss_reftab_colname)
        ) {
            local$possible_param <- c(
                local$possible_param, env$ts$mss_reftab_colname
            )
        }
    })
    
    # possible parameters
    output$possible_parameters <- renderUI({
        req(local$possible_param)
        
        helpText(
            tags$p(
                icon("info-circle"), 
                "You can use one of the following parameter",
                "or an arithmetic combination of them, such",
                "as division, addition or multiplication of",
                "two existing parameters:",
            ),
            tags$p(
                tags$div(
                    style = "column-count:2;",
                    do.call(tags$ul, lapply(local$possible_param, tags$li))
                )
            ),
            tags$p(
                tags$i("Example of arithmetic possible combinations:"),
                tags$code("t/N"), "or", tags$code("t1+t2"),
                "or", tags$code("N*µmic_1"),
                "(to be adapted with the name of the parameters",
                "in your model)."
            )
        )
    })
    
    ## update max n_rec
    observeEvent(c(
        env$ap$reftable_check$n_rec_scen, env$rf$mode, env$rf$grouping,
        env$rf$chosen_scenario
    ), {
        req(env$ap$reftable_check$valid)
        req(env$ap$reftable_check$n_rec)
        req(env$ap$reftable_check$n_rec_scen)
        req(env$rf$mode)
        
        if(env$rf$mode == "param_estim") {
            req(env$rf$chosen_scenario)
            local$max_n_rec <- 
                env$ap$reftable_check$n_rec_scen[env$rf$chosen_scenario]
        } else if(env$rf$mode == "model_choice") {
            local$max_n_rec <- env$ap$reftable_check$n_rec
            if(isTruthy(env$rf$grouping)) {
                selected_scenario <- as.integer(unlist(str_extract_all(
                    env$rf$grouping, "[0-9]+"
                )))
                if(length(selected_scenario) > 0) {
                    local$max_n_rec <- sum(
                        env$ap$reftable_check$n_rec_scen[
                            selected_scenario
                        ]
                    )
                }
            }
        }
        
        updateNumericInput(
            session, "n_rec",
            max = local$max_n_rec
        )
    }, ignoreNULL = FALSE)
    
    ## update n_rec
    observeEvent(c(
        env$ap$file_modif, input$n_rec, env$rf$mode, local$max_n_rec
    ), {
        feedbackWarning(
            "n_rec", !isTruthy(input$n_rec), 
            "Missing input."
        )
        req(input$n_rec)
        env$rf$n_rec <- as.integer(input$n_rec)
    }, ignoreNULL = FALSE)
    
    ## help regarding n_rec
    output$help_nrec <- renderUI({
        req(local$max_n_rec)
        
        helpText(
            icon("info-circle"), "Here", tags$code("0"),
            "(default) means using all simulated data available",
            "for the selected scenario/scenarii,", "i.e.", 
            tags$b(as.character(local$max_n_rec)), "simulations."
        )
    })
    
    ## feedback regarding n_rec
    output$feedback_nrec <- renderUI({
        req(local$max_n_rec < input$n_rec)
        
        tags$p(tags$div(
            icon("warning"),
            "The number of samples in the training set to be used is larger",
            "than the number of simulated data available", 
            "for the selected scenario/scenarii",
            style = "color: #F89406;"
        ))
    })
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
            color = "primary"
        ),
        progressBar(
            id = ns("rf_progress"),
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
            style = "width:60vw; overflow:scroll; overflow-y:scroll; height:130px; resize: both;"
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
                              sub_proj_name = reactive({NULL}),
                              # parameters
                              group = reactive({NULL}),
                              chosen_scenario = reactive({NULL}),
                              linear = reactive({NULL}),
                              min_node_size = 0,
                              n_rec = reactive({NULL}),
                              n_tree = reactive({NULL}),
                              n_noise_columns = reactive({NULL}),
                              noob = reactive({NULL}),
                              parameter = reactive({NULL}),
                              pls_max_var = reactive({NULL}),
                              run_mode = reactive({NULL})) {
    
    # namespace
    ns <- session$ns
    
    # max number of rows in log
    nlog <- 5
    
    # init local
    local <- reactiveValues(
        abcranger_run_process = NULL,
        abcranger_run_result = NULL,
        feedback = NULL,
        log_file_content = NULL,
        log_start_line = NULL,
        current_iter = 0,
        total_iter = 0,
        # input
        proj_dir = NULL,
        proj_file_list = NULL,
        proj_ready = NULL,
        valid_proj = NULL,
        sub_proj_name = NULL,
        # parameters
        group = NULL,
        chosen_scenario = NULL,
        linear = NULL,
        min_node_size = 0,
        n_rec = 0,
        n_tree = NULL,
        n_noise_columns = NULL,
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
        local$sub_proj_name <- sub_proj_name()
        
        local$group <- group()
        local$min_node_size <- min_node_size()
        local$n_noise_columns <- n_noise_columns()
        local$linear <- linear()
        local$pls_max_var <- pls_max_var()
        local$n_rec <- n_rec()
        local$n_tree <- n_tree()
        local$chosen_scenario <- chosen_scenario()
        local$noob <- noob()
        local$parameter <- parameter()
        local$run_mode <- run_mode()
    })
    
    ## required files
    required_files <- c("headerRF.txt", "reftableRF.bin", "statobsRF.txt")
    
    ## read log file
    log_file_content <- function() return(rep("", nlog))
    observeEvent(local$proj_dir, {
        req(local$proj_dir)
        log_file_content <<- reactiveFileReader(
            1000, session,
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
    #     pprint("log file content")
    #     pprint(tail(local$log_file_content, 5))
    # })
    
    ## run
    observeEvent(input$run, {
        
        # # debugging
        # pprint("run set up?")
        # pprint("valid proj ?")
        # pprint(local$valid_proj)
        # pprint("proj ready ?")
        # pprint(local$proj_ready)
        # pprint("proj file list")
        # pprint(local$proj_file_list)
        
        req(!is.null(local$proj_ready))
        req(!is.null(local$valid_proj))
        req(!is.null(local$proj_file_list))
        req(length(local$proj_file_list) > 0)
        req(local$n_tree)
        
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
            ## prepare run
            req(is.null(local$abcranger_run_process))
            
            ## init progress bar
            updateProgressBar(
                session = session,
                id = "rf_progress",
                value = 0, total = local$n_tree,
                title = "Running RF:"
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
                    "Check analysis name and run settings."
                )
            } else {
                ## ready to run
                
                # debugging
                # pprint("check options")
                # pprint(getOption("diyabcGUI"))
                # pprint(getOption("shiny.maxRequestSize"))
                
                ## reset log
                local$log_start_line = NULL
                local$current_iter = 0
                local$total_iter = local$n_tree
                
                local$feedback <- helpText(
                    icon("spinner", class = "fa-spin"),
                    "Run in progress."
                )
                
                # # debugging
                # pprint("run")
                # pprint("valid proj ?")
                # pprint(local$valid_proj)
                # pprint("proj ready ?")
                # pprint(local$proj_ready)
                # pprint("proj file list")
                # pprint(local$proj_file_list)
                # 
                # pprint("abcranger args")
                # pprint("proj_dir =")
                # pprint(local$proj_dir)
                # pprint("run mode =")
                # pprint(local$run_mode)
                # 
                # pprint("n_rec =")
                # pprint(local$n_rec)
                # 
                # pprint("min_node_size =")
                # pprint(local$min_node_size)
                # pprint("n_noise_columns =")
                # pprint(local$n_noise_columns)
                # pprint("linear =")
                # pprint(local$linear)
                # pprint("n_tree")
                # pprint(local$n_tree)
                # 
                # if(local$run_mode == "param_estim") {
                #     pprint("chosen_scen =")
                #     pprint(local$chosen_scenario)
                #     pprint("pls_max_var")
                #     pprint(local$pls_max_var)
                #     pprint("noob =")
                #     pprint(local$noob)
                #     pprint("parameter =")
                #     pprint(local$parameter)
                # }
                
                req(!is.null(local$proj_dir))
                req(!is.null(local$run_mode))
                req(!is.null(local$n_rec))
                req(!is.null(local$min_node_size))
                req(!is.null(local$n_noise_columns))
                req(!is.null(local$linear))
                req(!is.null(local$n_tree))
                
                if(local$run_mode == "param_estim") {
                    req(!is.null(local$chosen_scenario))
                    req(!is.null(local$pls_max_var))
                    req(!is.null(local$noob))
                    req(!is.null(local$parameter))
                    req(str_length(local$parameter) > 0)
                }
                
                logging("running abcranger")
                local$abcranger_run_process <- abcranger_run(
                    local$proj_dir, local$run_mode, local$n_rec, 
                    local$min_node_size, local$n_tree, local$n_noise_columns, 
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
        
        req(local$n_tree)
        
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
        if(local$abcranger_run_result == 0) {
            local$feedback <- helpText(
                icon("check"), "RF run succeeded."
            )
            updateProgressBar(
                session = session,
                id = "simu_progress",
                value = local$n_tree, total = local$n_tree,
                title = "Running RF:"
            )
            showNotification(
                id = ns("run_ok"),
                duration = 5,
                closeButton = TRUE,
                type = "message",
                tagList(
                    tags$p(
                        icon("check"),
                        "RF run succeeded."
                    )
                )
            )
            ## post processing
            req(!is.null(local$proj_dir))
            req(!is.null(local$run_mode))
            req(!is.null(local$sub_proj_name))
            prefix <- switch(
                local$run_mode,
                "param_estim" = "estimparam_out",
                "model_choice" = "modelchoice_out"
            )
            abcranger_postprocess(
                local$proj_dir, local$proj_dir, 
                run_mode = local$run_mode, prefix = prefix, 
                sub_proj_name = local$sub_proj_name,
                param = local$parameter
            )
            
        } else if(local$abcranger_run_result == -1000) {
            ## stopped run
            local$feedback <- helpText(
                icon("warning"), "RF run was stopped."
            )
            showNotification(
                id = ns("stop_run"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"),
                        "RF run was stopped."
                    )
                )
            )
        } else {
            ## error during run
            local$feedback <- helpText(
                icon("warning"), "Issues with RF run (see log panel)."
            )
            showNotification(
                id = ns("run_not_ok"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"),
                        "A problem happened during RF run."
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
        
        req(local$n_tree)
        req(local$run_mode)
        
        tmp_computed_0 <- switch(
            local$run_mode,
            "param_estim" = "growing random forest",
            "model_choice" = "growing first random forest"
        )
        
        req(!is.null(local$log_file_content))
        req(length(local$log_file_content) > 0)
        
        ## FIXME
        step <- c("read", "computed_0", "computed_1")
        
        ## parse log
        if(is.null(local$log_start_line)) {
            pttrn <- str_c(
                "^(",
                str_c(step, collapse = "|"),
                ")"
            )
            
            find_pttrn <- str_detect(local$log_file_content, pttrn)
            # logging("found pattern =", sum(find_pttrn))
            if(any(find_pttrn)) {
                local$log_start_line <- tail(which(find_pttrn), 1)
            }
        } else {
            last_message <- tail(
                local$log_file_content,
                length(local$log_file_content) - local$log_start_line
            )
            
            pttrn <- str_c(
                "^(",
                str_c(step, collapse = "|"),
                ")"
            )
            find_iter <- str_detect(last_message, pttrn)
            
            if(any(find_iter)) {
                current_msg <- tail(last_message[find_iter], 1)
                
                tmp_step <- str_extract(current_msg, pttrn)
                tmp_log <- switch(
                    tmp_step,
                    "read" = "Reading training set",
                    "computed_0" = tmp_computed_0,
                    "computed_1" = "growing second random forest"
                )
                
                # current iteration
                pttrn <- "[0-9]+(?=/)"
                if(str_detect(current_msg, pttrn)) {
                    local$current_iter <- as.integer(
                        str_extract(current_msg, pttrn)
                    )
                }
                
                # total iteration
                pttrn <- "(?<=/)[0-9]+"
                if(str_detect(current_msg, pttrn)) {
                    local$total_iter <- as.integer(
                        str_extract(current_msg, pttrn)
                    )
                }
                
                # logging(
                #     "Iteration:", 
                #     str_c(
                #         tmp_log,
                #         str_c(local$current_iter, "/", local$total_iter),
                #         sep = " "
                #     )
                # )
                
                updateProgressBar(
                    session = session,
                    id = "rf_progress",
                    value = local$current_iter, 
                    total = local$total_iter,
                    title = str_c("Running RF:", tmp_log, sep = " ")
                )
            }
        }
    })
    
    ## feedback
    observeEvent(local$feedback, {
        output$feedback <- renderUI({
            local$feedback
        })
    })
    
}