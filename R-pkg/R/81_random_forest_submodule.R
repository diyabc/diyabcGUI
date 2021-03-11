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
        file_ready = FALSE,
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
    #     pprint("proj_file_list")
    #     pprint(local$proj_file_list)
    # })
    
    # check required files
    observe({
        req(!is.null(local$proj_file_list))
        local$file_ready <- all(required_files %in% local$proj_file_list)
    })
    
    # enable control
    output$enable_control <- renderUI({
        req(!is.null(local$valid_proj))
        req(!is.null(local$file_ready))
        
        if(!local$valid_proj) {
            helpText(
                icon("warning"), "Project set up is not valid."
            )
        } else if(!local$file_ready) {
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
        sub_proj_name = reactive(rf_param$proj_name),
        group = reactive(rf_param$group),
        min_node_size = reactive(rf_param$min_node_size),
        noise_columns = reactive(rf_param$noise_columns),
        linear = reactive(rf_param$linear),
        pls_max_var = reactive(rf_param$pls_max_var),
        n_tree = reactive(rf_param$n_tree),
        n_rec = reactive(rf_param$n_rec),
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
        br(),
        actionBttn(
            inputId = ns("check"),
            label = "Check input",
            style = "fill",
            block = TRUE,
            color = "success"
        ),
        helpText(
            icon("clock"), "Checking the input may take some time."
        ),
        br(),
        uiOutput(ns("feedback_context")),
        hr(),
        proj_name_ui(ns("proj_name_setup"), 
                     label = "Analysis (sub-project) name"),
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
        ## removed
        # numericInput(
        #     ns("min_node_size"),
        #     label = "Minimal node size",
        #     min = 0,
        #     value = 0
        # ),
        # helpText(
        #     "0 means 1 for classification or 5 for regression."
        # ),
        conditionalPanel(
            condition = "input.run_mode == 'param_estim'", ns = ns,
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
            )
        ),
        conditionalPanel(
            condition = "input.run_mode == 'model_choice'", ns = ns,
            textInput(
                ns("group"),
                label = "Scenario grouping and selection"
            ) %>% 
                helper(type = "markdown", 
                       content = "scenario_grouping_selection"),
            uiOutput(ns("help_group")),
            uiOutput(ns("feedback_group"))
        ),
        numericInput(
            ns("n_rec"),
            label = "Number of samples in the training set to consider",
            value = 0, min = 0
        ),
        uiOutput(ns("feedback_nrec")),
        numericInput(
            ns("noise_columns"),
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
                "Percentage of maximum explained Y-variance", 
                "for retaining pls axis"
            ),
            numericInput(
                ns("noob"),
                label = "Number of oob testing samples",
                value = 1000,
                min = 1
            ) %>% 
                helper(type = "markdown", 
                       content = "noob_parameter")
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
rf_parameter_server <- function(input, output, session,
                                proj_dir = reactive({NULL}),
                                proj_header_file = reactive({NULL}),
                                locus_type = reactive({NULL})) {
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        valid_proj_name = FALSE,
        param_ready = TRUE,
        valid_group = FALSE,
        param_list = NULL,
        updated_param_list = NULL,
        proj_header_content = NULL,
        ref_table_size = 0,
        n_rec_per_scenario = list(),
        n_param = list(),
        n_stat = 0,
        n_scenario = 0,
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
        proj_name = NULL,
        # parameters
        chosen_scenario = NULL,
        group = NULL,
        linear = NULL,
        min_node_size = 0,
        n_rec = 0,
        n_tree = NULL,
        noise_columns = NULL,
        noob = NULL,
        parameter = NULL,
        pls_max_var = NULL,
        run_mode = NULL
    )
    
    ## project name
    proj_name_setup <- callModule(proj_name_server, "proj_name_setup")
    
    observeEvent(proj_name_setup$proj_name, {
        req(proj_name_setup$proj_name)
        out$proj_name <- proj_name_setup$proj_name
    })
    
    observeEvent(proj_name_setup$valid_proj_name, {
        req(!is.null(proj_name_setup$valid_proj_name))
        local$valid_proj_name <- proj_name_setup$valid_proj_name
    })
        
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
    #     pprint("proj_header_file")
    #     pprint(local$proj_header_file)
    # })
    
    ## context
    output$feedback_context <- renderUI({
        req(!is.null(local$ref_table_size))
        req(!is.null(local$n_param))
        req(!is.null(local$n_stat))
        req(!is.null(local$n_scenario))
        req(!is.null(local$n_rec_per_scenario))
        # ref_table_size = 0
        # n_param = 0
        # n_stat = 0
        # n_scenario = 0
        
        tmp_n_rec_per_scenario <- ""
        if(length(local$n_rec_per_scenario)) {
            if(local$n_scenario == length(local$n_rec_per_scenario)) {
                tmp_n_rec_per_scenario <- do.call(
                    tags$ul,
                    lapply(
                        1:local$n_scenario,
                        function(ind) {
                            return(
                                tags$li(
                                    tags$b(local$n_rec_per_scenario[ind]), 
                                    "for scenario",
                                    tags$b(as.character(ind))
                                )
                            )
                        }
                    )
                )
            }
        }
        
        tmp_ref_table_size <- tags$i("unknown")
        if(local$ref_table_size > 0) {
            tmp_ref_table_size <- tags$b(local$ref_table_size)
        }
        
        tmp_n_scenario <- tags$i("unknown")
        if(local$n_scenario > 0) {
            tmp_n_scenario <- tags$b(local$n_scenario)
        }
        
        tmp_n_param <- tags$i("unknown")
        if(length(local$n_param) > 0) {
            tmp_n_param <- do.call(
                tags$ul,
                lapply(
                    1:length(local$n_param),
                    function(ind) {
                        return(
                            tags$li(
                                tags$b(local$n_param[ind]), 
                                "for scenario",
                                tags$b(as.character(ind))
                            )
                        )
                    }
                )
            )
        }
        
        tmp_n_stat <- tags$i("unknown")
        if(local$n_stat > 0) {
            tmp_n_stat <- tags$b(local$n_stat)
        }
        
        tag_list <- list(
            tags$div(
                "Number of scenario:",
                tmp_n_scenario
            ),
            tags$div(
                "Total number of simulated datasets:",
                tmp_ref_table_size,
                tmp_n_rec_per_scenario
            ),
            tags$div(
                "Number of parameters:",
                tmp_n_param
            ),
            tags$div(
                "Number of summary statistics:",
                tmp_n_stat
            )
        )
        
        tagList(
            do.call(
                tags$ul,
                lapply(
                    tag_list,
                    tags$li
                )
            )
        )
    })
    
    ## feedback regarding n_rec
    output$feedback_nrec <- renderUI({
        
        txt <- tagList(
            tags$code("0"), 
            "means using the full training data set,",
            "i.e.", tags$code("0"), 
            "is equivalent to the total number of",
            "available simulated datasets."
        )
        
        if(isTruthy(out$run_mode)) {
            if(out$run_mode == "param_estim") {
                if(isTruthy(out$chosen_scenario) & 
                   isTruthy(local$n_rec_per_scenario)) {
                    txt <- tagList(
                        tags$code("0"), 
                        "means using the full training data set,",
                        "i.e.", tags$code("0"), 
                        "is equivalent to the total number of",
                        "simulated datasets available for the chosen scenario:",
                        tags$b(local$n_rec_per_scenario[
                            as.integer(out$chosen_scenario)
                            ])
                    )
                } else {
                    txt <- tagList(
                        tags$code("0"), 
                        "means using the full training data set,",
                        "i.e.", tags$code("0"), 
                        "is equivalent to the total number of",
                        "simulated datasets available for the chosen scenario."
                    )
                }
            } else {
                if(isTruthy(input$group) && local$valid_group &&
                   isTruthy(local$n_rec_per_scenario)) {
                    
                    group_id <- as.integer(unlist(str_extract_all(
                        input$group, "[0-9]+"
                    )))
                    
                    n_simu <- sum(as.numeric(
                        local$n_rec_per_scenario[group_id]
                    ))
                    
                    txt <- tagList(
                        tags$code("0"), 
                        "means using the full training data set,",
                        "i.e.", tags$code("0"), 
                        "is equivalent to the total number of",
                        "simulated datasets available for the chosen scenario:",
                        tags$b(as.character(n_simu))
                    )
                    
                } else if(isTruthy(local$ref_table_size) && 
                   (local$ref_table_size > 0)) {
                    txt <- tagList(
                        tags$code("0"), 
                        "means using the full training data set,",
                        "i.e.", tags$code("0"), 
                        "is equivalent to the total number of",
                        "simulated datasets:", tags$b(local$ref_table_size)
                    )
                }
            }
        }
        
        helpText(txt)
    })
    
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
        
        local$param_list <- lapply(
            file_check$raw_prior_list,
            function(item) {
                return(
                    str_extract(
                        item,
                        single_param_regex()
                    )
                )
            }
        )
        
        # possible groups
        output$help_group <- renderUI({
            if(isTruthy(length(file_check$raw_scenario_list))) {
                helpText(
                    "Here you have", 
                    tags$b(as.character(length(file_check$raw_scenario_list))),
                    "scenarii.",
                    "Leave blank to use all available scenarii without groups."
                )
            } else {
                "Leave blank to use all available scenarii without groups."
            }
            
        })
    })
    
    # possible parameters
    output$possible_parameters <- renderUI({
        
        tmp_param_list <- local$param_list
        
        if(isTruthy(local$proj_header_content) &&
           isTruthy(local$proj_header_content$raw_scenario_list) &&
           isTruthy(input$chosen_scenario) &&
           (input$chosen_scenario <= 
                length(local$proj_header_content$raw_scenario_list))) {
            
            raw_scenario_list <- local$proj_header_content$raw_scenario_list
            chosen_scenario <- input$chosen_scenario
            
            which_param <- Reduce("rbind", lapply(
                tmp_param_list,
                function(param) str_detect(raw_scenario_list, param)
            ))
            
            tmp_param_list <- tmp_param_list[which_param[,chosen_scenario]]
        }
        
        local$updated_param_list <- tmp_param_list
        
        helpText(
            "You can use one of the following parameter",
            "or an arithmetic combination of them, such",
            "as division, addition or multiplication of",
            "two existing parameters. like 't/N' or 't1+t2'.",
            tags$div(
                style = "column-count:2;",
                do.call(tags$ul, lapply(tmp_param_list, tags$li))
            )
        )
    })
    
    # check context
    observeEvent(input$check, {
        req(!is.null(local$proj_dir))
        
        # run diyabc to parse ref table
        tmp_diyabc_run <- diyabc_run_trainset_simu(
            local$proj_dir, n_run = 0, run_prior_check = FALSE
        )
        tmp_diyabc_run$wait()
        tmp_diyabc_result <- tmp_diyabc_run$get_exit_status()
        
        # check result
        if(tmp_diyabc_result == 0) {
            tmp_diyabc_log <- readLines(
                file.path(local$proj_dir, "diyabc_run_call.log"),
                warn = FALSE
            )
            
            # extract info
            pttrn <- str_c(
                "DEBUT +",
                "nrecneeded=[0-9]+ +",
                "rt\\.nrec=[0-9]+ +", 
                "rt\\.nstat=[0-9]+ +", 
                "nscenarios=[0-9]+ *"
            )
            find_pttrn <- str_detect(tmp_diyabc_log, pttrn)
            if(any(find_pttrn)) {
                pttrn_match <- tmp_diyabc_log[find_pttrn]
                
                local$ref_table_size <- as.integer(str_extract(
                    pttrn_match,
                    "(?<=rt\\.nrec=)[0-9]+"
                ))
                
                local$n_scenario <- as.integer(str_extract(
                    pttrn_match,
                    "(?<=nscenarios=)[0-9]+"
                ))
                
                local$n_stat <- as.integer(str_extract(
                    pttrn_match,
                    "(?<=rt\\.nstat=)[0-9]+"
                ))
            } else {
                local$ref_table_size <- 0
                local$n_stat <- 0
                local$n_scenario <- 0
            }
            
            # number of parameters
            pttrn <- "scenario\\[i\\].nparam=[0-9]+"
            find_pttrn <- str_detect(tmp_diyabc_log, pttrn)
            if(any(find_pttrn)) {
                pttrn_match <- tmp_diyabc_log[find_pttrn]
                pttrn <- "(?<=nparam=)[0-9]+"
                local$n_param <- str_extract(pttrn_match, pttrn)
            } else {
                local$n_param <- list()
            }
            
            # sample size per scenario
            pttrn <- "nrecscen\\[[0-9]+\\] = [0-9]+"
            find_pttrn <- str_detect(tmp_diyabc_log, pttrn)
            if(any(find_pttrn)) {
                pttrn_match <- tmp_diyabc_log[find_pttrn]
                pttrn <- "(?<= = )[0-9]+"
                local$n_rec_per_scenario <- str_extract(pttrn_match, pttrn)
            } else {
                local$n_rec_per_scenario <- list()
            }
        } else {
            local$ref_table_size <- 0
            local$n_param <- list()
            local$n_stat <- 0
            local$n_scenario <- 0
            local$n_rec_per_scenario <- list()
        }
        
        # clean up
        cleanup_diyabc_run(local$proj_dir)
    })
    
    ## update max n_rec
    observeEvent(local$ref_table_size, {
        req(!is.null(local$ref_table_size))
        req(local$ref_table_size > 0)
        updateNumericInput(
            session,
            "n_rec",
            max = local$ref_table_size
        )
    })
    
    # check and feedback groups
    observeEvent(input$group, {
        req(input$group)
        req(local$proj_header_content)
        
        file_check <- local$proj_header_content
        
        # check
        group_check <- parse_abcranger_group(
            input$group,
            length(file_check$raw_scenario_list)
        )
        local$valid_group <- group_check$valid
        
        # save check
        local$param_ready <- group_check$valid
        
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
                if(length(local$updated_param_list) > 0) {
                    possible_param <- str_c(
                        "(",
                        str_c(
                            unlist(local$updated_param_list), 
                            collapse = "|"
                        ),
                        ")"
                    )
                        
                    pttrn <- str_c(
                        "^", possible_param, 
                        "([\\+\\-\\*/]", possible_param, ")?$"
                    )
                    
                    if(!str_detect(input$parameter, pttrn)) {
                        local$param_ready <- FALSE
                        helpText(
                            icon("warning"),
                            "Issue with provided parameter",
                            "or combination of parameters."
                        )
                    } else {
                        local$param_ready <- TRUE
                        helpText(
                            icon("check"),
                            "Parameter to estimate is ok."
                        )
                    }
                } else {
                    NULL
                }
            }
        }
    })
    
    # proj not ready if no parameter in param_estim mode
    observe({
        req(input$run_mode)
        if(input$run_mode == "param_estim") {
            if(is.null(input$parameter)) {
                local$param_ready <- FALSE
            } else if(str_length(input$parameter) == 0) {
                local$param_ready <- FALSE
            } else {
                local$param_ready <- TRUE
            }
        } else {
            local$param_ready <- TRUE
        }
    })
    
    observe({
        req(!is.null(local$param_ready))
        req(!is.null(local$valid_proj_name))
        out$param_ready <- local$param_ready & local$valid_proj_name
    })
    
    # output
    observe({
        out$group <- input$group
        out$linear <- input$linear
        out$chosen_scenario <- input$chosen_scenario
        out$run_mode <- input$run_mode
        out$min_node_size <- 0
        out$n_rec <- input$n_rec
        out$n_tree <- input$n_tree
        out$noise_columns <- input$noise_columns
        out$noob <- input$noob
        out$parameter <- input$parameter
        out$pls_max_var <- input$pls_max_var
    })
    
    # observe({
    #     pprint("------- run mode = ")
    #     pprint(out$run_mode)
    # })
    
    # # debugging
    # observe({
    #     pprint("--------------------------")
    #     pprint("RF param")
    #     pprint(reactiveValuesToList(out))
    #     pprint("--------------------------")
    # })
    
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
                              noise_columns = reactive({NULL}),
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
        local$sub_proj_name <- sub_proj_name()
        
        local$group <- group()
        local$min_node_size <- min_node_size()
        local$noise_columns <- noise_columns()
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
                # pprint("noise_columns =")
                # pprint(local$noise_columns)
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
                req(!is.null(local$noise_columns))
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
        
        pprint("abcranger run process")
        pprint(local$abcranger_run_process)
        
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