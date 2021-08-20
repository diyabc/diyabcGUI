#' Training set simulation sub-module ui
#' @keywords internal
#' @author Ghislain Durif
train_set_simu_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("train_set_simu"))
    )
}

#' Training set simulation sub-module server
#' @keywords internal
#' @author Ghislain Durif
train_set_simu_server <- function(input, output, session) {
    
    # namespace
    ns <- session$ns
    
    ## update environment if existing proj
    observeEvent({
        c(env$ap$proj_dir, env$ap$proj_type, 
          env$ap$locus_type, env$ap$seq_mode, 
          env$ap$file_upload, env$ap$data_check, env$ap$header_check)
    }, {
        ### RESET
        # list of historical models
        env$ts$scenario_list <- NULL
        # list number of parameters per model
        env$ts$n_param <- NULL
        # list of model priors (discrete probabilities)
        env$ts$model_prior <- NULL
        # list of historical model parameters (name, type, priors)
        env$ts$prior_list <- NULL
        # list of conditions on historical parameters
        env$ts$cond_list <- NULL
        # table of loci description
        env$ts$locus_desc <- NULL
        # number of loci group
        env$ts$n_group <- NULL
        # list of group priors for MSS data
        env$ts$group_prior_list <- NULL
        # specific ref table column names for MSS data
        env$ts$mss_reftab_colname <- NULL
        ### requirements
        req(env$ap$proj_dir)
        req(env$ap$proj_type)
        req(env$ap$locus_type)
        req(env$ap$seq_mode)
        req(env$ap$proj_name)
        req(env$ap$header_check)
        req(env$ap$header_check$valid)
        ### UPDATE ENV
        # list of historical models
        env$ts$scenario_list <- env$ap$header_check$scenario_list
        # total number of parameters in all model
        env$ts$n_param <- env$ap$header_check$n_param
        # list of number of parameters per model
        env$ts$n_param_list <- env$ap$header_check$n_param_list
        # list of model priors (discrete probabilities)
        env$ts$model_prior <- NULL
        # table of historical model parameters (name, type, priors)
        env$ts$prior_list <- env$ap$header_check$prior_list
        # list of conditions on historical parameters
        env$ts$cond_list <- env$ap$header_check$cond_list
        # table of loci description
        env$ts$locus_desc <- env$ap$header_check$locus_desc
        # number of loci group
        env$ts$n_group <- env$ap$header_check$n_group
        # list of group priors for MSS data
        env$ts$group_prior_list <- env$ap$header_check$group_prior_list
        # specific ref table column names for MSS data
        env$ts$mss_reftab_colname <- get_mss_reftab_colname(
            env$ts$group_prior_list, env$ts$locus_desc
        )
    })
    
    # panel output
    observeEvent({
        c(env$ap$proj_dir, env$ap$proj_type, 
          env$ap$locus_type, env$ap$seq_mode, 
          env$ap$file_upload, env$ap$data_check)
    },{
        output$train_set_simu <- renderUI({
            
            # pprint(env$ap$proj_name)
            # pprint(env$ap$data_file)
            # pprint(env$ap$data_check)
            # pprint(env$ap$header_check)
            # pprint(env$ap$reftable_check)
            # pprint(env$ap$statobs_check)
            
            req(env$ap$proj_dir)
            req(env$ap$proj_type)
            req(env$ap$locus_type)
            req(env$ap$seq_mode)
            if(isTruthy(env$ap$proj_name) && 
               isTruthy(env$ap$data_file) &&
               isTruthy(env$ap$data_check) &&
               isTruthy(env$ap$data_check$valid) && 
               (is.null(env$ap$header_check) || 
                (isTruthy(env$ap$header_check) && 
                 isTruthy(env$ap$header_check$valid))) &&
               (is.null(env$ap$reftable_check) || 
                (isTruthy(env$ap$reftable_check) && 
                 isTruthy(env$ap$reftable_check$valid))) &&
               (is.null(env$ap$statobs_check) || 
                (isTruthy(env$ap$statobs_check) && 
                 isTruthy(env$ap$statobs_check$valid)))) {
                tagList(
                    train_set_setup_ui(ns("train_set_setup"))
                )
            } else {
                tagList(tags$div(
                    h4(
                        icon("warning"), "Project setup is not valid,",
                        "check the", tags$b("Project settings"), "above."
                    ),
                    style = "color: #F89406;"
                ))
            }
        })
    })
    
    ## Training set setup
    callModule(train_set_setup_server, "train_set_setup")
}

#' Training set setup ui
#' @keywords internal
#' @author Ghislain Durif
train_set_setup_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("existing")),
        uiOutput(ns("edition")),
        train_set_simu_run_ui(ns("train_set_simu_run")),
        hr(),
        prior_check_ui(ns("prior_check"))
    )
}

#' Training set sub-module server
#' @keywords internal
#' @author Ghislain Durif
train_set_setup_server <- function(input, output, session) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(edit = FALSE, existing = FALSE, new = FALSE)
    
    # existing or new setup ?
    observeEvent({
        c(env$ap$proj_dir, env$ap$proj_type, 
          env$ap$locus_type, env$ap$seq_mode, 
          env$ap$file_upload, env$ap$data_check)
    },{
        # requirements
        req(env$ap$proj_dir)
        req(env$ap$proj_type)
        req(env$ap$locus_type)
        req(env$ap$seq_mode)
        # check
        if(isTruthy(env$ap$proj_name) && 
           isTruthy(env$ap$data_file) &&
           isTruthy(env$ap$data_check) &&
           isTruthy(env$ap$data_check$valid) && 
           isTruthy(env$ap$header_check) && 
           isTruthy(env$ap$header_check$valid)) {
            local$existing <- TRUE
            local$edit <- FALSE
            local$new <- FALSE
        } else {
            local$existing <- FALSE
            local$edit <- TRUE
            local$new <- TRUE
        }
    })
    
    # button to edit exising setup
    output$existing <- renderUI({
        req(local$existing)
        req(!local$edit)
        req(env$ap$header_check$valid)
        
        tagList(
            box(
                title = "See the current configuration",
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                show_existing_proj_ui(ns("show_existing"))
            ),
            fluidRow(
                column(
                    width = 4,
                    actionButton(
                        ns("edit"),
                        label = "Edit configuration",
                        icon = icon("edit"),
                        width = '100%'
                    )
                )
            ),
            helpText(
                icon("warning"),
                "By editing the training set simulation configuration,", 
                "it will", tags$b("erase"), 
                "the corresponding training set configuration files",
                "(", tags$code(env$ap$header_check$header_file), ",",
                tags$code("reftableRF.bin"), "and", 
                tags$code("statobsRF.txt"), ")", 
                "if existing."
            ),
            hr()
        )
    })
    
    # show existing
    callModule(show_existing_proj_server, "show_existing")
    
    # edit exsting project ?
    observeEvent(input$edit, {
        req(input$edit)
        ask_confirmation(
            inputId = "edit_confirmation",
            title = "Want to confirm ?"
        )
    })
    
    observeEvent(input$edit_confirmation, {
        req(input$edit_confirmation)
        req(env$ap$proj_dir)
        local$edit <- TRUE
        # delete reftable and statobs files
        if(file.exists(file.path(env$ap$proj_dir, "reftableRF.bin"))) {
            fs::file_delete(file.path(env$ap$proj_dir, "reftableRF.bin"))
        }
        if(file.exists(file.path(env$ap$proj_dir, "statobsRF.txt"))) {
            fs::file_delete(file.path(env$ap$proj_dir, "statobsRF.txt"))
        }
        # project files update
        update_proj_file("ap")
    })
    
    ## edition mode
    output$edition <- renderUI({
        req(local$new || local$edit)
        train_set_config_ui(ns("train_set_config"))
    })
    callModule(train_set_config_server, "train_set_config")
    
    ## Training set simulation run
    callModule(train_set_simu_run_server, "train_set_simu_run")
    
    ## Model and scenario checking
    callModule(prior_check_server, "prior_check")
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
        h3(icon("dna"), "Number of loci to simulate"),
        helpText(
            "Locus settings defined in the provided header file."
        ),
        uiOutput(ns("show_loci")),
        hr(),
        uiOutput(ns("group_prior"))
    )
}

#' Show existing project set up module server
#' @keywords internal
#' @author Ghislain Durif
show_existing_proj_server <- function(input, output, session) {
    
    # namespace
    ns <- session$ns
    
    # show historical model
    output$show_scenarii <- renderUI({
        req(env$ap$header_check)
        req(env$ap$header_check$valid)
        req(env$ap$header_check$scenario_list)
        req(length(env$ap$header_check$scenario_list) > 0)
        
        tagList(
            do.call(
                flowLayout,
                lapply(
                    env$ap$header_check$scenario_list, 
                    function(item) tags$pre(item)
                )
            )
        )
    })
    
    # show priors
    output$show_priors <- renderUI({
        req(env$ap$header_check)
        req(env$ap$header_check$valid)
        req(env$ap$header_check$prior_list)
        req(length(env$ap$header_check$prior_list) > 0)
        
        tagList(
            do.call(
                tags$ul,
                lapply(
                    env$ap$header_check$prior_list,
                    function(item) tags$li(tags$code(item))
                )
            )
        )
    })
    
    # show conditions
    output$show_conditions <- renderUI({
        req(env$ap$header_check)
        req(env$ap$header_check$valid)
        
        if(isTruthy(env$ap$header_check$cond_list) &&
           length(env$ap$header_check$cond_list) > 0) {
            tagList(
                do.call(
                    tags$ul,
                    lapply(
                        env$ap$header_check$cond_list, 
                        function(item) tags$li(tags$code(item))
                    )
                )
            )
        } else {
            helpText("No condition in header file.")
        }
    })
    
    # show locus description
    output$show_loci <- renderUI({
        req(env$ap$header_check)
        req(env$ap$header_check$valid)
        req(env$ap$header_check$locus_desc)
        req(length(env$ap$header_check$locus_desc) > 0)
        
        tagList(
            do.call(
                tags$ul,
                lapply(
                    env$ap$header_check$locus_desc, 
                    function(item) tags$li(tags$code(item))
                )
            )
        )
    })
    
    ## group priors (if relevant)
    output$group_prior <- renderUI({
        req(env$ap$locus_type == "mss")
        req(env$ap$header_check)
        req(env$ap$header_check$valid)
        req(env$ap$header_check$group_prior_list)
        req(length(env$ap$header_check$group_prior_list) > 0)
        
        tagList(
            h3(icon("signal"), "Microsat/Sequence group priors"),
            helpText(
                "Group priors for MSS loci defined", 
                "in the provided header file."
            ),
            tagList(
                do.call(
                    flowLayout,
                    lapply(
                        env$ap$header_check$group_prior_list, 
                        function(item) tags$pre(item)
                    )
                )
            ),
            hr()
        )
    })
}

#' Training set configuration ui
#' @keywords internal
#' @author Ghislain Durif
train_set_config_ui <- function(id) {
    ns <- NS(id)
    tagList(
        helpText(
            tags$p(
                icon("info-circle"), "Click on the", icon("plus"),
                "to expand the different configuration boxes."
            ),
            tags$p(
                icon("warning"),
                "Do not forget to validate your input or modifications",
                "inside each box."
            )
        ),
        hr(),
        hist_model_panel_ui(ns("hist_model_panel")),
        br(),
        hr(),
        param_prior_panel_ui(ns("param_prior")),
        br(),
        hr(),
        param_cond_panel_ui(ns("param_cond")),
        br(),
        hr(),
        locus_setup_panel_ui(ns("locus_setup")),
        br(),
        hr(),
        uiOutput(ns("mss_config_setup")),
        actionBttn(
            ns("validate"),
            label = "Validate",
            style = "fill",
            block = TRUE,
            color = "primary"
        ),
        uiOutput(ns("feedback"))
    )
}

#' Training set configuration server
#' @keywords internal
#' @author Ghislain Durif
train_set_config_server <- function(input, output, session) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        validated = FALSE
    )
    
    # historical model setup
    callModule(hist_model_panel_server, "hist_model_panel")
    
    # prior
    callModule(param_prior_panel_server, "param_prior")
    
    # conditions
    callModule(param_cond_panel_server, "param_cond")
    
    # locus
    callModule(locus_setup_panel_server, "locus_setup")
    
    # MSS setup
    output$mss_config_setup <- renderUI({
        req(env$ap$locus_type)
        req(env$ap$locus_type == "mss")
        
        mss_setup_ui(ns("mss_setup"))
    })
    callModule(mss_setup_server, "mss_setup")
    
    
    # validate button
    observeEvent(input$validate, {
        
        # # debugging
        # pprint(env$ap$proj_dir)
        # pprint(env$ap$data_file)
        # 
        # pprint(env$ap$locus_type)
        # pprint(env$ap$seq_mode)
        # 
        # pprint(env$ts$scenario_list)
        # pprint(env$ts$n_param)
        # pprint(env$ts$n_param_list)
        # pprint(env$ts$model_prior)
        # pprint(env$ts$prior_list)
        # pprint(env$ts$cond_list)
        # pprint(env$ts$locus_desc)
        # pprint(env$ts$group_prior_list)
        # pprint(env$ts$mss_reftab_colname)
        
        local$validated <- FALSE
        
        req(env$ap$proj_dir)
        req(env$ap$data_file)

        req(env$ap$locus_type)
        req(env$ap$seq_mode)

        req(env$ts$scenario_list)
        # req(env$ts$model_prior) # always NULL
        req(env$ts$prior_list)
        # req(env$ts$cond_list) # can be empty
        req(env$ts$locus_desc)

        if(env$ap$locus_type == "mss") {
            req(env$ts$group_prior_list)
            req(env$ts$mss_reftab_colname)
        }
        
        write_check <- tryCatch(
            write_header(
                env$ap$proj_dir, env$ap$data_file, 
                env$ts$scenario_list, env$ts$n_param_list, 
                env$ts$prior_list, env$ts$cond_list, env$ap$locus_type, 
                env$ap$seq_mode, env$ts$locus_desc,
                env$ts$group_prior_list, env$ts$mss_reftab_colname
            ),
            error = function(e) {print(e); return(e)}
        )
        
        if("error" %in% class(write_check)) {
            local$validated <- FALSE
        } else {
            local$validated <- TRUE
            
            # delete previous headerRF, reftable and statobs files
            if(file.exists(file.path(env$ap$proj_dir, "headerRF.txt"))) {
                fs::file_delete(file.path(env$ap$proj_dir, "headerRF.txt"))
            }
            if(file.exists(file.path(env$ap$proj_dir, "reftableRF.bin"))) {
                fs::file_delete(file.path(env$ap$proj_dir, "reftableRF.bin"))
            }
            if(file.exists(file.path(env$ap$proj_dir, "statobsRF.txt"))) {
                fs::file_delete(file.path(env$ap$proj_dir, "statobsRF.txt"))
            }
            # project files update
            update_proj_file("ap")
        }
    })
    
    # # debugging
    # observe({
    #     pprint(local$validated)
    # })
    
    # feedback
    output$feedback <- renderUI({
        # init
        msg <- list()
        
        # check validation
        if(!local$validated) {
            msg <- append(msg, list(tagList(tags$p(tags$div(
                    icon("warning"),
                    "Your training set configuration is", 
                    "not complete (potential issue, see panels above)", 
                    "or not validated.",
                    style = "color: #F89406;"
            )))))
        } else {
            msg <- append(msg, list(tagList(
                tags$div(
                    h3(helpText(
                        icon("check"),
                        "Project is ready for training set simulation."
                    )),
                    style = "text-align:center;"
                )
            )))
        }
        
        # additional msg
        
        # check data file
        if(!isTruthy(env$ap$data_file)) {
            msg <- append(msg, list(tagList(tags$div(tags$ul(tags$li(
                "Missing data file.",
                style = "color: #F89406;"
            ))))))
        }
        
        # check locus type
        if(!isTruthy(env$ap$locus_type)) {
            msg <- append(msg, list(tagList(tags$div(tags$ul(tags$li(
                "Missing locus type.",
                style = "color: #F89406;"
            ))))))
        }
        
        # check sequencing mode
        if(!isTruthy(env$ap$seq_mode)) {
            msg <- append(msg, list(tagList(tags$div(tags$ul(tags$li(
                "Missing sequencing mode.",
                style = "color: #F89406;"
            ))))))
        }
        
        # check scenario list
        if(!isTruthy(env$ts$scenario_list)) {
            msg <- append(msg, list(tagList(tags$div(tags$ul(tags$li(
                "Missing scenario (historical model) definition.",
                style = "color: #F89406;"
            ))))))
        }
        
        # check prior list
        if(!isTruthy(env$ts$prior_list)) {
            msg <- append(msg, list(tagList(tags$div(tags$ul(tags$li(
                "Missing model parameter prior definition.",
                style = "color: #F89406;"
            ))))))
        }
        
        # check locus description
        if(!isTruthy(env$ts$locus_desc)) {
            msg <- append(msg, list(tagList(tags$div(tags$ul(tags$li(
                "Missing locus setup configuration.",
                style = "color: #F89406;"
            ))))))
        }
        
        if(env$ap$locus_type == "mss") {
            # check group prior list
            if(!isTruthy(env$ts$group_prior_list)) {
                msg <- append(msg, list(tagList(tags$div(tags$ul(tags$li(
                    "Missing mutation model defintion.",
                    style = "color: #F89406;"
                ))))))
            }
        }
        
        if(length(msg) > 0) {
            tags$p(do.call(tagList, unname(msg)))
        } else {
            NULL
        }
    })
}

#' Training set simulation run module ui
#' @keywords internal
#' @author Ghislain Durif
train_set_simu_run_ui <- function(id) {
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
        uiOutput(ns("nrun_input")),
        uiOutput(ns("feedback_nrun")),
        helpText(
            tags$p(
                icon("info-circle"),
                "The required number of simulations depends on your analysis:"
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
        h5(icon("comment"), "Run logs") %>% 
            helper(
                type = "inline", 
                content = tagList(
                    tags$b("Logs"), 
                    "are internal message generated by the software",
                    "during its run.", "It can contain useful information ",
                    "to understand errors for instance.", br(), br(),
                    "You should click on", tags$b("Show/Refresh"), 
                    "to load the latest version of the log file.", 
                    "The log panel does not update automatically."
                )
            ),
        tags$pre(
            uiOutput(ns("run_log")),
            style = "overflow:scroll; overflow-y:scroll; height:100px; resize: both;"
        ),
        actionButton(
            ns("show_log"), label = "Show/Refresh", icon = icon("pen"),
            width = "100%"
        )
    )
}

#' Training set simulation run module server
#' @keywords internal
#' @author Ghislain Durif
train_set_simu_run_server <- function(input, output, session) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        diyabc_run_process = NULL,
        diyabc_run_result = NULL,
        feedback = NULL,
        default_n_run = 100
    )
    
    # update local default nrun
    observeEvent(c(
        env$ap$reftable_check, env$ap$reftable_check$n_rec
    ), {
        local$default_n_run <- 100
        req(env$ap$reftable_check$n_rec > 0)
        local$default_n_run <- env$ap$reftable_check$n_rec
    }, ignoreNULL = FALSE
    )
    
    # nrun input
    output$nrun_input <- renderUI({
        req(is.numeric(local$default_n_run))
        numericInput(
            ns("nrun"),
            label = "Number of simulations requested in the training set",
            value = local$default_n_run,
            min = 10
        )
    })
    
    # feedback on simulation number
    output$feedback_nrun = renderUI({
        
        # number of simulations already available
        n_rec <- 0
        if(
            isTruthy(env$ap$reftable_check) && 
            isTruthy(env$ap$reftable_check$n_rec)
        ) {
            n_rec <- env$ap$reftable_check$n_rec
        }
        
        # number of required simulations
        n_run <- 0
        if(isTruthy(input$nrun)) {
            n_run <- input$nrun
        }
        
        tag_list <- tagList(
            tags$p(
                "Number of simulations already available =",
                tags$b(as.character(n_rec))
            ), 
            tags$p(
                "After the simulations,", 
                "the number of available simulations can be higher",
                "than what you requested,", 
                "because simulations are generated by batches,",
                "hence the number of simulations can only be a multiple",
                "of the batch size (see the", tags$b("Preferences"), 
                "tag to change the batch size)."
            )
        )
        if(n_run <= n_rec) {
            tag_list <- tagList(
                tags$p(tag_list),
                tags$p(
                    icon("warning"), 
                    "To generate additional training data,", 
                    "you must set the number of requested simulations",
                    "to be higher than",
                    tags$b(as.character(n_rec)), "."
                )
            )
        }
        
        helpText(tag_list)
    })
    
    ## run simulation
    observeEvent(input$simulate, {
        
        local$feedback <- tags$p(tags$div(
            icon("warning"), "Project is not ready.",
            "Check settings above.",
            style = "color: #F89406;"
        ))
        
        req(env$ap$proj_dir)
        req(env$ap$data_file)
        req(env$ap$header_check)
        req(env$ap$header_check$valid)
        
        req(input$nrun)
        
        ## enough required simulation ?
        n_rec <- 0
        if(isTruthy(env$ap$reftable_check$n_rec)) {
            local$feedback <- tags$p(tags$div(
                icon("warning"), 
                "The number of required simulations is lower than", 
                "the number of simulations already available.",
                style = "color: #F89406;"
            ))
            n_rec <- env$ap$reftable_check$n_rec
            
            req(input$nrun > env$ap$reftable_check$n_rec)
        }
        
        ## run in progress
        if(!is.null(local$diyabc_run_process)) {
            
            local$feedback <- helpText(
                icon("info-circle"), "Run in progress."
            )
            
            showNotification(
                id = ns("run_in_progress"), duration = 10,
                closeButton = TRUE, type = "warning",
                tagList(tags$p(
                    icon("warning"), "Run in progress."
                ))
            )
        } else {
            ## prepare run
            req(is.null(local$diyabc_run_process))
            
            ## init progress bar
            updateProgressBar(
                session = session,
                id = "simu_progress",
                value = n_rec, total = input$nrun,
                title = "Running simulation:"
            )
            
            ## ready to run
            
            # debugging
            # pprint("check options")
            # pprint(getOption("diyabcGUI"))
            # pprint(getOption("shiny.maxRequestSize"))
            
            local$feedback <- tags$div(
                h3(helpText(
                    icon("spinner", class = "fa-spin"),
                    "Simulations are running."
                )),
                style = "text-align:center;"
            )
            
            logging("Running simulation")
            local$diyabc_run_process <- diyabc_run_trainset_simu(
                env$ap$proj_dir, 
                as.integer(input$nrun),
                run_prior_check = FALSE
            )
        }
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
                invalidateLater(4000, session)
            } else {
                local$diyabc_run_result <- proc$get_exit_status()
            }
        })
    })
    
    ## clean up after run
    observeEvent(local$diyabc_run_result, {
        
        req(!is.null(local$diyabc_run_result))
        
        logging("diyabc simu run exit status:", local$diyabc_run_result)
        
        ## check run
        # run ok
        if(local$diyabc_run_result == 0) {
            
            local$feedback <- tags$div(
                h3(helpText(
                    icon("check"), "Run succeeded."
                )),
                style = "text-align:center;"
            )
            
            updateProgressBar(
                session = session,
                id = "simu_progress",
                value = input$nrun, total = input$nrun,
                title = "Running simulation:"
            )
            showNotification(
                id = ns("run_ok"), duration = 10,
                closeButton = TRUE, type = "message",
                tagList(tags$p(
                    icon("check"), "Simulations are done."
                ))
            )
            # project files update
            update_proj_file("ap")
        } else if(local$diyabc_run_result == -1000) {
            ## stopped run
            local$feedback <- tags$div(
                h3(icon("warning"), "Simulation run was stopped."),
                style = "text-align:center; color: #F89406;"
            )
            
            showNotification(
                id = ns("stop_run"), duration = 10,
                closeButton = TRUE, type = "error",
                tagList(tags$p(
                    icon("warning"), "Simulation run was stopped."
                ))
            )
        } else if(local$diyabc_run_result == -2000) {
            ## stopped run
            local$feedback <- tags$p(tags$div(
                icon("warning"), "Error in simulation process:", 
                "check your scenarios, priors and conditions.",
                style = "color: #F89406;"
            ))
            showNotification(
                id = ns("stop_run"), duration = 10,
                closeButton = TRUE, type = "error",
                tagList(tags$p(
                    icon("warning"), "Error in simulation process:", 
                    "check your scenarios, priors and conditions."
                ))
            )
        } else {
            ## error during run
            local$feedback <- tags$p(tags$div(
                icon("warning"), "Issues with run (see log panel below).",
                style = "color: #F89406;"
            ))
            showNotification(
                id = ns("run_not_ok"), duration = 10,
                closeButton = TRUE, type = "error",
                tagList(tags$p(
                    icon("warning"),
                    "A problem happened during simulations."
                ))
            )
        }
        
        # reset
        local$diyabc_run_result <- NULL
        local$diyabc_run_process <- NULL
    })
    
    ## show log
    observeEvent(input$show_log, {
        output$run_log <- renderUI({
            req(env$ap$proj_dir)
            logfile <- file.path(env$ap$proj_dir, "diyabc_run_call.log")
            req(file.exists(logfile))

            tmp_log <- readLines(logfile, warn=FALSE)
            req(length(tmp_log > 0))
            
            do.call(
                tagList,
                unname(as.list(tmp_log))
            )
        })
    })
    
    ## stop run
    observeEvent(input$stop, {
        ## if no current run
        if(is.null(local$diyabc_run_process)) {
            local$feedback <- helpText(
                icon("warning"), "No current run to stop."
            )
            showNotification(
                id = ns("no_run"), duration = 10,
                closeButton = TRUE, type = "error",
                tagList(tags$p(
                    icon("warning"), "No current run to stop."
                ))
            )
        } else {
            ## stop current run
            proc <- local$diyabc_run_process
            req(!is.null(proc$is_alive()))
            if(proc$is_alive()) {
                proc$kill()
            }
            local$diyabc_run_result <- -1000
            # project files update
            update_proj_file("ap")
        }
    })
    
    ## read log file during run and update progress bar
    observeEvent(input$simulate, {
        
        ## continue updating progress bar while diyabc is running
        if(!is.null(local$diyabc_run_process)) {
            invalidateLater(5000, session)
        }
        
        ## read log file
        req(env$ap$proj_dir)
        logfile <- file.path(env$ap$proj_dir, "diyabc_run_call.log")
        log_file_content <- character(0)
        if(file.exists(logfile)) {
            log_file_content <- readLines(logfile, warn=FALSE)
        }
        
        ## update progress bar
        req(input$nrun)
        req(log_file_content)
        
        # parse log
        last_log <- tail(log_file_content, 100)
        pttrn <- "^(?<=n simu data = )[0-9]+$"
        pttrn_match <- str_extract(last_log, pttrn)
        if(any(!is.na(pttrn_match))) {
            n_rec <- as.integer(tail(pttrn_match[!is.na(pttrn_match)], 1))
            
            updateProgressBar(
                session = session,
                id = "simu_progress",
                value = n_rec, 
                total = input$nrun,
                title = "Running simulation:"
            )
        }
        
        ## check for infinite loop
        if(!is.null(local$diyabc_run_process)) {
            if(sum(str_detect(last_log, "^locus=0")) > 2) {
                ## stop current run
                proc <- local$diyabc_run_process
                if(proc$is_alive()) {
                    proc$kill()
                }
                local$diyabc_run_result <- -2000
                ## feedback
                local$feedback <- tags$p(tags$div(
                    icon("warning"),
                    "Run was stopped because of infinite looping.", 
                    "Potential issue in model conditioning.",
                    style = "color: #F89406;"
                ))
            }
        }
    })
    
    ## feedback
    output$feedback <- renderUI({
        req(local$feedback)
        local$feedback
    })
}

#' Prior and scenario checking module ui
#' @keywords internal
#' @author Ghislain Durif
prior_check_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("check"), "Prior and scenario checking"),
        helpText(
            "This action requires a training set file",
            tags$code("reftableRF.bin"),
            "either generated by clicking on", tags$b("Simulate"),
            "above or uploaded with an existing project."
        ),
        actionGroupButtons(
            inputIds = c(ns("run"), ns("stop")),
            labels = list(
                tags$span(icon("play"), "Run"),
                tags$span(icon("stop"), "Stop")
            ),
            fullwidth = TRUE
        ),
        uiOutput(ns("feedback"))
    )
}

#' Prior and scenario checking module server
#' @keywords internal
#' @author Ghislain Durif
prior_check_server <- function(input, output, session) {
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        diyabc_run_process = NULL,
        diyabc_run_result = NULL,
        feedback = NULL
    )
    
    ## run simulation
    observeEvent(input$run, {
        
        local$feedback <- tags$p(tags$div(
            icon("warning"), "Missing simulation files.",
            style = "color: #F89406;"
        ))
        
        req(env$ap$proj_dir)
        req(env$ap$data_file)
        req(env$ap$header_check)
        req(env$ap$header_check$valid)
        req(env$ap$reftable_check)
        req(env$ap$reftable_check$valid)
        
        ## run in progress
        if(!is.null(local$diyabc_run_process)) {
            
            local$feedback <- helpText(
                icon("info-circle"), "Model checking in progress."
            )
            
            showNotification(
                id = ns("run_in_progress"), duration = 10,
                closeButton = TRUE, type = "warning",
                tagList(tags$p(
                    icon("warning"), "Model checking in progress."
                ))
            )
        } else {
            ## prepare run
            req(is.null(local$diyabc_run_process))
            
            ## ready to run
            
            # debugging
            # pprint("check options")
            # pprint(getOption("diyabcGUI"))
            # pprint(getOption("shiny.maxRequestSize"))
            
            local$feedback <- tags$div(
                h4(helpText(
                    icon("spinner", class = "fa-spin"),
                    "Model checking is running."
                )),
                style = "text-align:center;"
            )
            
            logging("Running model checking")
            local$diyabc_run_process <- diyabc_run_trainset_simu(
                env$ap$proj_dir, run_prior_check = TRUE
            )
        }
    })
    
    ## monitor simulation run
    observeEvent(local$diyabc_run_process, {
        req(!is.null(local$diyabc_run_process))
        
        print("diyabc model checking run process")
        print(local$diyabc_run_process)
        
        observe({
            req(!is.null(local$diyabc_run_process))
            proc <- local$diyabc_run_process
            req(!is.null(proc$is_alive()))
            if(proc$is_alive()) {
                invalidateLater(1000, session)
            } else {
                local$diyabc_run_result <- proc$get_exit_status()
            }
        })
    })
    
    ## clean up after run
    observeEvent(local$diyabc_run_result, {
        
        req(!is.null(local$diyabc_run_result))
        
        logging(
            "diyabc model checking run exit status:", local$diyabc_run_result
        )
        
        ## check run
        # run ok
        if(local$diyabc_run_result == 0) {
            
            local$feedback <- tags$div(
                h4(helpText(
                    icon("check"), "Model check succeeded."
                )),
                style = "text-align:center;"
            )
            
            showNotification(
                id = ns("run_ok"), duration = 10,
                closeButton = TRUE, type = "message",
                tagList(tags$p(
                    icon("check"), "Model check is done."
                ))
            )
            
            # graph output
            tmp_check <- tryCatch(
                scenario_check_graph_output(
                    env$ap$proj_dir, env$ap$proj_dir
                ),
                error = function(e) {print(e); return(e);}
            )
            
            if("error" %in% class(tmp_check)) {
                local$feedback <- tags$div(
                    icon("warning"),
                    "Graphical output generation for",
                    "scenario checking failed.",
                    style = "text-align:center; color: #F89406;"
                )
                
                showNotification(
                    id = ns("prior_mod_check_graph_output_fail"),
                    duration = 5,
                    closeButton = TRUE,
                    type = "error",
                    tagList(tags$p(
                        icon("warning"),
                        "Graphical output generation for",
                        "scenario checking failed."
                    ))
                )
            }
            
        } else if(local$diyabc_run_result == -1000) {
            ## stopped run
            local$feedback <- tags$div(
                h4(icon("warning"), "Model check run was stopped."),
                style = "text-align:center; color: #F89406;"
            )
            
            showNotification(
                id = ns("stop_run"), duration = 10,
                closeButton = TRUE, type = "error",
                tagList(tags$p(
                    icon("warning"), "Model check run was stopped."
                ))
            )
        } else {
            ## error during run
            local$feedback <- tags$p(tags$div(
                icon("warning"), 
                "Issues with model check run",
                "(save the project, c.f. below, and check the log files).",
                style = "color: #F89406;"
            ))
            showNotification(
                id = ns("run_not_ok"), duration = 10,
                closeButton = TRUE, type = "error",
                tagList(tags$p(
                    icon("warning"),
                    "A problem happened during model checking."
                ))
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
                id = ns("no_run"), duration = 10,
                closeButton = TRUE, type = "error",
                tagList(tags$p(
                    icon("warning"), "No current run to stop."
                ))
            )
        } else {
            ## stop current run
            proc <- local$diyabc_run_process
            req(!is.null(proc$is_alive()))
            if(proc$is_alive()) {
                proc$kill()
            }
            local$diyabc_run_result <- -1000
        }
    })
    
    ## feedback
    output$feedback <- renderUI({
        req(local$feedback)
        local$feedback
    })
}
