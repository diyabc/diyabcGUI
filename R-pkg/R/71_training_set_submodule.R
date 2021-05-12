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
    },{
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
        # list number of parameters per model
        env$ts$n_param <- env$ap$header_check$n_param
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
        env$ts$mss_reftab_colname <- NULL
    })
    
    # panel output
    observeEvent({
        c(env$ap$proj_dir, env$ap$proj_type, 
          env$ap$locus_type, env$ap$seq_mode, 
          env$ap$file_upload, env$ap$data_check)
    },{
        output$train_set_simu <- renderUI({
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
                tagList(
                    tags$div(
                        h4(
                            icon("warning"), "Project setup is not valid,",
                            "check the", tags$b("Project settings"), "above."
                        ),
                        style = "color: #F89406;"
                    )
                )
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
            actionButton(
                ns("edit"),
                label = "Edit configuration",
                icon = icon("edit")
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
            hr(),
            show_existing_proj_ui(ns("show_existing"))
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
        update_proj_file("ap")
    })
    
    ## edition mode
    output$edition <- renderUI({
        req(local$new || local$edit)
        train_set_config_ui(ns("train_set_config"))
    })
    callModule(train_set_config_server, "train_set_config")
    
    ## Training set simulation run
    callModule(train_set_simu_run_server, "simu_run")
    
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
        hist_model_panel_ui(ns("hist_model_panel")),
        br(),
        hr(),
        param_prior_panel_ui(ns("param_prior")),
        br(),
        param_cond_panel_ui(ns("param_cond")),
        hr(),
        locus_setup_panel_ui(ns("locus_setup")),
        br(),
        br(),
        actionBttn(
            ns("validate"),
            label = "Validate",
            style = "fill",
            block = TRUE
        ),
        uiOutput(ns("feedback"))
    )
}

#' Training set configuration server
#' @keywords internal
#' @author Ghislain Durif
train_set_config_server <- function(input, output, session) {
    
    # historical model setup
    callModule(hist_model_panel_server, "hist_model_panel")
    
    # prior
    callModule(param_prior_panel_server, "param_prior")
    
    # conditions
    callModule(param_cond_panel_server, "param_cond")
    
    # locus
    callModule(locus_setup_panel_server, "locus_setup")
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
        )
    )
}

#' Training set simulation run module server
#' @keywords internal
#' @author Ghislain Durif
train_set_simu_run_server <- function(input, output, session) {
    # namespace
    ns <- session$ns
    
    # max number of rows in log
    nlog <- 5
}

#' Prior and scenario checking module ui
#' @keywords internal
#' @author Ghislain Durif
prior_check_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h4("Prior and scenario checking"),
        helpText(
            "This action requires a training set file",
            tags$code("reftableRF.bin"),
            "either generated when clicking on 'Simulate'",
            "or uploaded with an existing project."
        ),
        actionGroupButtons(
            inputIds = c(ns("run_prior_mod_check"), 
                         ns("stop_prior_mod_check")),
            labels = list(
                tags$span(icon("play"), "Run"),
                tags$span(icon("stop"), "Stop")
            ),
            fullwidth = TRUE
        ),
        uiOutput(ns("feedback_prior_mod_check"))
    )
}

#' Prior and scenario checking module server
#' @keywords internal
#' @author Ghislain Durif
prior_check_server <- function(input, output, session) {
    # namespace
    ns <- session$ns
    
    # max number of rows in log
    nlog <- 5
}
