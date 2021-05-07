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
                    train_set_def_ui(ns("train_set_def")),
                    br(),
                    hr(),
                    train_set_simu_run_ui(ns("simu_run")),
                    br(),
                    hr(),
                    prior_check_ui(ns("prior_check"))
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
    
    ## Training set definition
    callModule(train_set_def_server, "train_set_def")
    
    ## Training set simulation run
    callModule(train_set_simu_run_server, "simu_run")
    
    ## Model and scenario checking
    callModule(prior_check_server, "prior_check")
}

#' Training set sub-module ui
#' @keywords internal
#' @author Ghislain Durif
train_set_def_ui <- function(id) {
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
        uiOutput(ns("feedback"))
    )
}

#' Training set sub-module server
#' @keywords internal
#' @author Ghislain Durif
train_set_def_server <- function(input, output, session) {
    # namespace
    ns <- session$ns
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
