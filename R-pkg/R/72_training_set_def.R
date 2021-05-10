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
                width = 6,
                h4("Select a scenario"),
                uiOutput(ns("select_scen"))
            ),
            column(
                width = 6,
                h4(uiOutput(ns("scen_nb"))),
                actionGroupButtons(
                    inputIds = c(ns("edit"), ns("add"), ns("remove")),
                    labels = list(
                        tags$span(icon("edit"), "Edit"),
                        tags$span(icon("plus"), "Add"),
                        tags$span(icon("minus"), "Remove")
                    ),
                    fullwidth = TRUE
                )
            )
        ),
        br(),
        hr(),
        uiOutput(ns("hist_model_setup")),
        hr(),
        uiOutput(ns("hist_model_list"))
    )
}

#' Historical model input panel module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyjs onclick
#' @importFrom shinyWidgets actionBttn
#' @importFrom stringr str_c
hist_model_panel_server <- function(input, output, session) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        n_scen = 0, scen_id = 1, 
        new = FALSE, edit = FALSE, lock = FALSE,
        current_scenario = NULL
    )
    
    # # debugging
    # observe({
    #     pprint("Current list of scenarii")
    #     pprint(env$ts$scenario_list)
    # })
    
    # # debugging
    # observe({
    #     pprint("Current scenario")
    #     pprint(local$current_scenario)
    # })
    
    # # debugging
    # observe({
    #     pprint("lock")
    #     pprint(local$lock)
    #     pprint("validated")
    #     pprint(local$validated)
    # })
    
    # update number of scenario
    observeEvent(env$ts$scenario_list, {
        if(isTruthy(env$ts$scenario_list)) {
            local$n_scen <- length(env$ts$scenario_list)
        } else {
            local$n_scen <- 0
        }
    })
    
    # scenario selector
    output$select_scen <- renderUI({
        possible_choice <- c("", "No scenario available at the moment")
        selected_choice <- NULL
        # update choice
        if(isTruthy(local$n_scen) && (local$n_scen > 0)) {
            possible_choice <- 1:local$n_scen
        }
        # update selection
        scen_id <- isolate(local$scen_id)
        if(isTruthy(scen_id)) {
            selected_choice <- scen_id
        }
        # output
        selectInput(
            ns("scen_id"),
            label = NULL,
            choices = possible_choice,
            selected = selected_choice,
            multiple = FALSE
        )
    })
    
    # update selected scenario
    observeEvent(input$scen_id, {
        req(input$scen_id)
        req(!local$lock)
        local$scen_id <- as.integer(input$scen_id)
        local$new <- FALSE
        local$edit <- FALSE
    })
    
    # update current scenario
    observe({
        if(isTruthy(local$scen_id) && (local$scen_id > 0) && 
           isTruthy(local$n_scen) && (local$n_scen > 0)) {
            local$current_scenario <- env$ts$scenario_list[local$scen_id]
        } else {
            local$current_scenario <- NULL
        }
    })
    
    # add a scenario
    observeEvent(input$add, {
        req(!local$lock)
        # increment scenario counter
        local$n_scen <- ifelse(isTruthy(local$n_scen), local$n_scen, 0) + 1
        # change current selected scenario
        local$scen_id <- max(local$n_scen)
        # new scenario
        local$new <- TRUE
        local$lock <- TRUE
    })
    
    # edit a scenario
    observeEvent(input$edit, {
        req(!local$lock)
        req(is.numeric(local$scen_id))
        req(local$scen_id > 0)
        # edt scenario
        local$edit <- TRUE
        local$lock <- TRUE
    })
    
    # editor
    output$hist_model_setup <- renderUI({
        req(local$new || local$edit)
        hist_model_ui(ns("hist_model"))
    })
    
    # remove a scenario
    observeEvent(input$remove, {
        req(!local$lock)
        req(is.numeric(local$n_scen))
        req(local$n_scen > 0)
        req(is.numeric(local$scen_id))
        req(local$scen_id > 0)
        req(local$scen_id < local$n_scen + 1)
        # remove scenario
        env$ts$scenario_list <- env$ts$scenario_list[-local$scen_id]
        # update number of scenarii
        local$n_scen <- local$n_scen - 1
        # update selected scenario
        if(local$n_scen == 0) {
            local$scen_id <- 0
        } else {
            local$scen_id <- max(1, local$scen_id - 1)
        }
    })
    
    # output number of scenarii
    output$scen_nb <- renderUI({
        tagList(
            "Current number of", 
            ifelse(local$n_scen > 1, "scenarii", "scenario"),
            "=", tags$b(as.character(local$n_scen))
        )
    })
    
    # edit a scenario
    hist_model <- callModule(
        hist_model_server, "hist_model",
        project_dir = reactive(env$ap$proj_dir),
        raw_scenario = reactive(local$current_scenario),
        scenario_id = reactive(local$scen_id)
    )
    
    # is current scenario validated
    observeEvent(hist_model$validated, {
        req(local$new || local$edit)
        if(isTruthy(hist_model$validated) && isTruthy(hist_model$valid)) {
            local$lock <- FALSE
            env$ts$scenario_list[local$scen_id] <- hist_model$raw
        } else {
            local$lock <- TRUE
        }
    })
    
    # output existing scenarii in the project
    output$hist_model_list <- renderUI({
        if(isTruthy(env$ts$scenario_list) && 
           (length(env$ts$scenario_list) > 0)) {
            tagList(
                helpText(
                    tags$p(
                        icon("comment"), "Current",  
                        ifelse(local$n_scen > 1, "scenarii", "scenario"), ":"
                    )
                ),
                do.call(
                    flowLayout, 
                    lapply(env$ts$scenario_list, tags$pre)
                )
            )
        } else {
            tags$div(
                icon("warning"), "Please add at least one scenario.",
                style = "color: #F89406;"
            )
        }
    })
    
    # update default parameter priors
    observe({
        req(!isTruthy(env$ts$prior_list))
        env$ts$prior_list <- default_param_prior(env$ts$scenario_list)
    })
}

#' Parameter prior setting module ui
#' @keywords internal
#' @author Ghislain Durif
param_prior_panel_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("chart-bar"), "Priors and conditions"),
        uiOutput(ns("param_prior_def")),
        hr()
    )
}

#' Parameter prior setting module ui
#' @keywords internal
#' @author Ghislain Durif
param_prior_panel_server <- function(input, output, session,
                                     prior_list = reactive({NULL})) {
    
    # init local
    local <- reactiveValues(
        prior_list = NULL, param_name = NULL, param_type = NULL
    )
    
    # get input and set default prior if no input
    observeEvent(env$ts$prior_list, {
        req(env$ts$prior_list)
        # existing pior list
        local$prior_list <- env$ts$prior_list
        # param name
        local$param_name <- str_extract(
            local$prior_list, str_c("^", single_param_regex(), "(?= )")
        )
        # param type
        local$param_type <- str_extract(local$prior_list, str_c("(N|T|A)"))
    })
    
    # get parameter name
    observe({
        
    })
    
}
