#' Historical model input panel module ui
#' @keywords internal
#' @author Ghislain Durif
hist_model_panel_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("history"), "Historical models"),
        box(
            title = "Define your scenarii",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            tagList(
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
                uiOutput(ns("hist_model_list")),
                hr(),
                fluidRow(
                    column(
                        width = 4,
                        actionButton(
                            ns("validate"),
                            label = "Validate your models",
                            icon = icon("check"),
                            width = '100%'
                        )
                    ),
                    column(
                        width = 8,
                        uiOutput(ns("feedback"))
                    )
                )
            )
        )
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
        scenario_list = character(0),
        n_scen = 0, scen_id = 1, 
        new = FALSE, edit = FALSE, lock = FALSE,
        current_scenario = NULL, validated = FALSE
    )
    
    # # debugging
    # observe({
    #     pprint("Current list of scenarii")
    #     pprint(env$ts$scenario_list)
    #     pprint(local$scenario_list)
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
            local$scenario_list <- env$ts$scenario_list
            local$n_scen <- length(env$ts$scenario_list)
        } else {
            local$scenario_list <- character(0)
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
        req(is.numeric(local$n_scen))
        req(local$n_scen > 0)
        req(local$scen_id > 0)
        req(local$scen_id < local$n_scen + 1)
        # edit scenario
        local$edit <- TRUE
        local$lock <- TRUE
        # update current scenario
        local$current_scenario <- local$scenario_list[local$scen_id]
    })
    
    # editor
    output$hist_model_setup <- renderUI({
        req(local$new || local$edit)
        hist_model_ui(ns("hist_model"))
    })
    
    # remove a scenario
    observeEvent(input$remove, {
        # possible to remove current scenario without validating it
        # req(!local$lock)
        req(is.numeric(local$n_scen))
        req(local$n_scen > 0)
        req(is.numeric(local$scen_id))
        req(local$scen_id > 0)
        req(local$scen_id < local$n_scen + 1)
        # remove scenario
        local$scenario_list <- local$scenario_list[-local$scen_id]
        # update number of scenarii
        local$n_scen <- local$n_scen - 1
        # update selected scenario
        if(local$n_scen == 0) {
            local$scen_id <- 0
        } else {
            local$scen_id <- max(1, local$scen_id - 1)
        }
        # update current scenario
        local$current_scenario <- NULL
        # update status
        local$new <- FALSE
        local$edit <- FALSE
        local$lock <- FALSE
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
            # update scenario list
            local$scenario_list[local$scen_id] <- hist_model$raw
            # update current scenario
            local$current_scenario <- NULL
            # update status
            local$new <- FALSE
            local$edit <- FALSE
            local$lock <- FALSE
        } else {
            local$lock <- TRUE
        }
    })
    
    # what happen when editing a scenario
    observeEvent({
        c(local$lock, input$scen_id, input$edit, input$add)
    }, {
        if(local$lock) {
            shinyjs::disable("scen_id")
            shinyjs::disable("edit")
            shinyjs::disable("add")
        } else {
            shinyjs::enable("scen_id")
            shinyjs::enable("edit")
            shinyjs::enable("add")
        }
    })
    
    # output existing scenarii in the project
    output$hist_model_list <- renderUI({
        if(isTruthy(local$scenario_list) && 
           (length(local$scenario_list) > 0)) {
            tagList(
                helpText(
                    tags$p(
                        icon("comment"), "Current",  
                        ifelse(local$n_scen > 1, "scenarii", "scenario"), ":"
                    )
                ),
                do.call(
                    flowLayout, 
                    lapply(local$scenario_list, tags$pre)
                )
            )
        } else {
            tags$div(
                icon("warning"), "Please add at least one scenario.",
                style = "color: #F89406;"
            )
        }
    })
    
    ## validate scenario list
    observeEvent(input$validate, {
        req(!local$lock)
        env$ts$scenario_list <- local$scenario_list
        local$validated <- TRUE
    })
    
    ## feedback on list of scenario
    output$feedback <- renderUI({
        if(local$lock) {
            tags$p(
                tags$div(
                    icon("warning"), 
                    "A scenario is being edited, please validate it",
                    "before validating the list of scenarii.",
                    style = "color: #F89406;"
                )
            )
        } else if(
            (length(env$ts$scenario_list) != length(local$scenario_list)) || 
            !all(env$ts$scenario_list == local$scenario_list)
        ) {
            local$validated <- FALSE
            tags$p(
                tags$div(
                    icon("warning"), 
                    "Scenario list was modified. Please validate.",
                    style = "color: #F89406;"
                )
            )
        } else {
            NULL
        }
    })
}

#' Parameter prior setting module ui
#' @keywords internal
#' @author Ghislain Durif
param_prior_panel_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("chart-bar"), "Priors and conditions"),
        box(
            title = "Setup the prior on your model parameters",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            tagList(
                uiOutput(ns("param_prior_def")),
                fluidRow(
                    column(
                        width = 4,
                        actionButton(
                            ns("validate"),
                            label = "Validate your priors",
                            icon = icon("check"),
                            width = '100%'
                        )
                    ),
                    column(
                        width = 8,
                        uiOutput(ns("feedback"))
                    )
                )
            )
        )
    )
}

#' Parameter prior setting module ui
#' @keywords internal
#' @author Ghislain Durif
param_prior_panel_server <- function(input, output, session) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        prior_list = NULL, param_type = NULL,
        Ne_prior_list = NULL, time_prior_list = NULL, rate_prior_list = NULL,
        modified_prior_list = NULL, validated = NULL
    )
    
    # update parameter priors based on scenario list
    observeEvent(env$ts$scenario_list, {
        env$ts$prior_list <- clean_param_prior(
            env$ts$prior_list, env$ts$scenario_list
        )
    })
    
    # get input and set default prior if no input
    observeEvent(env$ts$prior_list, {
        if(isTruthy(env$ts$prior_list)) {
            # existing pior list
            local$prior_list <- env$ts$prior_list
        } else {
            local$prior_list <- NULL
            local$param_type <- NULL
            local$Ne_prior_list <- NULL
            local$time_prior_list <- NULL
            local$rate_prior_list <- NULL
        }
    })
    
    # # debugging
    # observe({
    #     pprint("env prior list")
    #     pprint(env$ts$prior_list)
    #     pprint("local prior list")
    #     pprint(local$prior_list)
    # })
    
    # update parameter type-specific prior list
    observeEvent(local$prior_list, {
        if(isTruthy(local$prior_list)) {
            # param type
            local$param_type <- str_extract(
                local$prior_list, str_c("(?<= )(N|T|A)(?= )")
            )
            # extract parameter priors by parameter type
            if(any(local$param_type == "N")) {
                local$Ne_prior_list <- 
                    local$prior_list[local$param_type == "N"]
            } else {
                local$Ne_prior_list <- NULL
            }
            if(any(local$param_type == "T")) {
                local$time_prior_list <- 
                    local$prior_list[local$param_type == "T"]
            } else {
                local$time_prior_list <- NULL
            }
            if(any(local$param_type == "A")) {
                local$rate_prior_list <- 
                    local$prior_list[local$param_type == "A"]
            } else {
                local$rate_prior_list <- NULL
            }
        } else {
            local$Ne_prior_list <- NULL
            local$time_prior_list <- NULL
            local$rate_prior_list <- NULL
        }
    })
    
    # # debugging
    # observe({
    #     pprint("Ne prior list")
    #     pprint(local$Ne_prior_list)
    #     pprint("time prior list")
    #     pprint(local$time_prior_list)
    #     pprint("rate prior list")
    #     pprint(local$rate_prior_list)
    # })
    
    # get parameter name
    output$param_prior_def <- renderUI({
        tag_list1 <- NULL
        tag_list2 <- NULL
        tag_list3 <- NULL
        if(isTruthy(local$Ne_prior_list)) {
            tag_list1 <- tagList(
                h4(tags$b("Ne parameter(s) (effective size)")),
                hr(),
                prior_list_def_ui(ns("prior_def_Ne"))
            )
        }
        if(isTruthy(local$time_prior_list)) {
            tag_list2 <- tagList(
                h4(tags$b("Time parameter(s)")),
                hr(),
                prior_list_def_ui(ns("prior_def_time"))
            )
        }
        if(isTruthy(local$rate_prior_list)) {
            tag_list3 <- tagList(
                h4(tags$b("Admixture rate parameter(s)")),
                hr(),
                prior_list_def_ui(ns("prior_def_rate"))
            )
        }
        # output
        tagList(tag_list1, tag_list2, tag_list3)
    })
    
    ## Prior list definition
    Ne_prior_list <- callModule(
        prior_list_def_server, "prior_def_Ne", 
        prior_list = reactive(local$Ne_prior_list),
        type = reactive({"N"})
    )
    time_prior_list <- callModule(
        prior_list_def_server, "prior_def_time", 
        prior_list = reactive(local$time_prior_list),
        type = reactive({"T"})
    )
    rate_prior_list <- callModule(
        prior_list_def_server, "prior_def_rate", 
        prior_list = reactive(local$rate_prior_list),
        type = reactive({"A"})
    )
    
    ## get user input
    observeEvent({
        c(Ne_prior_list$prior_list, time_prior_list$prior_list, 
          rate_prior_list$prior_list)
    }, {
        if(any(local$param_type == "N")) {
            req(Ne_prior_list$valid)
        }
        if(any(local$param_type == "T")) {
            req(time_prior_list$valid)
        }
        if(any(local$param_type == "A")) {
            req(rate_prior_list$valid)
        }
        
        # merge
        local$modified_prior_list <- c(
            Ne_prior_list$prior_list, time_prior_list$prior_list, 
            rate_prior_list$prior_list
        )
    })
    
    ## validate prior list
    observeEvent(input$validate, {
        req(length(local$modified_prior_list > 0))
        env$ts$prior_list <- local$modified_prior_list
        local$validated <- TRUE
    })
    
    ## feedback on list of priors
    output$feedback <- renderUI({
        if(length(local$prior_list) == 0) {
            tags$p(
                tags$div(
                    icon("warning"), "No parameters.",
                    style = "color: #F89406;"
                )
            )
        } else if(
            (length(env$ts$prior_list) != length(local$modified_prior_list)) 
            || 
            !all(env$ts$prior_list == local$modified_prior_list)
        ) {
            local$validated <- FALSE
            tags$p(
                tags$div(
                    icon("warning"), 
                    "Prior list was modified. Please validate.",
                    style = "color: #F89406;"
                )
            )
        } else {
            NULL
        }
    })
}

#' Prior list setting module ui
#' @keywords internal
#' @author Ghislain Durif
prior_list_def_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("prior_list"))
    )
}

#' Parameter prior setting module server
#' @keywords internal
#' @author Ghislain Durif
prior_list_def_server <- function(input, output, session, 
                                  prior_list = reactive({NULL}),
                                  type = reactive({NULL})) {
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        prior_list = NULL, param_name = NULL, type = NULL, prior_def = NULL
    )
    
    # get input
    observe({
        local$prior_list <- prior_list()
        local$type <- type()
        # param name
        local$param_name <- str_extract(
            local$prior_list, str_c("^", single_param_regex(), "(?= )")
        )
    })
    
    # init output
    out <- reactiveValues(prior_list = NULL, valid = FALSE)
    
    # # debugging
    # observe({
    #     pprint(local$prior_list)
    #     pprint(local$param_name)
    #     pprint(local$type)
    # })
    
    # rendering
    output$prior_list <- renderUI({
        req(local$prior_list)
        req(local$param_name)
        do.call(
            tagList, 
            unname(lapply(
                local$param_name, 
                function(item) {
                    return(prior_def_ui(ns(str_c(item, "_prior"))))
                }
            ))
        )
    })
    
    # get output
    observe({
        req(local$prior_list)
        req(local$param_name)
        local$prior_def <- lapply(
            1:length(local$prior_list), 
            function(ind) {
                callModule(
                    prior_def_server,
                    str_c(local$param_name[ind], "_prior"),
                    prior = reactive(local$prior_list[ind]),
                    type = reactive(local$type)
                )
            }
        )
    })
    
    observe({
        prior_check <- unlist(lapply(
            local$prior_def, function(item) return(item$valid)
        ))
        if(!all(prior_check)) {
            out$prior_list <- NULL
            out$valid <- FALSE
        } else {
            out$valid <- TRUE
            out$prior_list <- unlist(lapply(
                local$prior_def, function(item) return(item$encoding)
            ))
        }
    })
    
    # ## debugging
    # observe({
    #     pprint(out$prior_list)
    # })
    
    ## output
    return(out)
}

#' Prior definition module ui
#' @keywords internal
#' @author Ghislain Durif
prior_def_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("prior_def"))
    )
}

#' Parameter defintion module server
#' @keywords internal
#' @author Ghislain Durif
prior_def_server <- function(input, output, session, 
                             prior = reactive({NULL}),
                             type = reactive({NULL})) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        prior = NULL, type = NULL, 
        name = NULL, distrib = NULL,
        min = NULL, max = NULL, mean = NULL, stdev = NULL,
        bound_min = NULL, bound_max = NULL, bound_step = NULL,
        mean_min = NULL, mean_max = NULL, mean_step = NULL,
        sd_min = NULL, sd_max = NULL, sd_step = NULL
    )
    
    # get input
    observe({
        local$prior <- prior()
        local$type <- type()
    })
    
    # init output
    out <- reactiveValues(encoding = NULL, valid = TRUE)
    
    # # debugging
    # observe({
    #     logging("prior = ", local$prior)
    #     logging("type = ", local$type)
    # })
    
    # input setup
    observeEvent(local$type, {
        req(local$type)
        if(local$type == "A") {
            local$bound_min <- 1E-5
            local$bound_max <- 1 - 1E-5
            local$bound_step <- 0.001
            local$mean_min <- 1E-5
            local$mean_max <- 1 - 1E-5
            local$mean_step <- 0.001
            local$sd_min <- 0
            local$sd_max <- NA
            local$sd_step <- 0.001
        } else {
            local$bound_min <- 0
            local$bound_max <- NA
            local$bound_step <- 1
            local$mean_min <- 0
            local$mean_max <- NA
            local$mean_step <- 1
            local$sd_min <- 0
            local$sd_max <- NA
            local$sd_step <- 1
        }
    })
    
    # parse input
    observe({
        req(local$prior)
        if(isTruthy(check_header_prior(local$prior))) {
            # param name
            local$name <- get_param_name(local$prior)
            # distribution
            local$distrib <- get_prior_distrib(local$prior)
            # update parameter values
            tmp_value <- get_prior_num_val(local$prior)
            if(isTruthy(tmp_value)) {
                local$min <- tmp_value[1]
                local$max <- tmp_value[2]
                local$mean <- tmp_value[3]
                local$stdev <- tmp_value[4]
            } else {
                local$min <- NULL
                local$max <- NULL
                local$mean <- NULL
                local$stdev <- NULL
            }
        } else {
            local$min <- NULL
            local$max <- NULL
            local$mean <- NULL
            local$stdev <- NULL
        }
    })
    
    # render input
    output$prior_def <- renderUI({
        req(local$name)
        req(local$distrib)
        req(local$min)
        req(local$max)
        req(local$mean)
        req(local$stdev)
        req(local$bound_min)
        # req(local$bound_max) # can be NA
        req(local$bound_step)
        req(local$mean_min)
        # req(local$mean_max) # can be NA
        req(local$mean_step)
        req(local$sd_min)
        # req(local$sd_max) # can be NA
        req(local$sd_step)
        
        input_min <- numericInput(
            ns("min"), label = NULL, value = local$min,
            min = local$bound_min, max = local$bound_max,
            step = local$bound_step
        )
        
        input_max <- numericInput(
            ns("max"), label = NULL, value = local$max,
            min = local$bound_min, max = local$bound_max,
            step = local$bound_step
        )
        
        input_mean <- numericInput(
            ns("mean"), label = NULL, value = local$mean,
            min = local$mean_min, max = local$mean_max,
            step = local$mean_step
        )
        
        input_stdev <- numericInput(
            ns("stdev"), label = NULL, value = local$stdev,
            min = local$sd_min, max = local$sd_max,
            step = local$sd_step
        )
        
        title_style <- 
            "text-align:right;margin-right:1em;vertical-align:middle;"
        
        tagList(
            fluidRow(
                column(
                    width = 2,
                    tags$h5(local$name)
                ),
                column(
                    width = 2,
                    selectInput(
                        ns("prior_type"),
                        label = NULL,
                        choices = list("Uniform" = "UN", "Log-Unif." = "LU",
                                       "Normal" = "NO", "Log-Norm." = "LN"),
                        selected = local$distrib
                    ),
                ),
                column(
                    width = 4,
                    splitLayout(
                        tags$h5("Min.", style = title_style),
                        input_min,
                        cellWidths = c("40%", "60%")
                    )
                ),
                column(
                    width = 4,
                    splitLayout(
                        tags$h5("Max.", style = title_style),
                        input_max,
                        cellWidths = c("40%", "60%")
                    )
                )
            ),
            fluidRow(
                column(
                    offset = 4, width = 4,
                    splitLayout(
                        tags$h5("Mean", style = title_style),
                        input_mean,
                        cellWidths = c("40%", "60%")
                    )
                ),
                column(
                    width = 4,
                    splitLayout(
                        tags$h5("Std. dev.", style = title_style),
                        input_stdev,
                        cellWidths = c("40%", "60%")
                    )
                )
            ),
            hr()
        )
    })
    
    ## distribution
    observe({
        req(local$name)
        req(input$prior_type)
        if(isTruthy(local$type) && (input$prior_type == local$type)) {
            req(NULL)
        }
        local$type <- input$prior_type
    })
    
    ## disable mean and stdev if uniform or log-uniform
    observe({
        req(local$type)
        if(local$type %in% c("UN", "LU")) {
            updateNumericInput(session, "mean", value = 0)
            updateNumericInput(session, "stdev", value = 0)
            shinyjs::disable("mean")
            shinyjs::disable("stdev")
        } else {
            shinyjs::enable("mean")
            shinyjs::enable("stdev")
        }
    })
    
    #### check for missing input
    ## min
    observe({
        req(local$name)
        feedbackWarning(
            "min", !isTruthy(input$min),
            "Missing value."
        )
    })
    
    ## max
    observe({
        req(local$name)
        feedbackWarning(
            "max", !isTruthy(input$max),
            "Missing value."
        )
    })
    
    ## mean
    observe({
        req(local$name)
        feedbackWarning(
            "mean", !isTruthy(input$mean),
            "Missing value."
        )
    })
    
    ## stdev
    observe({
        req(local$name)
        feedbackWarning(
            "stdev", !isTruthy(input$stdev),
            "Missing value."
        )
    })

    ## check for min/max
    observe({
        req(local$name)
        req(input$min)
        req(input$max)
        feedbackWarning(
            "min", (input$min >= input$max), "min >= max"
        )
        feedbackWarning(
            "max", (input$min >= input$max), "min >= max"
        )
    })
    
    ## check for normal and log-normal parameter setting
    observe({
        req(local$name)
        req(input$prior_type)
        req(input$min)
        req(input$mean)
        feedbackWarning(
            "mean", 
            input$prior_type %in% c("NO", "LN") && 
                (input$mean < input$min),
            "mean < min"
        )
        feedbackWarning(
            "min", 
            input$prior_type %in% c("NO", "LN") && 
                (input$mean < input$min),
            "mean < min"
        )
    })
    
    observe({
        req(local$name)
        req(input$prior_type)
        req(input$max)
        req(input$mean)
        feedbackWarning(
            "mean", 
            input$prior_type %in% c("NO", "LN") && 
                (input$mean > input$max),
            "mean > max"
        )
        feedbackWarning(
            "max", 
            input$prior_type %in% c("NO", "LN") && 
                (input$mean > input$max),
            "mean > max"
        )
    })

    # observe({
    #     logging("min = ", input$min)
    #     logging("max = ", input$max)
    #     logging("mean = ", input$mean)
    #     logging("stdev = ", input$stdev)
    # })
    
    ## encode output
    observe({
        out$encoding <- NULL
        out$valid <- FALSE
        req(local$name)
        req(local$type)
        req(local$selected_type)
        req(input$min)
        req(input$max)
        req(input$mean)
        req(input$stdev)
        out$valid <- !((input$min >= input$max) || 
            ((local$selected_type %in% c("NO", "LN")) &&
                 (input$mean < input$min || input$mean > input$max)))
        if(out$valid) {
            out$encoding <- str_c(
                local$name, " ",
                local$type, " ",
                local$selected_type, "[",
                input$min, ",", input$max, ",",
                input$mean, ",", input$stdev, "]"
            )
        }
    })
    
    # ## debugging
    # observe({
    #     logging("prior def = ", out$encoding)
    # })
    
    ## output
    return(out)
}

#' Parameter condition setting module ui
#' @keywords internal
#' @author Ghislain Durif
param_cond_panel_ui <- function(id) {
    ns <- NS(id)
    # help page
    cond_help <- tagList(
        tags$ul(
            tags$li(
                "You might need to impose some", tags$b("conditions"), "on", 
                "historical parameters",
                "(e.g. to avoid genealogical inconsistencies)",
                "or to constraint simulation settings.",
            ),
            tags$li(
                "For instance, there can be two time parameters", 
                "with overlapping prior distributions.", 
                "However, you might want",
                "the first one, say", tags$code("t1"), 
                ", to always be larger than the second one, say", 
                tags$code("t2"), ".",
            ),
            tags$li(
                "To do so, you just need to set",
                tags$code("t1 > t2"), 
                "in the 'Condition setting' panel.", 
                "Such a condition should concern",
                "two parameters of the same type",
                "(i.e. two effective sizes, two times or two admixture rates)."
            )
        )
    )
    
    tagList(
        h4("Condition setting"),
        box(
            title = "Define conditions on your model parameters",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            tagList(
                uiOutput(ns("cond_input")),
                fluidRow(
                    column(
                        width = 4,
                        actionButton(
                            ns("validate"),
                            label = "Validate",
                            icon = icon("check"),
                            width = '100%'
                        )
                    )
                ),
                helpText(
                    icon("info-circle"),
                    "Enter a single condition per line.",
                    "Conditions should have the following format:", 
                    tags$code("XX<YY"),
                    "where", tags$code("XX"), "and", tags$code("YY"),
                    "are parameters of the same type from the same model.",
                    "You can use the standard comparison signs:",
                    tags$code(">"), ",", tags$code(">="), ",", 
                    tags$code("<"),  ",",
                    tags$code("=<"), "."
                ) %>%
                    helper(
                        type = "inline", 
                        content = as.character(cond_help)
                    ),
                uiOutput(ns("feedback"))
            )
        )
    )
}

#' Parameter condition setting module server
#' @keywords internal
#' @author Ghislain Durif
param_cond_panel_server <- function(input, output, session) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        cond_list = NULL, input_cond = NULL, validated = TRUE, 
        cond_check = NULL
    )
    
    # get current condition list (if existing)
    observeEvent(env$ts$cond_list, {
        if(isTruthy(env$ts$cond_list)) {
            local$cond_list <- str_c(env$ts$cond_list, collapse = "\n")
        } else {
            local$cond_list <- NULL
        }
    })
    
    # condition input
    output$cond_input <- renderUI({
        textAreaInput(
            ns("cond_set"), 
            value = local$cond_list,
            label = NULL, 
            rows = 4,
            resize = "none"
        )
    })
    
    # invalidate
    observeEvent(input$cond_set, {
        if(isTruthy(input$cond_set)) {
            local$input_cond <- unlist(str_split(input$cond_set, "\n"))
            if(length(local$input_cond) != length(env$ts$cond_list) ||
               any(sort(local$input_cond) != sort(env$ts$cond_list))) {
                local$validated <- FALSE
            } else {
                local$validated <- TRUE
            }
        } else {
            local$validated <- FALSE
            local$input_cond <- NULL
        }
    })
    
    # feedback
    observe({
        feedbackWarning(
            "cond_set", !local$validated, "Condition(s) not validated."
        )
    })
    
    # get user input
    observeEvent(input$validate, {
        if(is.null(local$input_cond) || (local$input_cond == "")) {
            local$validated <- TRUE
            env$ts$cond_list <- NULL
        }
        # else
        req(local$input_cond)
        req(env$ts$scenario_list)
        # check
        local$cond_check <- check_cond(local$input_cond, env$ts$scenario_list)
        # update if valid edition
        if(local$cond_check$valid) {
            local$validated <- TRUE
            env$ts$cond_list <- local$input_cond
        } else {
            local$validated <- FALSE
        }
    })
    
    # feedback
    output$feedback <- renderUI({
        req(local$cond_check)
        req(!local$cond_check$valid)
        tags$div(
            icon("warning"), "Issue with input conditions.",
            do.call(
                tags$ul,
                unname(lapply(local$cond_check$msg, tags$li))
            ),
            style = "color: #F89406;"
        )
    })
}

#' Locus setup panel ui
#' @keywords internal
#' @author Ghislain Durif
locus_setup_panel_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("dna"), "Locus setup"),
        box(
            title = "Configure the loci to simulate",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            tagList(
                uiOutput(ns("locus_setup"))
            )
        )
    )
}

#' Locus setup panel server
#' @keywords internal
#' @author Ghislain Durif
locus_setup_panel_server <- function(input, output, session) {
    # namespace
    ns <- session$ns
    
    # render ui
    output$locus_setup <- renderUI({
        
        req(env$ap$locus_type)
        req(env$ap$data_check)
        req(env$ap$data_check$valid)
        
        if(env$ap$locus_type == "snp") {
            tagList(
                snp_locus_setup_ui(ns("snp_setup"))
            )
        } else if(env$ap$locus_type == "mss") {
            tagList(
                mss_locus_setup_ui(ns("mss_setup"))
            )
        } else {
            NULL
        }
    })
    
    ## server-side
    callModule(snp_locus_setup_server, "snp_setup")
    callModule(mss_locus_setup_server, "mss_setup")
}

#' Locus SNP setup ui
#' @keywords internal
#' @author Ghislain Durif
snp_locus_setup_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("input_setup")),
        fluidRow(
            column(
                width = 4,
                actionButton(
                    ns("validate"),
                    label = "Validate locus setup",
                    icon = icon("check"),
                    width = '100%'
                )
            )
        ),
        uiOutput(ns("feedback"))
    )
}

#' Locus SNP setup server
#' @keywords internal
#' @author Ghislain Durif
snp_locus_setup_server <- function(input, output, session) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        locus_desc = NULL, locus_count = NULL, from = NULL,
        input_count = NULL, input_from = NULL, 
        modified_locus_desc = NULL, 
        valid = TRUE, msg = NULL, validated = TRUE
    )
    
    # ## debugging
    # observe({
    #     pprint("locus desc")
    #     pprint(env$ts$locus_desc)
    # })
    
    # get existing locus description if any (and default otherwise)
    observeEvent({
        c(env$ts$locus_desc, env$ap$data_check)
    }, {
        local$locus_desc <- character(0)
        local$validated <- FALSE
        
        req(env$ap$locus_type)
        req(env$ap$locus_type == "snp")
        req(env$ap$data_check)
        req(env$ap$data_check$locus_count)
        req(nrow(env$ap$data_check$locus_count) > 0)
        
        local$locus_desc <- clean_snp_locus_desc(
            env$ts$locus_desc, env$ap$data_check$locus_count
        )
        
        if(identical(local$locus_desc, env$ts$locus_desc)) {
            local$validated <- TRUE
        }
    })
    
    # parse locus description
    observeEvent(local$locus_desc, {
        
        req(local$locus_desc)
        
        req(env$ap$locus_type)
        req(env$ap$locus_type == "snp")
        req(env$ap$data_check)
        req(env$ap$data_check$locus_count)
        req(nrow(env$ap$data_check$locus_count) > 0)
        
        check <- check_snp_locus_desc(
            local$locus_desc, env$ap$data_check$locus_count
        )
        
        local$valid <- check$valid
        local$msg <- check$msg
        
        # prepare rendering setup
        if(local$valid) {
            local$locus_count <- check$locus_count
            local$from <- check$from
        } else {
            local$from <- 1
            local$locus_count <- subset(
                env$ap$data_check$locus_count,
                env$ap$data_check$locus_count$available > 0
            )
            local$locus_count$count <- local$locus_count$available
        }
    })
    
    # # debugging
    # observe({
    #     pprint("locus count")
    #     pprint(local$locus_count)
    # })
    
    # render ui
    output$input_setup <- renderUI({
        req(local$locus_count)
        req(nrow(local$locus_count) > 0)
        req(local$from)
        
        tag_list <- unname(lapply(
            split(local$locus_count, seq(nrow(local$locus_count))),
            function(item) {
                return(
                    fluidRow(
                        column(
                            width = 4,
                            shinyjs::disabled(textInput(
                                ns(str_c("type_", item$type)),
                                label = "SNP locus type",
                                value = item$type
                            ))
                        ),
                        column(
                            width = 4,
                            shinyjs::disabled(textInput(
                                ns(str_c("type_", item$type)),
                                label = "Number of available loci",
                                value = as.character(item$available)
                            ))
                        ),
                        column(
                            width = 4,
                            numericInput(
                                inputId = ns(str_c("num_", item$type)),
                                label = "Number of loci to simulate",
                                min = 0, step = 1,
                                max = as.integer(item$available),
                                value = as.integer(item$count)
                            )
                        )
                    )
                )
            }
        ))
        
        tagList(
            do.call(tagList, tag_list),
            br(),
            fluidRow(
                column(
                    width = 4,
                    numericInput(
                        inputId = ns("locus_id_from"),
                        label = "from",
                        min = 1, step = 1,
                        max = sum(local$locus_count$available),
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
                                "if you set up", tags$code("from = 4"),
                                "the loci from 4 to 9 (out of 10)",
                                "will be used in the analysis.",
                                sep = " "
                            )
                        )
                )
            )
        )
    })
    
    ## get input locus count
    observe({ 
        req(local$locus_count)
        req(local$locus_count$type)
        
        modified_input_count <- unlist(lapply(
            local$locus_count$type,
            function(item) {
                out <- NA
                feedbackWarning(
                    str_c("num_", item), 
                    !isTruthy(input[[ str_c("num_", item) ]]),
                    "Missing input."
                )
                if(isTruthy(input[[ str_c("num_", item) ]])) {
                    n_loci <- input[[ str_c("num_", item) ]]
                    out <- str_c(
                        as.integer(n_loci),
                        str_c("<", item, ">"),
                        sep = " "
                    )
                }
                return(out)
            }
        ))
        
        if(!identical(local$input_count, modified_input_count)) {
            local$validated <- FALSE
            local$input_count <- modified_input_count
        }
    })
    
    ## get input from
    observeEvent(input$locus_id_from, {
        feedbackWarning(
            "locus_id_from", !isTruthy(input$locus_id_from),
            "Missing input."
        )
        local$validated <- FALSE
        if(isTruthy(input$locus_id_from)) {
            local$input_from <- input$locus_id_from
        } else {
            local$input_from <- NA
        }
    })
    
    # ## debugging
    # observe({
    #     pprint("user input")
    #     pprint(local$input_count)
    #     pprint(local$input_from)
    # })
    
    ## process input
    observeEvent(input$validate, {
        req(env$ap$data_check)
        req(env$ap$data_check$locus_count)
        req(nrow(env$ap$data_check$locus_count) > 0)
        
        req(local$input_count)
        req(!any(is.na(local$input_count)))
        req(local$input_from)
        
        local$validated <- TRUE
        
        # merge
        local$modified_locus_desc <- str_c(
            str_c(local$input_count, collapse = " "),
            "G1 from", local$input_from, sep = " "
        )
        
        # check and update
        check <- check_snp_locus_desc(
            local$modified_locus_desc, env$ap$data_check$locus_count
        )
        
        local$valid <- check$valid
        local$msg <- check$msg
        
        # update if valid and if modification
        if(!local$valid) {
            local$modified_locus_desc <- character(0)
        }
    })
    
    # ## debugging
    # observe({
    #     pprint("modified desc")
    #     pprint(local$modified_locus_desc)
    # })
    
    # observe({
    #     pprint("validated?")
    #     pprint(local$validated)
    #     pprint("valid?")
    #     pprint(local$valid)
    # })
    
    # update env
    observeEvent(local$modified_locus_desc, {
        req(is.logical(local$valid))
        # update if valid and if modification
        if(isTruthy(local$modified_locus_desc) && local$valid) {
            if(!identical(local$modified_locus_desc, env$ts$locus_desc)) {
                env$ts$locus_desc <- local$modified_locus_desc
            }
        }
    })
    
    # feedback for locus description check
    output$feedback <- renderUI({
        req(is.logical(local$valid))
        req(is.logical(local$validated))
        if(!local$validated) {
            tagList(
                tags$div(
                    icon("warning"), "Locus description(s) not validated.",
                    style = "color: #F89406;"
                )
            )
        } else if(!local$valid) {
            req(local$msg)
            tagList(
                tags$div(
                    icon("warning"), "Issue with SNP locus description:",
                    do.call(
                        tags$ul,
                        lapply(local$msg, tags$li)
                    ),
                    style = "color: #F89406;"
                )
            )
        } else {
            NULL
        }
    })
}

#' Locus MSS setup ui
#' @keywords internal
#' @author Ghislain Durif
mss_locus_setup_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("feedback")),
        helpText(
            icon("info-circle"),
            "With Microsat/Sequence data,",
            "the number of simulated loci is not configurable."
        )
    )
}

#' Locus MSS setup server
#' @keywords internal
#' @author Ghislain Durif
mss_locus_setup_server <- function(input, output, session) {
    output$feedback <- renderUI({
        req(env$ap$locus_type)
        req(env$ap$locus_type == "mss")
        req(env$ap$data_check)
        req(env$ap$data_check$valid)
        req(env$ap$data_check$locus_mode)
        
        tagList(
            tags$ul(
                tags$li(
                    "Number of Microsat locus =",
                    as.character(sum(env$ap$data_check$locus_mode == "M"))
                ),
                tags$li(
                    "Number of Sequence locus =",
                    as.character(sum(env$ap$data_check$locus_mode == "S"))
                )
            )
        )
    })
}

#' MSS specific config setup ui
#' @keywords internal
#' @author Ghislain Durif
mss_setup_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("object-group"), "Microsat/Sequence locus configuration"),
        uiOutput(ns("mss_setup")),
        br(),
        hr(),
        h3(icon("signal"), "Microsat/Sequence group priors"),
        uiOutput(ns("mss_prior")),
        br(),
        hr()
    )
}

#' MSS specific config setup server
#' @keywords internal
#' @author Ghislain Durif
mss_setup_server <- function(input, output, session) {
    
    ns <- session$ns
    
    output$mss_setup <- renderUI({
        req(env$ap$locus_type)
        req(env$ap$locus_type == "mss")
        
        tagList(
            mss_config_ui(ns("mss_config"))
        )
        
    })
    
    callModule(mss_config_server, "mss_config")
    
    output$mss_prior <- renderUI({
        req(env$ap$locus_type)
        req(env$ap$locus_type == "mss")
        
        tagList(
            mss_group_prior_ui(ns("mss_group_prior"))
        )
    })
    
    callModule(mss_group_prior_server, "mss_group_prior")
    
}

#' MSS config ui
#' @keywords internal
#' @author Ghislain Durif
mss_config_ui <- function(id) {
    ns <- NS(id)
    tagList(
        helpText(
            icon("info-circle"),
            "By default (for new projects),",
            "all Microsat loci are grouped together,",
            "same for all Sequence loci."
        ),
        uiOutput(ns("microsat")),
        uiOutput(ns("sequence"))
    )
}

#' MSS config server
#' @keywords internal
#' @author Ghislain Durif
mss_config_server <- function(input, output, session) {
    # namespace
    ns <- session$ns
    # microsat
    output$microsat <- renderUI({
        req(env$ap$locus_type)
        req(env$ap$locus_type == "mss")
        req(env$ap$data_check)
        req(env$ap$data_check$valid)
        req(env$ap$data_check$locus_mode)
        req(any(env$ap$data_check$locus_mode == "M"))
        tagList(
            microsat_config_ui(ns("microsat_config"))
        )
    })
    callModule(microsat_config_server, "microsat_config")
    # sequence
    output$sequence <- renderUI({
        req(env$ap$locus_type)
        req(env$ap$locus_type == "mss")
        req(env$ap$data_check)
        req(env$ap$data_check$valid)
        req(env$ap$data_check$locus_mode)
        req(any(env$ap$data_check$locus_mode == "S"))
        tagList(
            sequence_config_ui(ns("sequence_config"))
        )
    })
    callModule(sequence_config_server, "sequence_config")
}

#' Microsat config ui
#' @keywords internal
#' @author Ghislain Durif
microsat_config_ui <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            title = "Setup Microsat locus configuration",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            tagList(
                helpText(
                    icon("info-circle"),
                    "By default (for new projects),",
                    "all Microsat loci are assumed to be dinucleid",
                    "(motif = 2) with a range of 40."
                )
            )
        )
    )
}

#' Microsat config server
#' @keywords internal
#' @author Ghislain Durif
microsat_config_server <- function(input, output, session) {}

#' Sequence config ui
#' @keywords internal
#' @author Ghislain Durif
sequence_config_ui <- function(id) {
    ns <- NS(id)
    # help page
    sequence_config_help <- tagList()
    
    tagList(
        # %>%
        #     helper(
        #         type = "inline", 
        #         content = as.character(mss_config_help)
        #     ),
        box(
            title = "Setup Sequence locus configuration",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            "TODO"
        )
    )
}

#' Sequence config server
#' @keywords internal
#' @author Ghislain Durif
sequence_config_server <- function(input, output, session) {}

#' MSS group prior setup ui
#' @keywords internal
#' @author Ghislain Durif
mss_group_prior_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("microsat")),
        uiOutput(ns("sequence"))
    )
}

#' MSS group prior setup ui
#' @keywords internal
#' @author Ghislain Durif
mss_group_prior_server <- function(input, output, session) {
    # namespace
    ns <- session$ns
    # microsat
    output$microsat <- renderUI({
        req(env$ap$locus_type)
        req(env$ap$locus_type == "mss")
        req(env$ap$data_check)
        req(env$ap$data_check$valid)
        req(env$ap$data_check$locus_mode)
        req(any(env$ap$data_check$locus_mode == "M"))
        tagList(
            microsat_group_prior_ui(ns("microsat_prior"))
        )
    })
    callModule(microsat_group_prior_server, "microsat_prior")
    # sequence
    output$sequence <- renderUI({
        req(env$ap$locus_type)
        req(env$ap$locus_type == "mss")
        req(env$ap$data_check)
        req(env$ap$data_check$valid)
        req(env$ap$data_check$locus_mode)
        req(any(env$ap$data_check$locus_mode == "S"))
        tagList(
            sequence_group_prior_ui(ns("sequence_prior"))
        )
    })
    callModule(sequence_group_prior_server, "sequence_prior")
}

#' Microsat group prior setup ui
#' @keywords internal
#' @author Ghislain Durif
microsat_group_prior_ui <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            title = "Setup Microsat group priors",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            "TODO"
        )
    )
}

#' Microsat group prior setup ui
#' @keywords internal
#' @author Ghislain Durif
microsat_group_prior_server <- function(input, output, session) {}

#' Sequence group prior setup ui
#' @keywords internal
#' @author Ghislain Durif
sequence_group_prior_ui <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            title = "Setup Sequence group priors",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            "TODO"
        )
    )
}

#' Sequence group prior setup ui
#' @keywords internal
#' @author Ghislain Durif
sequence_group_prior_server <- function(input, output, session) {}

