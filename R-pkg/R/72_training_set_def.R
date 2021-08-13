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
    
    # modified scenario list
    observeEvent(local$scenario_list, {
        if(!identical(env$ts$scenario_list, local$scenario_list)) {
            local$validated <- FALSE
        } else {
            local$validated <- TRUE
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
        if(!identical(env$ts$scenario_list, local$scenario_list)) {
            env$ts$scenario_list <- local$scenario_list
        }
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
        } else if(!identical(env$ts$scenario_list, local$scenario_list)) {
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
        modified_prior_list = NULL, validated = TRUE
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
            local$validated <- FALSE
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
        
        if(!identical(
            sort(env$ts$prior_list), sort(local$modified_prior_list)
        )) {
            local$validated <- FALSE
        } else {
            local$validated <- TRUE
        }
    })
    
    ## validate prior list
    observeEvent(input$validate, {
        req(length(local$modified_prior_list > 0))
        local$validated <- TRUE
        if(!identical(
            sort(env$ts$prior_list), sort(local$modified_prior_list)
        )) {
            env$ts$prior_list <- local$modified_prior_list
        }
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
        } else if(isTruthy(local$modified_prior_list) && !local$validated) {
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
prior_list_def_server <- function(
    input, output, session, prior_list = reactive({NULL}), 
    type = reactive({NULL})
) {
    
    # FIXME
    
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
    #     pprint(local$counter)
    # })
    
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

#' Prior definition module server
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
        sd_min = NULL, sd_max = NULL, sd_step = NULL,
        render = 0
    )
    
    # get input
    observe({
        local$prior <- prior()
        local$type <- type()
        local$render <- isolate(local$render) + 1
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
    observeEvent(local$prior, {
        req(local$prior)
        if(check_header_prior(local$prior)) {
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
                        ns("distrib"),
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
    
    ## disable mean and stdev if uniform or log-uniform
    observeEvent(c(
        input$distrib, local$render
    ), {
        req(input$distrib)
        if(input$distrib %in% c("UN", "LU")) {
            updateNumericInput(session, "mean", value = 0)
            updateNumericInput(session, "stdev", value = 0)
            shinyjs::disable("mean")
            shinyjs::disable("stdev")
        } else {
            shinyjs::enable("mean")
            shinyjs::enable("stdev")
        }
    })
    
    # # debug
    # observe({
    #     pprint(local$render)
    # })
    
    #### check for missing input
    ## min
    observeEvent(input$min, {
        req(local$name)
        feedbackWarning(
            "min", !isTruthy(input$min),
            "Missing value."
        )
    })
    
    ## max
    observeEvent(input$max, {
        req(local$name)
        feedbackWarning(
            "max", !isTruthy(input$max),
            "Missing value."
        )
    })
    
    ## mean
    observeEvent(input$mean, {
        req(local$name)
        feedbackWarning(
            "mean", !isTruthy(input$mean),
            "Missing value."
        )
    })
    
    ## stdev
    observeEvent(input$stdev, {
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
            "min", (input$min > input$max), "min > max"
        )
        feedbackWarning(
            "max", (input$min > input$max), "min > max"
        )
    })
    
    ## check for normal and log-normal parameter setting
    observe({
        req(local$name)
        req(input$distrib)
        req(input$min)
        req(input$mean)
        feedbackWarning(
            "mean", 
            input$distrib %in% c("NO", "LN") && 
                (input$mean < input$min),
            "mean < min"
        )
        feedbackWarning(
            "min", 
            input$distrib %in% c("NO", "LN") && 
                (input$mean < input$min),
            "mean < min"
        )
    })
    
    observe({
        req(local$name)
        req(input$distrib)
        req(input$max)
        req(input$mean)
        feedbackWarning(
            "mean", 
            input$distrib %in% c("NO", "LN") && 
                (input$mean > input$max),
            "mean > max"
        )
        feedbackWarning(
            "max", 
            input$distrib %in% c("NO", "LN") && 
                (input$mean > input$max),
            "mean > max"
        )
    })
    
    # # debug
    # observe({
    #     logging("distrib = ", input$distrib)
    #     logging("min = ", input$min)
    #     logging("max = ", input$max)
    #     logging("mean = ", input$mean)
    #     logging("stdev = ", input$stdev)
    # })
    
    ## encode output
    observe({
        
        # pprint(local$name)
        # pprint(local$type)
        # pprint(input$distrib)
        # pprint(input$min)
        # pprint(input$max)
        # pprint(input$mean)
        # pprint(input$stdev)
        
        out$encoding <- NULL
        out$valid <- FALSE
        req(local$name)
        req(local$type)
        req(input$distrib)
        req(input$min)
        req(input$max)
        req(input$mean)
        req(input$stdev)
        out$valid <- !((input$min >= input$max) || 
            ((input$distrib %in% c("NO", "LN")) &&
                 (input$mean < input$min || input$mean > input$max)))
        if(out$valid) {
            out$encoding <- str_c(
                local$name, 
                local$type, 
                str_c(
                    input$distrib, "[",
                    input$min, ",", input$max, ",",
                    input$mean, ",", input$stdev, "]"
                ),
                sep = " "
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
        h3(
            icon("signal"), 
            "Microsat/Sequence group priors and mutation model"
        ),
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
    
    # locus description (default or given in the header)
    observeEvent({
        c(env$ap$locus_type, env$ap$data_check, env$ts$locus_desc)
    }, {
        
        req(env$ap$locus_type)
        req(env$ap$locus_type == "mss")
        req(env$ap$data_check)
        req(env$ap$data_check$valid)
        req(env$ap$data_check$locus_name)
        req(env$ap$data_check$locus_type)
        req(env$ap$data_check$locus_mode)
        if(any(env$ap$data_check$locus_type == "S")) {
            req(env$ap$data_check$seq_length)
        }
        
        if(!isTruthy(env$ts$locus_desc) || 
           !check_header_locus_desc(env$ts$locus_desc, type = "mss")) {
            env$ts$locus_desc <- default_mss_locus_desc(
                env$ap$data_check$locus_name, 
                env$ap$data_check$locus_type, 
                env$ap$data_check$locus_mode, 
                env$ap$data_check$seq_length
            )
        }
    })
    
    # microsat
    output$microsat <- renderUI({
        req(env$ap$locus_type)
        req(env$ap$locus_type == "mss")
        req(env$ap$data_check)
        req(env$ap$data_check$valid)
        req(env$ap$data_check$locus_mode)
        req(any(env$ap$data_check$locus_mode == "M"))
        tagList(
            mss_locus_list_config_ui(ns("microsat_config"))
        )
    })
    callModule(
        mss_locus_list_config_server, "microsat_config", local_mode = "M"
    )
    
    # sequence
    output$sequence <- renderUI({
        req(env$ap$locus_type)
        req(env$ap$locus_type == "mss")
        req(env$ap$data_check)
        req(env$ap$data_check$valid)
        req(env$ap$data_check$locus_mode)
        req(any(env$ap$data_check$locus_mode == "S"))
        tagList(
            mss_locus_list_config_ui(ns("sequence_config"))
        )
    })
    callModule(
        mss_locus_list_config_server, "sequence_config", local_mode = "S"
    )
}

#' Number of group setup ui
#' @keywords internal
#' @author Ghislain Durif
n_group_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(
                width = 4,
                uiOutput(ns("n_group"))
            ),
            column(
                width = 8,
                actionGroupButtons(
                    inputIds = c(ns("add_group"), ns("rm_group")),
                    labels = list(
                        tags$span(icon("plus"), "Add group"),
                        tags$span(icon("minus"), "Remove group")
                    ),
                    fullwidth = TRUE
                )
            )
        ),
        helpText(
            icon("warning"),
            "Choose the definitive number of groups",
            tags$b("before"),
            "assigning locus to new groups."
        ),
        helpText(
            icon("warning"),
            "Be advised that,",
            "after affecting loci to groups,", 
            tags$b("when you validate"), ",", 
            "the", tags$b("group ids"), "will be potentially", 
            tags$b("reordered and reindexed"), ",",
            "so that the first locus is in the first group,",
            "and to avoid empty group ids.",
            "However, the grouping strategy you choose will not be affected,",
            "only the group ids (if reordering/reindexing is required)."
        )
    )
}

#' Number of group setup server
#' @keywords internal
#' @author Ghislain Durif
n_group_server <- function(
    input, output, session, n_group = reactive({NULL})
) {
    # init local
    local <- reactiveValues(n_group = 1)
    
    # init output
    out <- reactiveValues(n_group = NULL)
    
    # get input
    observe({
        req(n_group())
        local$n_group <- n_group()
    })
    
    # add a group
    observeEvent(input$add_group, {
        local$n_group <- ifelse(is.null(local$n_group), 0, local$n_group) + 1
    })
    
    # remove a group
    observeEvent(input$rm_group, {
        req(is.numeric(local$n_group))
        req(local$n_group > 1)
        local$n_group <- local$n_group - 1
    })
    
    # rendering
    output$n_group <- renderUI({
        req(local$n_group)
        tagList(
            h5(tags$b("Number of groups =", as.character(local$n_group)))
        )
    })
    
    # output
    observeEvent(local$n_group, {
        req(is.numeric(local$n_group))
        req(local$n_group > 0)
        out$n_group <- local$n_group
    })
    return(out)
}

#' MSS locus list config ui
#' @keywords internal
#' @author Ghislain Durif
mss_locus_list_config_ui <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            title = textOutput(ns("title")),
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            tagList(
                n_group_ui(ns("n_group")),
                uiOutput(ns("help")),
                uiOutput(ns("locus_list")),
                hr(),
                fluidRow(
                    column(
                        width = 4,
                        actionButton(
                            ns("validate"),
                            label = "Validate",
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

#' MSS locus list config server
#' @keywords internal
#' @author Ghislain Durif
mss_locus_list_config_server <- function(
    input, output, session, local_mode = "M"
) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        group_id = NULL, initial_locus_desc = NULL,
        locus_desc = NULL, unused_locus_desc = NULL, 
        locus_mask = NULL, locus_def = NULL, 
        modified_locus_desc = NULL, validated = TRUE
    )
    
    # title
    output$title <- renderText({
        if(local_mode == "M") {
            "Setup Microsat locus configuration"
        } else if(local_mode == "S") {
            "Setup Sequence locus configuration"
        } else {
            NULL
        }
    })
    
    # help
    output$help <- renderUI({
        if(local_mode == "M") {
            helpText(
                icon("info-circle"),
                "By default (for new projects),",
                "all Microsat loci are assumed to be dinucleid",
                "(motif = 2) with a range of 40."
            )
        } else if(local_mode == "S") {
            helpText(
                icon("info-circle"),
                "All Sequence lengths are inferred from the data file."
            )
        } else {
            NULL
        }
    })
    
    # current locus description (default or given in the header)
    observeEvent({
        c(env$ap$locus_type, env$ap$data_check, env$ts$locus_desc)
    }, {
        
        # pprint(local_mode)
        # pprint(env$ap$locus_type)
        # pprint(env$ap$data_check)
        # pprint(env$ap$data_check$valid)
        # pprint(env$ts$locus_desc)
        
        req(env$ap$locus_type)
        req(env$ap$locus_type == "mss")
        req(env$ap$data_check)
        req(env$ap$data_check$valid)
        req(env$ap$data_check$locus_mode)
        req(any(env$ap$data_check$locus_mode == local_mode))
        req(env$ts$locus_desc)
        
        local$initial_locus_desc <- env$ts$locus_desc
    })
    
    # extract and parse locus description corresponding to given mode (M or S)
    observeEvent(local$initial_locus_desc, {
        req(local$initial_locus_desc)
        
        # if(local_mode == "S") {
        #     pprint(local$initial_locus_desc)
        #     pprint(env$ap$data_check$locus_mode)
        # }
        
        local$locus_mask <- (env$ap$data_check$locus_mode == local_mode)
        local$locus_desc <- local$initial_locus_desc[local$locus_mask]
        local$unused_locus_desc <- local$initial_locus_desc[!local$locus_mask]

        local$group_id <- unique(as.integer(str_extract(
            local$locus_desc, "(?<=G)[0-9]+"
        )))

        local$n_group <- length(unique(local$group_id))
        
        # if(local_mode == "S") {
        #     pprint(local$locus_mask)
        #     pprint(local$locus_desc)
        #     pprint(local$unused_locus_desc)
        #     pprint(local$group_id)
        # }
    })
    
    # number of groups
    n_group_server <- callModule(
        n_group_server, "n_group", n_group = reactive(local$n_group)
    )
    observeEvent(n_group_server$n_group, {
        req(n_group_server$n_group)
        req(local$n_group)
        req(local$group_id)
        if(n_group_server$n_group > local$n_group) {
            local$group_id <- c(
                local$group_id,
                max(local$group_id) + 
                    1:(n_group_server$n_group - local$n_group)
            )
        } else {
            local$group_id <- local$group_id[1:n_group_server$n_group]
        }
        local$n_group <- n_group_server$n_group
    })
    
    # render locus description setup
    output$locus_list <- renderUI({
        req(local$locus_desc)
        do.call(
            tagList,
            unname(lapply(
                1:length(local$locus_desc),
                function(ind) {
                    return(mss_locus_config_ui(ns(str_c("locus", ind))))
                }
            ))
        )
    })
    
    # locus description setup server-side
    observe({
        req(local$locus_desc)
        req(local$group_id)
        local$locus_def <- lapply(
            1:length(local$locus_desc),
            function(ind) {
                callModule(
                    mss_locus_config_server,
                    str_c("locus", ind), local_mode = local_mode,
                    locus_desc = reactive(local$locus_desc[ind]),
                    group_id = reactive(local$group_id)
                )
            }
        )
    })
    # get input
    observe({
        req(local$locus_def)
        local$modified_locus_desc <- unname(unlist(lapply(
           local$locus_def, function(item) {
               req(item$locus_desc)
               return(item$locus_desc)
           }
        )))
    })
    
    # modified input ?
    observeEvent(local$modified_locus_desc, {
        req(local$locus_desc)
        req(local$modified_locus_desc)
        if(!identical(local$locus_desc, local$modified_locus_desc)) {
            local$validated <- FALSE
        }
    })
    
    # feedback
    output$feedback <- renderUI({
        if(!local$validated) {
            tagList(
                tags$p(
                    tags$div(
                        icon("warning"), 
                        "Configuration was modified. Please validate.",
                        style = "color: #F89406;"
                    )
                )
            )
        } else {
            NULL
        }
    })
    
    # validation
    observeEvent(input$validate, {
        
        # print("#################### validate ####################")
        # pprint(local$initial_locus_desc)
        # pprint(env$ap$data_check$locus_mode)
        # pprint(local$locus_mask)
        # pprint(local$modified_locus_desc)
        # pprint(local$unused_locus_desc)
        
        req(local$initial_locus_desc)
        req(env$ap$data_check$locus_mode)
        req(local$locus_mask)
        req(local$modified_locus_desc)
        if(sum(local$locus_mask) < length(local$locus_mask)) {
            req(local$unused_locus_desc)
        }
        
        local$validated <- TRUE
        
        # final locus description
        final_locus_desc <- character(length(local$initial_locus_desc))
        
        ### correct group (if necessary) for current mode
        start_id <- 1
        if(sum(local$locus_mask) < length(local$locus_mask)) {
            # group start to 1 for first locus mode
            if(head(env$ap$data_check$locus_mode, 1) != local_mode) {
                start_id <- max(unique(as.integer(str_extract(
                    local$unused_locus_desc, "(?<=G)[0-9]+"
                )))) + 1
            }
        }
        
        # correct group id for current mode
        final_locus_desc[local$locus_mask] <- correct_mss_locus_desc_group_id(
            local$modified_locus_desc, start_id
        )
        
        ### correct group (if necessary) for other mode
        start_id <- 1
        if(sum(local$locus_mask) < length(local$locus_mask)) {
            # group start to 1 for first locus mode
            if(head(env$ap$data_check$locus_mode, 1) == local_mode) {
                start_id <- max(unique(as.integer(str_extract(
                    final_locus_desc[local$locus_mask], "(?<=G)[0-9]+"
                )))) + 1
            }
            
            # correct group id for current mode
            final_locus_desc[!local$locus_mask] <- 
                correct_mss_locus_desc_group_id(
                    local$unused_locus_desc, start_id
                )
        }
        
        # ### debug
        # pprint(final_locus_desc)
        
        ### finally store result (if relevant)
        if(!identical(final_locus_desc, env$ts$locus_desc)) {
            env$ts$locus_desc <- final_locus_desc
        }
    })
}

#' MSS locus config ui
#' @keywords internal
#' @author Ghislain Durif
mss_locus_config_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("locus_setup"))
    )
}

#' MSS locus config server
#' @keywords internal
#' @author Ghislain Durif
mss_locus_config_server <- function(
    input, output, session, local_mode = "M", locus_desc = reactive({NULL}),
    group_id = reactive({NULL})
) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        name = NULL,
        group = NULL,
        motif = NULL,
        range = NULL,
        length = NULL,
        fixed_header = NULL,
        # input
        locus_desc = NULL,
        group_id = NULL
    )
    
    # init output
    out <- reactiveValues(locus_desc = NULL)
    
    # get input
    observe({
        local$locus_desc <- locus_desc()
        local$group_id <- group_id()
    })
    
    # parse locus description
    observeEvent(local$locus_desc, {
        req(local$locus_desc)
        
        local$fixed_header <- str_extract(local$locus_desc, "^.+(?= G[0-9]+)")
        
        local$name <- str_extract(
            local$locus_desc, 
            str_c("^", single_param_regex(), "(?= )")
        )
        
        local$group <- str_extract(local$locus_desc, "(?<=G)[0-9]+")
        
        if(local_mode == "M") {
            local$motif <- as.integer(str_extract(
                local$locus_desc, "(?<= )[0-9]+(?= )"
            ))
            
            local$range <- as.integer(str_extract(
                local$locus_desc, "(?<= )[0-9]+$"
            ))
        } else if(local_mode == "S") {
            local$length <- as.integer(str_extract(
                local$locus_desc, "(?<= )[0-9]+$"
            ))
        }
    })
    
    # render locus setup
    output$locus_setup <- renderUI({
        
        # pprint(local$name)
        # pprint(local$group)
        # pprint(local$motif)
        # pprint(local$range)
        # pprint(local$length)
        # pprint(local$group_id)
        
        req(local$name)
        req(local$group)
        req(local$group_id)
        
        if(local_mode == "M") {
            req(local$motif)
            req(local$range)
            tagList(fluidRow(
                column(
                    width = 3,
                    shinyjs::disabled(textInput(
                        ns("name"), label = "Locus",
                        value = local$name
                    ))
                ),
                column(
                    width = 3,
                    selectInput(
                        ns("group"), label = "Group",
                        choices = as.character(local$group_id),
                        selected = as.character(local$group)
                    )
                ),
                column(
                    width = 3,
                    numericInput(
                        ns("motif"), label = "Motif",
                        value = local$motif, min = 0, max = 10
                    )
                ),
                column(
                    width = 3,
                    numericInput(
                        ns("range"), label = "Range", value = local$range,
                        min = 10,max = 100
                    )
                )
            ))
            
        } else if(local_mode == "S") {
            req(local$length)
            tagList(fluidRow(
                column(
                    width = 4,
                    shinyjs::disabled(textInput(
                        ns("name"), label = "Locus",
                        value = local$name
                    ))
                ),
                column(
                    width = 4,
                    selectInput(
                        ns("group"), label = "Group",
                        choices = as.character(local$group_id),
                        selected = as.character(local$group)
                    )
                ),
                column(
                    width = 4,
                    numericInput(
                        ns("length"), label = "Length",
                        value = local$length, min = 0, max = NA
                    )
                )
            ))
        } else {
            NULL
        }
    })
    
    # parse output
    observe({
        req(input$group)
        
        if(local_mode == "M") {
            req(input$motif)
            req(input$range)
            out$locus_desc <- str_c(
                local$fixed_header,
                " G", as.character(input$group),
                " ", as.character(input$motif),
                " ", as.character(input$range)
            )
        } else if(local_mode == "S") {
            req(input$length)
            out$locus_desc <- str_c(
                local$fixed_header,
                " G", as.character(input$group),
                " ", as.character(input$length)
            )
        }
    })
    
    # # debug
    # observe({
    #     pprint(out$locus_desc)
    # })
    
    # output
    return(out)
}

#' MSS group prior setup ui
#' @keywords internal
#' @author Ghislain Durif
mss_group_prior_ui <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            title = "Setup mutation model on the groups",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            tagList(
                uiOutput(ns("group_prior")),
                hr(),
                fluidRow(
                    column(
                        width = 4,
                        actionButton(
                            ns("validate"),
                            label = "Validate",
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

#' MSS group prior setup ui
#' @keywords internal
#' @author Ghislain Durif
mss_group_prior_server <- function(input, output, session) {
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        group_prior_list = NULL, group_desc = NULL,
        modified_group_prior_list = NULL, validated = TRUE
    )
    
    # env group prior description (default or given in the header)
    # and corresponding locus_mode/group_id
    observeEvent({
        c(env$ap$locus_type, env$ap$data_check, 
          env$ts$locus_desc, env$ts$group_prior_list)
    }, {
        req(env$ap$locus_type)
        req(env$ap$locus_type == "mss")
        req(env$ap$data_check)
        req(env$ap$data_check$valid)
        req(env$ts$locus_desc)
        
        # extract group description
        local$group_desc <- get_group_desc(env$ts$locus_desc)
        req(local$group_desc$locus_mode)
        req(local$group_desc$group_id)
        
        # check group prior list (if existing)
        tmp_check <- check_group_prior(
            env$ts$group_prior_list, 
            locus_mode = local$group_desc$locus_mode, 
            group_id = local$group_desc$group_id
        )
        
        # update local
        if(tmp_check) {
            local$group_prior_list <- env$ts$group_prior_list
            local$validated <- TRUE
        } else {
            local$group_prior_list <- default_mss_group_prior(
                env$ts$locus_desc
            )
            local$validated <- FALSE
        }
    })
    
    # # debugging
    # observe({
    #     pprint(local$group_desc)
    #     pprint(local$group_prior_list)
    # })
    
    # render group prior
    output$group_prior <- renderUI({
        req(env$ap$locus_type)
        req(env$ap$locus_type == "mss")
        req(local$group_prior_list)
        req(local$group_desc)
        req(is.data.frame(local$group_desc))
        req(local$group_desc$group_id)
        req(local$group_desc$locus_mode)
        req(length(local$group_prior_list) == nrow(local$group_desc))
        
        do.call(
            tagList,
            unname(lapply(
                1:length(local$group_prior_list), 
                function(ind) {
                    tmp_group_id <- local$group_desc$group_id[ind]
                    tmp_locus_mode <- switch (
                        local$group_desc$locus_mode[ind],
                        "M" = "Microsat",
                        "S" = "Sequence"
                    )
                    tmp_title <- h4(tags$ul(tags$li(
                        "Group", tmp_group_id, str_c("(", tmp_locus_mode, ")")
                    )))
                    return(tagList(
                        tmp_title,
                        group_prior_def_ui(
                            ns(str_c("prior_", tmp_group_id))
                        )
                    ))
                }
            ))
        )
    })
    
    # get output
    observe({
        
        req(env$ap$locus_type)
        req(env$ap$locus_type == "mss")
        req(local$group_prior_list)
        req(local$group_desc)
        req(is.data.frame(local$group_desc))
        req(local$group_desc$group_id)
        req(local$group_desc$locus_mode)
        req(length(local$group_prior_list) == nrow(local$group_desc))
        
        local$group_prior_def <- lapply(
            1:length(local$group_prior_list),
            function(ind) {
                return(callModule(
                    group_prior_def_server, 
                    str_c("prior_", local$group_desc$group_id[ind]),
                    group_prior = local$group_prior_list[[ind]], 
                    group_id = local$group_desc$group_id[ind],
                    locus_mode = local$group_desc$locus_mode[ind]
                ))
            }
        )
    })
    
    # merge output
    observe({
        req(local$group_prior_def)
        local$modified_group_prior_list <- lapply(
            local$group_prior_def,
            function(item) {
                req(item$group_prior)
                return(item$group_prior)
            }
        )
    })
    
    # # debugging
    # observeEvent(local$modified_group_prior_list, {
    #     pprint(local$modified_group_prior_list)
    # })
    
    # output was modified by user ?
    observeEvent(local$modified_group_prior_list, {
        req(local$group_prior_list)
        req(local$modified_group_prior_list)
        if(!identical(
            local$group_prior_list, local$modified_group_prior_list
        )) {
            local$validated <- FALSE
        }
    })
    
    # validation ?
    observeEvent(input$validate, {
        req(local$modified_group_prior_list)
        
        # check group prior list (if existing)
        req(local$group_desc$locus_mode)
        req(local$group_desc$group_id)
        
        tmp_check <- check_group_prior(
            local$modified_group_prior_list, 
            locus_mode = local$group_desc$locus_mode, 
            group_id = local$group_desc$group_id
        )
        
        # update env if group prior list has been modified (and is valid)
        if(tmp_check) {
            local$validated <- TRUE
            if(!identical(
                env$ts$group_prior_list, local$modified_group_prior_list
            )) {
                env$ts$group_prior_list <- local$modified_group_prior_list
            }
        }
    })
    
    # # debugging
    # observeEvent(env$ts$group_prior_list, {
    #     pprint(env$ts$group_prior_list)
    # })
    
    # feedback
    output$feedback <- renderUI({
        if(!local$validated) {
            tagList(
                tags$p(
                    tags$div(
                        icon("warning"), 
                        "Configuration was modified. Please validate.",
                        style = "color: #F89406;"
                    )
                )
            )
        } else {
            NULL
        }
    })
}

#' MSS group prior parameter setting module ui
#' @keywords internal
#' @author Ghislain Durif
group_prior_def_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("mut_model_def")),
        uiOutput(ns("prior_def"))
    )
}

#' MSS group prior parameter setting module server
#' @keywords internal
#' @author Ghislain Durif
group_prior_def_server <- function(
    input, output, session, group_prior = NULL, 
    group_id = NULL, locus_mode = NULL
) {
    # namespace
    ns <- session$ns

    # init local
    local <- reactiveValues(
        counter = 0, 
        param_desc = NULL,
        mut_model = NULL, mut_model_desc = mutation_model_desc(),
        modified_mut_model = NULL, modified_mut_model_name = NULL,
        output_mut_model = NULL, 
        mean_group_prior_def = NULL, indiv_group_prior_def = NULL, 
        tmp_mean_group_prior = NULL, tmp_indiv_group_prior = NULL, 
        tmp_group_prior = NULL, modified_group_prior = NULL,
        # INPUT
        unsplit_group_prior = NULL, group_prior = NULL, group_id = NULL, 
        locus_mode = NULL
    )
    
    # init output
    out <- reactiveValues(group_prior = NULL)
    
    # get input
    observe({
        local$unsplit_group_prior = group_prior
        local$group_id = group_id
        local$locus_mode = locus_mode
    })
    
    # parse input
    observeEvent(
        c(local$unsplit_group_prior, local$group_id, local$locus_mode),
    {
        req(local$unsplit_group_prior)
        req(local$group_id)
        req(local$locus_mode)

        # split input prior by line
        local$group_prior <- unlist(str_split(
            local$unsplit_group_prior, "\n"
        ))

        # description of parameter for given locus mode
        local$param_desc <- group_prior_param_desc(
            locus_mode = local$locus_mode
        )
        
        # mutation model (if relevant)
        if(local$locus_mode == "S") {
            local$mut_model <- unlist(tail(local$group_prior, 1))
        }
    })
    
    # # debugging
    # observe({
    #     pprint(local$group_prior)
    #     pprint(local$group_id)
    #     pprint(local$locus_mode)
    #     pprint(local$mut_model)
    # })
    
    # render mutation model choice (if relevant)
    output$mut_model_def <- renderUI({
        req(local$locus_mode == "S")
        tagList(
            seq_mutation_model_def_ui(ns("mut_model"))
        )
    })

    ## get mutation model (if relevant)
    observe({
        req(local$locus_mode == "S")
        req(local$mut_model)
        local$output_mut_model <- callModule(
            seq_mutation_model_def_server, "mut_model", 
            mut_model = reactive(local$mut_model)
        )
    })

    observe({
        req(local$locus_mode == "S")
        req(local$output_mut_model$mut_model_name)
        req(local$output_mut_model$mut_model)
        local$modified_mut_model_name <- local$output_mut_model$mut_model_name
        local$modified_mut_model <- local$output_mut_model$mut_model
    })
    
    # # debugging
    # observe({
    #     pprint(local$modified_mut_model_name)
    #     pprint(local$modified_mut_model)
    # })

    ## render ui for group prior setup
    output$prior_def <- renderUI({
        req(local$group_prior)
        req(local$param_desc$param)

        param_name <- local$param_desc$param

        do.call(
            tagList,
            unname(lapply(
                1:3,
                function(ind) {
                    tagList(
                        mean_group_prior_def_ui(ns(param_name[2*ind - 1])),
                        indiv_group_prior_def_ui(ns(param_name[2*ind]))
                    )
                }
            ))
        )
    })
    
    # get user input for group prior setup regarding mean prior
    observeEvent(c(
        local$group_prior, local$param_desc, local$modified_mut_model_name
    ), {
        # print("=========== RENDER MEAN ===========")
        req(local$group_prior)
        req(local$param_desc$param)
        req(local$locus_mode)
        
        seq_param <- NULL
        tmp_mut_model_desc <- NULL
        
        if(local$locus_mode == "S") {
            req(local$modified_mut_model_name)
            
            seq_param <- c("MU", "K1", "K2")
            
            tmp_mut_model_desc <- subset(
                local$mut_model_desc,
                local$mut_model_desc$model == local$modified_mut_model_name,
            )
            req(nrow(tmp_mut_model_desc) == 1)
        }
        
        param_name <- local$param_desc$param
        
        local$mean_group_prior_def <- lapply(
            1:3,
            function(ind) {
                show_input <- TRUE
                
                # hide prior depending on mutation model (if sequence data)
                if(local$locus_mode == "S") {
                    tmp_seq_param <- seq_param[ind]
                    show_input <- as.logical(
                        tmp_mut_model_desc[tmp_seq_param]
                    )
                }
                callModule(
                    mean_group_prior_def_server, param_name[2*ind - 1],
                    prior = reactive(local$group_prior[2*ind]),
                    locus_mode = local$locus_mode,
                    show_input = show_input
                )
            }
        )
    })
    
    # get user input for group prior setup
    observeEvent(c(
        local$group_prior, local$param_desc, local$modified_mut_model_name
    ), {
        # print("=========== RENDER INDIV ===========")
        req(local$group_prior)
        req(local$param_desc$param)
        req(local$locus_mode)
        
        seq_param <- NULL
        tmp_mut_model_desc <- NULL
        
        if(local$locus_mode == "S") {
            req(local$modified_mut_model_name)
            
            seq_param <- c("MU", "K1", "K2")
            
            tmp_mut_model_desc <- subset(
                local$mut_model_desc,
                local$mut_model_desc$model == local$modified_mut_model_name,
            )
            req(nrow(tmp_mut_model_desc) == 1)
        }
        
        param_name <- local$param_desc$param
        
        local$indiv_group_prior_def <- lapply(
            1:3,
            function(ind) {
                show_input <- TRUE
                
                # hide prior depending on mutation model (if sequence data)
                if(local$locus_mode == "S") {
                    tmp_seq_param <- seq_param[ind]
                    show_input <- as.logical(
                        tmp_mut_model_desc[tmp_seq_param]
                    )
                }
                
                callModule(
                    indiv_group_prior_def_server, param_name[2*ind],
                    prior = reactive(local$group_prior[2*ind + 1]),
                    locus_mode = local$locus_mode,
                    show_input = show_input
                )
            }
        )
    })
    
    # # debug
    # observe({
    #     print("-------------- DEBUG group_prior_def_server --------------")
    #     print("MEAN")
    #     req(local$mean_group_prior_def)
    #     lapply(
    #         local$mean_group_prior_def,
    #         function(item) {
    #             req(item)
    #             print(reactiveValuesToList(item))
    #         }
    #     )
    #     print("INDIV")
    #     req(local$indiv_group_prior_def)
    #     lapply(
    #         local$indiv_group_prior_def,
    #         function(item) {
    #             req(item)
    #             print(reactiveValuesToList(item))
    #         }
    #     )
    # })
    
    # collect user input for group prior setup
    observe({
        # print("=========== COLLECT MEAN ===========")
        req(local$mean_group_prior_def)

        # mean group prior extraction
        local$tmp_mean_group_prior <- unlist(lapply(
            local$mean_group_prior_def,
            function(item) {
                if(item$valid && isTruthy(item$encoding)) {
                    return(item$encoding)
                } else {
                    return(NA)
                }
            }
        ))
    })
    
    observe({
        # print("=========== COLLECT INDIV ===========")
        req(local$indiv_group_prior_def)

        # indiv group prior extraction
        local$tmp_indiv_group_prior <- unlist(lapply(
            local$indiv_group_prior_def,
            function(item) {
                if(item$valid && isTruthy(item$encoding)) {
                    return(item$encoding)
                } else {
                    return(NA)
                }
            }
        ))
    })
    
    # parse user input for group prior setup
    observeEvent(c(
        local$tmp_mean_group_prior, local$tmp_indiv_group_prior, 
        local$modified_mut_model
    ), {
        req(local$group_prior)
        req(local$tmp_mean_group_prior)
        req(local$tmp_indiv_group_prior)
        req(identical(length(local$tmp_mean_group_prior), 3L))
        req(identical(length(local$tmp_indiv_group_prior), 3L))
        
        local$tmp_group_prior <- unlist(lapply(
            1:3,
            function(ind) return(c(
                local$tmp_mean_group_prior[ind],
                local$tmp_indiv_group_prior[ind]
            ))
        ))
        
        # group information
        local$tmp_group_prior <- c(
            unlist(head(local$group_prior, 1)), local$tmp_group_prior
        )
        
        # mutation model (if relevant)
        if(local$locus_mode == "S") {
            req(local$modified_mut_model)
            local$tmp_group_prior <- c(
                local$tmp_group_prior, local$modified_mut_model
            )
        }
    })

    # # debugging
    # observe({
    #     pprint(local$tmp_group_prior)
    # })

    # merge user input with original group prior (in case of NA)
    observeEvent(local$tmp_group_prior, {
        req(local$group_prior)
        req(local$tmp_group_prior)
        
        # check if issue with total length
        req(identical(length(local$group_prior),length(local$tmp_group_prior)))

        # merge
        local$modified_group_prior <- ifelse(
            !is.na(local$tmp_group_prior),
            local$tmp_group_prior,
            local$group_prior
        )
    })
    
    # ## debugging
    # observe({
    #     pprint(local$modified_group_prior)
    # })
    
    ## prepare output
    observeEvent(local$modified_group_prior, {
        req(local$modified_group_prior)
        out$group_prior <- str_c(local$modified_group_prior, collapse = "\n")
    })

    
    # ## debugging
    # observe({
    #     pprint(out$group_prior)
    # })

    ## output
    return(out)
}

#' MSS mean group prior definition ui
#' @keywords internal
#' @author Ghislain Durif
mean_group_prior_def_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("prior_def"))
    )
}

#' MSS group prior definition server
#' @keywords internal
#' @author Ghislain Durif
mean_group_prior_def_server <- function(
    input, output, session, prior = reactive({NULL}), locus_mode = "M", 
    show_input = TRUE
) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        name = NULL, desc = NULL, distrib = NULL,
        min = NULL, max = NULL, mean = NULL, 
        stdev = NULL, note = NULL,
        # numeric input setup
        input_min = 0, input_max = NA, input_step = 0.00001,
        # module input
        prior = NULL
    )
    
    # get input
    observe({
        local$prior <- prior()
    })
    
    # init output
    out <- reactiveValues(encoding = NULL, valid = TRUE)
    
    # # debugging
    # observe({
    #     pprint(local$prior)
    #     pprint(locus_mode)
    # })
    
    # parse input
    observe({
        req(locus_mode)
        if(isTruthy(local$prior) && 
           check_mean_group_prior(local$prior, locus_mode)) {
            
            param_desc <- group_prior_param_desc(locus_mode)
            
            local$name <- get_group_prior_param(local$prior, locus_mode)
            local$desc <- param_desc$desc[param_desc$param == local$name]
            local$note <- param_desc$note[param_desc$param == local$name]
            local$distrib <- get_group_prior_distrib(local$prior)
            
            tmp_val <- get_group_prior_val(local$prior)
            
            local$min <- as.numeric(tmp_val[1])
            local$max <- as.numeric(tmp_val[2])
            local$mean <- as.numeric(tmp_val[3])
            local$stdev <- as.numeric(tmp_val[4])
        } else {
            local$name <- NULL
            local$desc <- NULL
            local$note <- NULL
            local$distrib <- NULL
            local$min <- NULL
            local$max <- NULL
            local$mean <- NULL
            local$stdev <- NULL
        }
    })
    
    # render input
    output$prior_def <- renderUI({
        
        req(show_input)
        
        req(local$name)
        req(local$desc)
        req(local$distrib)
        req(local$min)
        req(local$max)
        req(local$mean)
        req(local$stdev)
        
        input_min <- numericInput(
            ns("min"), label = NULL, value = local$min,
            min = local$input_min, max = local$input_max,
            step = local$input_step
        )
        
        input_max <- numericInput(
            ns("max"), label = NULL, value = local$max,
            min = local$input_min, max = local$input_max,
            step = local$input_step
        )
        
        input_mean <- numericInput(
            ns("mean"), label = NULL, value = local$mean,
            min = local$input_min, max = local$input_max,
            step = local$input_step
        )
        
        input_stdev <- numericInput(
            ns("stdev"), label = NULL, value = local$stdev,
            min = local$input_min, max = local$input_max,
            step = local$input_step
        )
        
        title_style <- 
            "text-align:right;margin-right:1em;vertical-align:middle;"
        
        param_title <- tags$h5(local$desc)
        if(isTruthy(local$note)) {
            param_title <- tags$h5(local$desc) %>% helper(
                type = "inline", 
                content = local$note
            )
        }
        
        tagList(
            param_title,
            fluidRow(
                column(
                    width = 4,
                    selectInput(
                        ns("distrib"),
                        label = NULL,
                        choices = list("Uniform" = "UN", "Log-Unif." = "LU",
                                       "Gamma" = "GA"),
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
    
    ## disable mean and stdev if uniform or log-uniform
    observe({
        req(input$distrib)
        if(input$distrib %in% c("UN", "LU")) {
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
            "min", (input$min > input$max), "min > max"
        )
        feedbackWarning(
            "max", (input$min > input$max), "min > max"
        )
    })
    
    ## check for normal and log-normal parameter setting
    observe({
        req(local$name)
        req(input$distrib)
        req(input$min)
        req(input$mean)
        feedbackWarning(
            "mean", 
            input$distrib %in% c("GA") && 
                (input$mean < input$min),
            "mean < min"
        )
        feedbackWarning(
            "min", 
            input$distrib %in% c("GA") && 
                (input$mean < input$min),
            "mean < min"
        )
    })
    
    observe({
        req(local$name)
        req(input$distrib)
        req(input$max)
        req(input$mean)
        feedbackWarning(
            "mean", 
            input$distrib %in% c("NO", "LN") && 
                (input$mean > input$max),
            "mean > max"
        )
        feedbackWarning(
            "max", 
            input$distrib %in% c("NO", "LN") && 
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
        req(input$distrib)
        req(input$min)
        req(input$max)
        req(input$mean)
        req(input$stdev)
        out$valid <- !((input$min > input$max) || 
                        ((input$distrib %in% c("GA")) &&
                            (input$mean < input$min || input$mean > input$max)))
        if(out$valid) {
            out$encoding <- str_c(
                local$name,
                str_c(
                    input$distrib,
                    str_c(
                        "[", input$min, ",", input$max, ",",
                        input$mean, ",", input$stdev, "]"
                    )
                ),
                sep = " "
            )
        }
    })
    
    # ## debugging
    # observe({
    #     logging("mean group prior def = ", out$encoding)
    # })
    
    ## output
    return(out)
}

#' MSS indiv group prior definition ui
#' @keywords internal
#' @author Ghislain Durif
indiv_group_prior_def_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("prior_def"))
    )
}

#' MSS group prior definition server
#' @keywords internal
#' @author Ghislain Durif
indiv_group_prior_def_server <- function(
    input, output, session, prior = reactive({NULL}), locus_mode = "M",
    show_input = TRUE
) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        name = NULL, desc = NULL, distrib = NULL,
        min = NULL, max = NULL, mean = NULL, 
        stdev = NULL, note = NULL,
        # numeric input setup
        input_min = 0, input_max = NA, input_step = 0.00001,
        # module input
        prior = NULL
    )
    
    # get input
    observe({
        local$prior <- prior()
    })
    
    # init output
    out <- reactiveValues(encoding = NULL, valid = TRUE)
    
    # # debugging
    # observe({
    #     pprint(local$prior)
    #     pprint(locus_mode)
    # })
    
    
    # parse input
    observe({
        req(locus_mode)
        if(isTruthy(local$prior) && 
           check_indiv_group_prior(local$prior, locus_mode)) {
            
            param_desc <- group_prior_param_desc(locus_mode)
            
            local$name <- get_group_prior_param(local$prior, locus_mode)
            local$desc <- param_desc$desc[param_desc$param == local$name]
            local$note <- param_desc$note[param_desc$param == local$name]
            local$distrib <- get_group_prior_distrib(local$prior)
            
            tmp_val <- get_group_prior_val(local$prior)
            
            local$min <- as.numeric(tmp_val[1])
            local$max <- as.numeric(tmp_val[2])
            local$mean <- tmp_val[3]
            local$stdev <- as.numeric(tmp_val[4])
        } else {
            local$name <- NULL
            local$desc <- NULL
            local$note <- NULL
            local$distrib <- NULL
            local$min <- NULL
            local$max <- NULL
            local$mean <- NULL
            local$stdev <- NULL
        }
    })
    
    # render input
    output$prior_def <- renderUI({
        
        req(show_input)
        
        req(local$name)
        req(local$desc)
        req(local$distrib)
        req(local$min)
        req(local$max)
        req(local$mean)
        req(local$stdev)
        
        input_min <- numericInput(
            ns("min"), label = NULL, value = local$min,
            min = local$input_min, max = local$input_max,
            step = local$input_step
        )
        
        input_max <- numericInput(
            ns("max"), label = NULL, value = local$max,
            min = local$input_min, max = local$input_max,
            step = local$input_step
        )
        
        input_mean <- shinyjs::disabled(textInput(
            ns("mean"), label = NULL, value = local$mean
        ))
        
        input_stdev <- numericInput(
            ns("stdev"), label = NULL, value = local$stdev,
            min = local$input_min, max = local$input_max,
            step = local$input_step
        )
        
        title_style <- 
            "text-align:right;margin-right:1em;vertical-align:middle;"
        
        param_title <- tags$h5(local$desc)
        if(isTruthy(local$note)) {
            param_title <- tags$h5(local$desc) %>% helper(
                type = "inline", 
                content = local$note
            )
        }
        
        tagList(
            param_title,
            fluidRow(
                column(
                    width = 4,
                    shinyjs::disabled(selectInput(
                        ns("distrib"),
                        label = NULL,
                        choices = list("Gamma" = "GA"),
                        selected = local$distrib
                    ))
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
            "min", (input$min > input$max), "min > max"
        )
        feedbackWarning(
            "max", (input$min > input$max), "min > max"
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
        req(input$distrib)
        req(input$min)
        req(input$max)
        req(input$mean)
        req(input$stdev)
        out$valid <- !(input$min > input$max)
        if(out$valid) {
            out$encoding <- str_c(
                local$name,
                str_c(
                    input$distrib,
                    str_c(
                        "[", input$min, ",", input$max, ",",
                        input$mean, ",", input$stdev, "]"
                    )
                ),
                sep = " "
            )
        }
    })
    
    # ## debugging
    # observe({
    #     logging("indiv group prior def = ", out$encoding)
    # })
    
    ## output
    return(out)
}

#' Sequence data mutation model setting module ui
#' @keywords internal
#' @author Ghislain Durif
seq_mutation_model_def_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("mut_model_def"))
    )
}

#' Sequence data mutation model setting module server
#' @keywords internal
#' @author Ghislain Durif
seq_mutation_model_def_server <- function(
    input, output, session, mut_model = reactive({NULL})
) {
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        mut_model_desc = mutation_model_desc(),
        mut_model = NULL,
        mut_model_name = NULL,
        invariant_perc = NULL,
        gamma_shape = NULL
    )
    
    # init output
    out <- reactiveValues(mut_model = NULL, mut_model_name = NULL)
    
    # get input
    observe({
        local$mut_model <- mut_model()
    })
    
    # # debugging
    # observe({
    #     pprint(local$mut_model)
    # })
    
    # parse input
    observeEvent(local$mut_model, {
        req(local$mut_model)
        parsed_mut_model <- parse_seq_mut_model(local$mut_model)
        
        if(parsed_mut_model$valid) {
            local$mut_model_name <- parsed_mut_model$mut_model
            local$invariant_perc <- parsed_mut_model$invariant_perc
            local$gamma_shape <- parsed_mut_model$gamma_shape
        } else {
            local$mut_model_name <- NULL
            local$invariant_perc <- NULL
            local$gamma_shape <- NULL
        }
    })
    
    # render ui
    output$mut_model_def <- renderUI({
        
        # pprint(local$mut_model_name)
        # pprint(local$invariant_perc)
        # pprint(local$gamma_shape)
        # pprint(local$mut_model_desc)
        
        req(local$mut_model_name)
        req(local$invariant_perc)
        req(local$gamma_shape)
        req(local$mut_model_desc)
        req(local$mut_model_desc$model)
        req(local$mut_model_desc$desc)
        
        possible_choice <- local$mut_model_desc$model
        names(possible_choice) <- local$mut_model_desc$desc
        
        tagList(
            fluidRow(
                column(
                    width = 4,
                    selectInput(
                        ns("mut_model"),
                        label = NULL,
                        choices = possible_choice,
                        selected = local$mut_model_name,
                        multiple = FALSE
                    )
                )
            ),
            fluidRow(
                column(
                    width = 4,
                    numericInput(
                        ns("invariant_perc"),
                        label = "Percentage of invariant sites",
                        value = local$invariant_perc,
                        min = 0, max = 100, step = 1
                    )
                )
            ),
            fluidRow(
                column(
                    width = 4,
                    numericInput(
                        ns("gamma_shape"),
                        label = "Shape of the gamma",
                        value = local$gamma_shape,
                        min = 0, max = NA, step = 0.001
                    )
                )
            )
        )
    })
    
    ## format output
    observe({
        req(input$mut_model)
        req(input$invariant_perc)
        req(input$gamma_shape)
        
        out$mut_model <- str_c(
            "MODEL",
            input$mut_model, input$invariant_perc, input$gamma_shape,
            sep = " "
        )
        out$mut_model_name <- input$mut_model
    })
    
    # ## debugging
    # observe({
    #     pprint(out$mut_model)
    #     pprint(out$mut_model_name)
    # })
    
    ## output
    return(out)
}
