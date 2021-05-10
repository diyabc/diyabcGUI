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
param_prior_panel_server <- function(input, output, session) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        prior_list = NULL, param_type = NULL,
        Ne_prior_list = NULL, time_prior_list = NULL, rate_prior_list = NULL,
        modified_prior_list = NULL
    )
    
    # get input and set default prior if no input
    observeEvent(env$ts$prior_list, {
        req(env$ts$prior_list)
        # existing pior list
        local$prior_list <- env$ts$prior_list
        # param type
        local$param_type <- str_extract(
            local$prior_list, str_c("(?<= )(N|T|A)(?= )")
        )
    })
    
    # # debugging
    # observe({
    #     pprint(local$prior_list)
    # })
    
    # update parameter type-specific prior list
    observeEvent(local$prior_list, {
        req(local$prior_list)
        if(any(local$param_type == "N")) {
            local$Ne_prior_list <- local$prior_list[local$param_type == "N"]
        } else {
            local$Ne_prior_list <- NULL
        }
        if(any(local$param_type == "T")) {
            local$time_prior_list <- local$prior_list[local$param_type == "T"]
        } else {
            local$time_prior_list <- NULL
        }
        if(any(local$param_type == "A")) {
            local$rate_prior_list <- local$prior_list[local$param_type == "A"]
        } else {
            local$rate_prior_list <- NULL
        }
    })
    
    # get parameter name
    output$param_prior_def <- renderUI({
        req(local$prior_list)
        tag_list1 <- NULL
        tag_list2 <- NULL
        tag_list3 <- NULL
        if(isTruthy(local$Ne_prior_list)) {
            tag_list1 <- tagList(
                h4(tags$b("Ne parameter(s) (effective size)")),
                prior_list_def_ui(ns("prior_def_Ne"))
            )
        }
        if(isTruthy(local$time_prior_list)) {
            tag_list2 <- tagList(
                h4(tags$b("Time parameter(s)")),
                prior_list_def_ui(ns("prior_def_time"))
            )
        }
        if(isTruthy(local$rate_prior_list)) {
            tag_list3 <- tagList(
                h4(tags$b("Admixture rate parameter(s)")),
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
        
        # pprint("original prior list")
        # pprint(env$ts$prior_list)
        # 
        # pprint("modified prior list")
        # pprint(local$modified_prior_list)
        
        req(local$modified_prior_list)
        req(env$ts$prior_list)
        
        # update env ?
        if((length(local$modified_prior_list) == 
            length(env$ts$prior_list)) &&
           any(sort(local$modified_prior_list) != sort(env$ts$prior_list))) {
            env$ts$prior_list <- local$modified_prior_list
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

#' Parameter prior setting module ui
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
        prior = NULL, type = NULL, name = NULL, distrib = NULL,
        min = NULL, max = NULL, mean = NULL, stdev = NULL,
        input_min = NA, input_max = NA, input_step = NA
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
            local$input_min <- 0
            local$input_max <- 1
            local$input_step <- 0.001
        } else {
            local$input_min <- 0
            local$input_max <- NA
            local$input_step <- 1
        }
    })
    
    # parse input
    observe({
        req(local$prior)
        req(check_header_prior(local$prior))
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
    })
    
    # render input
    output$prior_def <- renderUI({
        req(local$name)
        req(local$distrib)
        req(local$min)
        req(local$max)
        req(local$mean)
        req(local$stdev)
        req(local$input_min)
        req(local$input_step)
        req(is.na(local$input_max) || is.numeric(local$input_max))
        
        # pprint(local$name)
        # pprint(local$distrib)
        # pprint(local$min)
        # pprint(local$max)
        # pprint(local$mean)
        # pprint(local$stdev)
        # pprint(local$input_min)
        # pprint(local$input_max)
        # pprint(local$input_step)
        
        tagList(
            tags$h5(local$name),
            fluidRow(
                column(
                    width = 6,
                    radioGroupButtons(
                        ns("prior_type"),
                        label = NULL,
                        choices = list("Uniform" = "UN", "Log-Unif." = "LU",
                                       "Normal" = "NO", "Log-Norm." = "LN"),
                        selected = local$distrib,
                        justified = TRUE
                    ),
                ),
                column(
                    width = 6,
                    fluidRow(
                        column(
                            width = 6,
                            splitLayout(
                                tags$h5(
                                    "Min.",
                                    style="text-align:right;margin-right:1em;vertical-align:middle;"
                                ),
                                numericInput(
                                    ns("min"), label = NULL,
                                    value = local$min,
                                    min = local$input_min,
                                    max = local$input_max,
                                    step = local$input_step
                                ),
                                cellWidths = c("40%", "60%")
                            )
                        ),
                        column(
                            width = 6,
                            splitLayout(
                                tags$h5(
                                    "Max.",
                                    style="text-align:right;margin-right:1em;vertical-align:middle;"
                                ),
                                numericInput(
                                    ns("max"), label = NULL,
                                    value = local$max,
                                    min = local$input_min,
                                    max = local$input_max,
                                    step = local$input_step
                                ),
                                cellWidths = c("40%", "60%")
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            width = 6,
                            splitLayout(
                                tags$h5(
                                    "Mean",
                                    style="text-align:right;margin-right:1em;vertical-align:middle;"
                                ),
                                numericInput(
                                    ns("mean"), label = NULL,
                                    value = local$mean,
                                    min = local$input_min,
                                    max = local$input_max,
                                    step = local$input_step
                                ),
                                cellWidths = c("40%", "60%")
                            )
                        ),
                        column(
                            width = 6,
                            splitLayout(
                                tags$h5(
                                    "Std. dev.",
                                    style="text-align:right;margin-right:1em;vertical-align:middle;"
                                ),
                                numericInput(
                                    ns("stdev"), label = NULL,
                                    value = local$stdev,
                                    min = local$input_min,
                                    max = NA,
                                    step = local$input_step
                                ),
                                cellWidths = c("40%", "60%")
                            )
                        )
                    )
                )
            )
        )
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
            "min", 
            (input$min >= input$max),
            str_c("Min should be lower than max.")
        )
        feedbackWarning(
            "max", 
            (input$min >= input$max),
            str_c("Min should be lower than max.")
        )
    })
    
    ## check for normal and log-normal parameter setting
    observe({
        req(local$name)
        req(input$prior_type)
        req(input$min)
        req(input$max)
        req(input$mean)
        feedbackWarning(
            "mean", 
            input$prior_type %in% c("NO", "LN") && 
                (input$mean < input$min || input$mean > input$max),
            "Mean should be between max and min values."
        )
        feedbackWarning(
            "min", 
            input$prior_type %in% c("NO", "LN") && 
                (input$mean < input$min || input$mean > input$max),
            "Mean should be between max and min values."
        )
        feedbackWarning(
            "max", 
            input$prior_type %in% c("NO", "LN") && 
                (input$mean < input$min || input$mean > input$max),
            "Mean should be between max and min values."
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
        req(input$prior_type)
        req(input$min)
        req(input$max)
        req(input$mean)
        req(input$stdev)
        out$valid <- !((input$min >= input$max) || 
            ((input$prior_type %in% c("NO", "LN")) &&
                 (input$mean < input$min || input$mean > input$max)))
        if(out$valid) {
            out$encoding <- str_c(
                local$name, " ",
                local$type, " ",
                input$prior_type, "[",
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
