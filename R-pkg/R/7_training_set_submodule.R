#' Training set sub-module ui
#' @keywords internal
#' @author Ghislain Durif
training_set_ui <- function(id) {
    ns <- NS(id)
    tagList(
        hist_model_panel_ui(ns("hist_model_panel")),
        hr(),
        prior_cond_set_ui(ns("prior_cond"))
    )
}

#' Training set sub-module server
#' @keywords internal
#' @author Ghislain Durif
#' @param project_dir project directory as a `reactive`.
#' @param project_name project name as a `reactive`.
#' @param scenario_list list of raw scenarii as a `reactive`.
training_set_server <- function(input, output, session,
                                project_dir = reactive({NULL}),
                                project_name = reactive({NULL}),
                                scenario_list = reactive({NULL})) {
    # init local
    local <- reactiveValues(
        cond_list = list(),
        param_list = list(),
        param_type_list = list(),
        project_dir = NULL,
        project_name = NULL,
        scenario_list = NULL
    )
    # get input
    observe({
        local$project_dir = project_dir()
        local$project_name = project_name()
        local$scenario_list = scenario_list()
    })
    # init output
    out <- reactiveValues()
    # historic model panel
    hist_model_def <- callModule(hist_model_panel_server, "hist_model_panel",
                                 project_dir = reactive(local$project_dir))
    # update local
    observe({
        local$scenario_list <- hist_model_def$scenario_list
        # extract input
        if(length(local$scenario_list) > 0) {
            # param list
            local$param_list <- Reduce("c", lapply(
                local$scenario_list,
                function(item) {
                    c(lapply(
                        item$param$Ne_param,
                        function(subitem) return(list(type = "N", 
                                                      name = subitem))
                     ),
                     lapply(
                         item$param$time_param,
                         function(subitem) return(list(type = "T", 
                                                       name = subitem))
                     ),
                     lapply(
                         item$param$rate_param,
                         function(subitem) return(list(type = "A", 
                                                       name = subitem))
                     ))
                }
            ))
            # condition
            local$cond_list <- lapply(
                local$scenario_list,
                function(item) return(item$cond)
            )
        }
    })
    # parameter prior and conditon setting
    prior_cond_set <- callModule(
                        prior_cond_set_server, "prior_cond",
                        cond_list = reactive(local$cond_list),
                        param_list = reactive(local$param_list))
                        # param_type_list = reactive(local$param_type_list))
    # output
    return(out)
}

#' Historical model input panel module ui
#' @keywords internal
#' @author Ghislain Durif
hist_model_panel_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(tags$span(icon("history"), "Define historical models")),
        fluidRow(
            column(
                width = 4,
                actionButton(
                    ns("add"), 
                    label = tags$span(icon("plus"), "Add")
                )
            ),
            column(
                width = 4,
                uiOutput(ns("scenario_nb"))
            )
        ),
        br(),
        tabsetPanel(id = ns("scenario_tabs"))
    )
}

#' Historical model input panel module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyjs onclick
#' @importFrom shinyWidgets actionBttn
#' @importFrom stringr str_c
hist_model_panel_server <- function(input, output, session,
                                    project_dir = reactive({NULL}), 
                                    raw_scenario_list = reactive({NULL})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        count = NULL,
        project_dir = NULL,
        raw_scenario_list = NULL,
        scenario_list = list(),
        scenario_nb = 0
    )
    # get input
    observe({
        local$project_dir <- project_dir()
        # FIXME raw_scenario_list
    })
    # init output
    out <- reactiveValues(scenario_list = NULL)
    
    # add scenario
    observeEvent(input$add, {
        # increment count
        local$count <- ifelse(is.null(local$count), 0, local$count) + 1
        local$scenario_nb <- ifelse(is.null(local$scenario_nb), 
                                    0, local$scenario_nb) + 1
        # id
        id <- local$count
        # add new tab
        appendTab(
            inputId = "scenario_tabs",
            tabPanel(
                title = closable_tab_title(id, 
                                           label = str_c("Scenario ", id),
                                           ns = ns),
                value = id,
                tags$br(),
                hist_model_ui(ns(str_c("model", id)))
            ),
            select = TRUE
        )
        # observe closing
        observe({
            shinyjs::onclick(id = str_c("close", id), {
                local$scenario_nb <<- ifelse(!is.null(local$scenario_nb), 
                                             local$scenario_nb - 1, 0)
                removeTab(inputId = "scenario_tabs", target = str_c(id))
                local$scenario_list[[ id ]] <<- NULL
            })
        })
        # server function
        local$scenario_list[[ id ]] <<- callModule(
            hist_model_server, str_c("model", id),
            project_dir = reactive(local$project_dir), 
            raw_scenario = reactive(local$raw_scenario)
        )
    })
    
    # update scenario number
    output$scenario_nb <- renderUI({
        helpText(
            str_c("Current number of scenarii = ", local$scenario_nb)
        )
    })
    
    # update output
    observe({
        out$scenario_list <- local$scenario_list
        # if(length(local$scenario_list) > 0) {
        #     print(local$scenario_list[[1]]$raw)
        #     print(local$scenario_list[[1]]$param)
        # }
    })
    
    ## output
    return(out)
}

#' Return closable panel title
#' @keywords internal
#' @author Ghislain Durif
closable_tab_title <- function(id, label, ns) {
    tags$span(
        label,
        HTML("&nbsp;"),
        tags$span(
            actionButton(
                ns(str_c("close", id)), 
                label = NULL,
                icon = icon("close")
            )
        )
    )
}

#' Parameter prior and condition setting module ui
#' @keywords internal
#' @author Ghislain Durif
prior_cond_set_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("chart-bar"), "Priors and conditions"),
        uiOutput(ns("param_prior_def")),
        hr(),
        h4("Condition setting"),
        uiOutput(ns("cond_def"))
    )
}

#' Parameter prior and condition setting module server
#' @keywords internal
#' @author Ghislain Durif
#' @param cond_list list of conditions on parameters, as a `reactive`.
#' @param param_list list of scenario parameters, as a `reactive`.
#' @importFrom stringr str_c
prior_cond_set_server <- function(input, output, session, 
                                  cond_list = reactive({list()}),
                                  param_list = reactive({list()})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        cond_list = NULL,
        param_list = NULL,
        prior_list = NULL
    )
    # get input
    observe({
        local$cond_list <- cond_list()
        local$param_list <- param_list()
    })
    # init output
    out <- reactiveValues(raw_prior_list = list())
    # render ui param prior setting
    observeEvent(local$param_list, {
        output$param_prior_def <- renderUI({
            tag_list <- lapply(
                local$param_list,
                function(item) return(prior_ui(ns(str_c("prior_", item$name))))
            )
            do.call(tagList, tag_list)
        })
    })
    # server side
    observe({
        local$prior_list <<- lapply(
            local$param_list,
            function(item) {
                req(item$name)
                req(item$type)
                callModule(prior_server, str_c("prior_", item$name),
                           param_name = reactive({item$name}),
                           param_type = reactive({item$type}))
            }
        )
    })
    observe({
        out$raw_prior_list <- unlist(lapply(
            local$prior_list,
            function(item) return(item$raw)
        ))
        print(out$raw_prior_list)
    })
    ## output
    return(out)
}

#' Prior choice module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyWidgets radioGroupButtons
prior_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(
                width = 1,
                textOutput(
                    ns("param_name")
                )
            ),
            column(
                width = 4,
                radioGroupButtons(
                    ns("prior_type"),
                    label = NULL,
                    choices = list("Uniform" = "UN", "Log-Unif." = "LU",
                                   "Normal" = "NO", "Log-Norm." = "LN"),
                    selected = "UN",
                    justified = TRUE
                )
            ),
            column(
                width = 7,
                fluidRow(
                    column(
                        width = 3,
                        splitLayout(
                            tags$p(
                                "Min.", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            numericInput(
                                ns("min"), label = NULL,
                                value = 10, step = 0.01
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    ),
                    column(
                        width = 3,
                        splitLayout(
                            tags$p(
                                "Max.", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            numericInput(
                                ns("max"), label = NULL,
                                value = 10000, step = 0.01
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    ),
                    column(
                        width = 3,
                        splitLayout(
                            tags$p(
                                "Mean", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            numericInput(
                                ns("mean"), label = NULL,
                                value = 0, step = 0.01
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    ),
                    column(
                        width = 3,
                        splitLayout(
                            tags$p(
                                "Std. dev.", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            numericInput(
                                ns("stdev"), label = NULL,
                                value = 0, step = 0.01
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    )
                )
            )
        )
    )
}

#' Prior choice module server
#' @keywords internal
#' @author Ghislain Durif
#' @param param_name parameter name as a `reactive`.
#' @param param_type parameter type as a `reactive`, `'N'` for population 
#' effective size, `'T'` for time, and `'A'` for admixture rate.
#' @importFrom shinyjs disable enable
#' @importFrom stringr str_c
prior_server <- function(input, output, session, 
                         param_name = reactive({NULL}),
                         param_type = reactive({NULL})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(param_name = NULL, param_type = NULL)
    # get input
    observe({
        local$param_name <- param_name()
        local$param_type <- param_type()
    })
    # init output
    out <- reactiveValues(raw = NULL, valid = TRUE)
    # update param name output
    output$param_name <- renderText({local$param_name})
    ## check for min/max
    observe({
        req(local$param_name)
        req(input$min)
        req(input$max)
        if(input$min >= input$max) {
            out$valid <- FALSE
            showNotification(
                id = ns("issue_min_max"),
                type = "warning",
                closeButton = TRUE,
                duration = 10,
                tags$p(
                    icon("warning"),
                    str_c(
                        "For parameter `", local$param_name, "`: ",
                        "min should be lower than max."
                    )
                )
            )
        } else {
            out$valid <- TRUE
        }
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
    ## check for normal and log-normal parameter setting
    observe({
        req(local$param_name)
        req(input$prior_type)
        req(is.numeric(input$min))
        req(is.numeric(input$max))
        req(is.numeric(input$mean))
        if(input$prior_type %in% c("NO", "LN")) {
            if(input$mean < input$min | input$mean > input$max) {
                out$valid <- FALSE
                showNotification(
                    id = ns("issue_norm_lognorm"),
                    type = "warning",
                    closeButton = TRUE,
                    duration = 10,
                    tags$p(
                        icon("warning"),
                        str_c(
                            "For parameter `", local$param_name, "`: ",
                            "mean should be between max and min values."
                        )
                    )
                )
            } else {
                out$valid <- TRUE
            }
        }
    })
    ## deal with admixture rate
    observeEvent(local$param_type, {
        req(local$param_type)
        if(local$param_type == "A") {
            updateNumericInput(session, "min", value = 0.001, min = 0, max = 1)
            updateNumericInput(session, "max", value = 0.999, min = 0, max = 1)
        } else {
            updateNumericInput(session, "min", value = 10, 
                               min = NULL, max = NULL)
            updateNumericInput(session, "max", value = 10000, 
                               min = NULL, max = NULL)
        }
    })
    # observe({
    #     logging("min = ", input$min)
    #     logging("max = ", input$max)
    #     logging("mean = ", input$mean)
    #     logging("stdev = ", input$stdev)
    # })
    ## encode output
    observe({
        req(local$param_name)
        req(local$param_type)
        req(input$prior_type)
        req(is.numeric(input$min))
        req(is.numeric(input$max))
        req(is.numeric(input$mean))
        req(is.numeric(input$stdev))
        out$raw <- str_c(local$param_name, " ",
                         local$param_type, " ",
                         input$prior_type, "[",
                         input$min, ",", input$max, ",",
                         input$mean, ",", input$stdev, "]")
        # logging("raw prior def = ", out$raw)
    })
    ## output
    return(out)
    
}
