#' Simulation page ui
#' @keywords internal
#' @author Ghislain Durif
#' @includeFrom shinyjs disabled
simu_page_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "Project settings",
                width = 12,
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                new_proj_set_ui(ns("proj_set")) %>% 
                    helper(type = "markdown", 
                           content = "simulation_project")
            )
        ),
        fluidRow(
            box(
                title = "Historical model",
                width = 12,
                status = "info", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                simu_hist_model_ui(ns("hist_model"))
            )
        ),
        fluidRow(
            box(
                title = "Genetic data",
                width = 12,
                status = "warning", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                genetic_loci_ui(ns("genetic_loci"))
            )
        )
        ,
        fluidRow(
            box(
                title = "Project action",
                width = 12,
                status = "danger", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE,
                "FILLME"
            )
        )
    )
}

#' Simulation page server
#' @keywords internal
#' @author Ghislain Durif
#' @param project_dir project directory as a `reactive`.
#' @param project_name project name as a `reactive`.
#' @param raw_scenario raw scenario as a `reactive`.
simu_page_server <- function(input, output, session,
                             project_dir = reactive({NULL}),
                             project_name = reactive({NULL}),
                             raw_scenario = reactive({NULL})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        project_dir = NULL,
        project_name = NULL,
        raw_scenario = NULL
    )
    # get input
    observe({
        local$project_dir = project_dir()
        local$project_name = project_name()
        local$raw_scenario = raw_scenario()
    })
    # init output
    out <- reactiveValues(
        setting = NULL,
        scenario = NULL
    )
    ## project setting
    setting <- callModule(new_proj_set_server, "proj_set")
    # update local
    observe({
        local$project_dir <- setting$project_dir
        local$project_name <- setting$project_name
    })
    # update output
    observe({
        out$setting <- setting
    })
    ## historical model
    scenario <- callModule(simu_hist_model_server, "hist_model",
                           project_dir = reactive(local$project_dir),
                           project_name = reactive(local$name),
                           raw_scenario = reactive(local$raw_scenario))
    # update local
    observe({
        local$raw_scenario <- scenario$raw_scenario
    })
    # update output
    observe({
        out$scenario <- scenario
    })
    ## genetic loci
    genetic_loci <- callModule(genetic_loci_server, "genetic_loci")
    # output
    return(out)
}

#' Simulation historical model ui
#' @keywords internal
#' @author Ghislain Durif
simu_hist_model_ui <- function(id) {
    ns <- NS(id)
    tagList(
        hist_model_ui(ns("hist_model")),
        simu_hist_model_param_ui(ns("param_setting"))
    )
}

#' Simulation historical model server
#' @keywords internal
#' @author Ghislain Durif
#' @param project_dir project directory as a `reactive`.
#' @param project_name project name as a `reactive`.
#' @param raw_scenario raw scenario as a `reactive`.
#' @importFrom shinyjs disable enable
simu_hist_model_server <- function(input, output, session,
                                   project_dir = reactive({NULL}),
                                   project_name = reactive({NULL}),
                                   raw_scenario = reactive({NULL})) {
    # init local
    local <- reactiveValues(
        project_dir = NULL,
        project_name = NULL,
        raw_scenario = NULL
    )
    # get input
    observe({
        local$project_dir = project_dir()
        local$project_name = project_name()
        local$raw_scenario = raw_scenario()
    })
    # init output
    out <- reactiveValues(
        raw_scenario = NULL,
        scenario_param = NULL
    )
    # define model
    scenario <- callModule(hist_model_server, "hist_model",
                           project_dir = reactive(local$project_dir), 
                           raw_scenario = reactive(local$raw_scenario))
    # update output
    observe({
        out$raw_scenario <- scenario$raw
        out$scenario_param <- scenario$param
    })
    # scenario parameter values
    observe({
        out$param_values <- callModule(simu_hist_model_param_server,
                                       "param_setting",
                                       scenario_cond = reactive(scenario$cond),
                                       scenario_param = reactive(scenario$param))
    })
    # output
    return(out)
}

#' Simulation historical model parameter setting ui
#' @keywords internal
#' @author Ghislain Durif
simu_hist_model_param_ui <- function(id) {
    ns <- NS(id)
    tagList(
        verticalLayout(
            tagList(
                hr(),
                h3("Parameter values"),
                uiOutput(ns("Ne_param_values")),
                uiOutput(ns("time_param_values")),
                uiOutput(ns("rate_param_values")),
                uiOutput(ns("conditions"))
            ),
            tagList(
                hr(),
                h3("Sample sizes"),
                uiOutput(ns("sample_param"))
            ),
            tagList(
                hr(),
                h3("Simulation number"),
                numericInput(
                    ns("nrep"),
                    label = "Number of repetitions",
                    value = 1
                )
            )
        )
    )
}

#' Simulation historical model parameter setting server
#' @keywords internal
#' @author Ghislain Durif
#' @param scenario_cond list of conditions on scenario parameters as a `reactive`.
#' @param scenario_param scenario parameter list as `reactive`.
#' @importFrom shinydashboard infoBox renderInfoBox
#' @importFrom shinyjs disable enable
simu_hist_model_param_server <- function(input, output, session,
                                         scenario_cond = reactive({NULL}),
                                         scenario_param = reactive({NULL})) {
    # local reactive values
    local <- reactiveValues(
        scenario_cond = NULL,
        scenario_param = NULL
    )
    # get input
    observe({
        local$scenario_cond = scenario_cond()
        local$scenario_param = scenario_param()
    })
    # init output reactive values
    out <- reactiveValues(param_values = list(),
                          sample_sizes = list(),
                          nrep = NULL)
    # parameters
    observe({
        req(local$scenario_param)
        # get Ne params
        param_list <- local$scenario_param$Ne_param
        # rendering
        output$Ne_param_values <- renderUI({
            render_model_param(session, param_list,
                               "Ne parameter(s)",
                               "Effective population size.",
                               value = 100, min = 0, max = NA, step = 1)
        })
        # return value
        out$param_values$Ne <- lapply(param_list, function(param) {
            return(input$param)
        })
    })

    observe({
        req(local$scenario_param)
        # get time params
        param_list <- local$scenario_param$time_param
        # rendering
        output$time_param_values <- renderUI({
            render_model_param(session, param_list, "Time parameter(s)",
                               "Time in <b>backward generations</b>.",
                               value = 100, min = 0, max = NA, step = 1)
        })
        # return value
        out$param_values$time <- lapply(param_list, function(param) {
            return(input$param)
        })
    })

    observe({
        req(local$scenario_param)
        # get rate params
        param_list <- local$scenario_param$rate_param
        # rendering
        output$rate_param_values <- renderUI({
            render_model_param(session, param_list, "Admixture rate parameter(s)",
                               "Rate between 0 and 1.",
                               value = 0.5, min = 0, max = 1, step = 0.001)
        })
        # return value
        out$param_values$rate_param <- lapply(param_list, function(param) {
            return(input$param)
        })
    })
    
    # info on conditions
    observeEvent(local$scenario_cond, {
        output$conditions <- renderUI({
            if(!is.null(local$scenario_cond) & length(local$scenario_cond) > 0) {
                helpText(
                    h4(icon("warning"), "Conditions"),
                    tags$p(
                        "You may want to consider the following conditions ",
                        "when setting the parameter values above."
                    ),
                    tags$p(
                        "This is an advisory warning to avoid gene genealogy ",
                        "incongruenties. You may prefer to ignore."
                    ),
                    do.call(
                        tags$ul,
                        lapply(local$scenario_cond, function(item) {
                            return(tags$li(item))
                        })
                    )
                )
            } else {
                tagList()
            }
        })
    })
    
    # sample
    observe({
        req(local$scenario_param)
        # parameter
        param <- local$scenario_param
        # number of sample event
        nsample <- sum(param$event_type == "sample")
        # number of population
        npop <- param$npop
        # populations concerned by sample
        sample_pop <- param$event_pop[param$event_type == "sample"]
        # time of sampling
        sample_time <- param$event_time[param$event_type == "sample"]
        # rendering
        output$sample_param <- renderUI({
            # numeric input for each sample
            numeric_input_list <- list()
            if(!is.null(npop) & nsample > 0) {
                numeric_input_list <- lapply(1:nsample, function(sample_id) {
                    render_sample_sizes(session, sample_id,
                                        unlist(sample_time[sample_id]),
                                        unlist(sample_pop[sample_id]))
                })
            }
            # Convert the list to a tagList
            tagList(
                do.call(flowLayout, numeric_input_list)
            )
        })
        # return value
        sampling_grid <- expand.grid(sample_id = 1:nsample,
                                     sex = c("f", "m"),
                                     KEEP.OUT.ATTRS = FALSE,
                                     stringsAsFactors = FALSE)
        out$sample_sizes <- lapply(
            split(sampling_grid,
                  seq(nrow(sampling_grid))),
            function(row) {
                do.call(function(sample_id, sex) {
                    return(paste0("sample", sample_id,
                                  "_pop", unlist(sample_time[sample_id]),
                                  "_time", unlist(sample_pop[sample_id]),
                                  "_", sex))
                }, row)
            }
        )
    })
    # number of repetitions
    observe({
        out$nrep <- input$nrep
    })
    # output
    return(out)
}

#' Render model parameter ui
#' @keywords internal
#' @author Ghislain Durif
#' @param type string, int or float
#' @param value numeric, default input value. Default is 100.
#' @param min numeric, input min value. Default is 0.
#' @param max numeric, input min value. Default is NA (no max value).
#' @param step numeric, step value for input.
render_model_param <- function(session, param_list, title, doc,
                               value = 100, min = 0, max = NA, step = 1) {
    ns <- session$ns

    if(length(param_list) > 0) {
        # numeric input for each parameters
        numeric_input_list <- lapply(param_list, function(param) {
            fluidRow(
                column(
                    width = 10,
                    numericInput(ns(param), label = param,
                                 value = value, min = min,
                                 max = max, step = step)
                )
            )
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        tagList(
            h5(title) %>%
                helper(
                    type = "inline",
                    title = title,
                    content = doc
                ),
            do.call(flowLayout, numeric_input_list)
        )
    } else {
        tagList()
    }
}

#' Render sampling parameter ui
#' @keywords internal
#' @author Ghislain Durif
render_sample_sizes <- function(session, sample_id, time_tag, pop_id) {
    # init output reactive values
    out <- reactiveValues()
    # module namespace
    ns <- session$ns
    # sampling tag
    tag_name <- paste0("sample", sample_id ,
                       "_pop", pop_id,
                       "_time", time_tag)
    # ui
    tagList(
        h5(tags$b("# ", sample_id), " from pop. ", tags$b(pop_id),
           " at time ", tags$b(time_tag)),
        verticalLayout(
            fluidRow(
                column(
                    width = 10,
                    numericInput(
                        ns(paste0(tag_name, "_f")),
                        label = "Female",
                        value = 25,
                        min = 0
                    ),
                ),
            ),
            fluidRow(
                column(
                    width = 10,
                    numericInput(
                        ns(paste0(tag_name, "_m")),
                        label = "Male",
                        value = 25,
                        min = 0
                    )
                )
            )
        )
    )
}
