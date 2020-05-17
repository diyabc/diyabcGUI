#' Simulation page ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyjs disabled
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
                genetic_loci_ui(ns("genetic_setting"))
            )
        )
        ,
        fluidRow(
            box(
                title = "Project action",
                width = 12,
                status = "danger", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE,
                simu_proj_action_ui(ns("proj_action"))
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
        genetic_setting = NULL,
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
        # print("---- scenario = ")
        # print(scenario)
        out$scenario <- scenario
    })
    ## genetic loci
    genetic_setting <- callModule(genetic_loci_server, "genetic_setting")
    # # update output
    # observe({
    #     # print("---- genetic_setting = ")
    #     # print(genetic_setting)
    #     # print(genetic_setting$locus_description)
    #     # out$genetic_setting <- genetic_setting
    # })
    # ## debugging
    # observe({
    #     # print(scenario)
    #     print("sample sizes = ")
    #     print(scenario$param_setting$sample_sizes)
    #     # print(genetic_setting)
    # })
    ## simulation project action
    callModule(simu_proj_action_server, "proj_action",
               project_dir = reactive(setting$project_dir),
               project_name = reactive(setting$project_name),
               validation = reactive(setting$validation),
               raw_param = reactive(scenario$param_setting$param_values),
               raw_scenario = reactive(scenario$raw_scenario),
               locus_description = reactive(genetic_setting$locus_description),
               sample_sizes = reactive(scenario$param_setting$sample_sizes),
               n_rep = reactive(scenario$param_setting$nrep),
               sex_ratio = reactive(genetic_setting$sex_ratio),
               seq_mode = reactive(genetic_setting$seq_mode),
               locus_type = reactive(genetic_setting$locus_type))
    
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
    param_setting <- callModule(simu_hist_model_param_server,
                               "param_setting",
                               scenario_cond = reactive(scenario$cond),
                               scenario_param = reactive(scenario$param))
    # update output
    observe({
        out$param_setting <- param_setting
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
        scenario_param = NULL,
        param_values = list()
    )
    # get input
    observe({
        local$scenario_cond = scenario_cond()
        local$scenario_param = scenario_param()
        # print("check input")
        # print(local$scenario_cond)
        # print(local$scenario_param)
    })
    # init output reactive values
    out <- reactiveValues(param_values = list(),
                          sample_sizes = list(),
                          nrep = NULL)
    # Ne parameters
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
        local$param_values$Ne <- lapply(param_list, function(param) {
            return(list(name = param, type = "N", value = input[[ param ]]))
        })
    })
    # time parameters
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
        local$param_values$time <- lapply(param_list, function(param) {
            return(list(name = param, type = "T", value = input[[ param ]]))
        })
    })
    # rate parameters
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
        local$param_values$rate_param <- lapply(param_list, function(param) {
            return(list(name = param, type = "A", value = input[[ param ]]))
        })
    })
    
    # update output
    observe({
        param_list <- c(local$param_values$Ne, 
                        local$param_values$time, 
                        local$param_values$rate)
        out$param_values <- unlist(lapply(param_list, function(item) {
            return(
                str_c(item$name, item$type, item$value, sep = " ")
            )
        }))
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
                        "incongruenties. You may prefer to ignore it."
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
        # logging("nsample = ", nsample)
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
        if(nsample > 0) {
            out$sample_sizes <- Reduce("rbind", lapply(
                1:nsample,
                function(sample_id) {
                    tag_name <- str_c("sample", sample_id)
                    return(c(input[[str_c(tag_name, "_f")]],
                             input[[str_c(tag_name, "_m")]]))
                }
            ))
        }
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
    tag_name <- paste0("sample", sample_id)
                       # "_pop", pop_id,
                       # "_time", time_tag)
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

#' Simulation project action module ui
#' @keywords internal
#' @author Ghislain Durif
simu_proj_action_ui <- function(id) {
    ns <- NS(id)
    tagList(
        actionGroupButtons(
            inputIds = c(ns("save"),
                         ns("duplicate")),
            labels = list(
                tags$span(icon("save"), "Save"),
                tags$span(icon("copy"), "Duplicate")
            ),
            fullwidth = TRUE
        ),
        hr(),
        actionBttn(
            inputId = ns("simulate"),
            label = "Simulate",
            style = "fill",
            block = TRUE
        )
    )
}

#' Simulation project action module server
#' @keywords internal
#' @author Ghislain Durif
#' @param project_dir project directory as a `reactive`.
#' @param project_name project name as a `reactive`.
#' @param validation boolean indicating if the project setting are validated, 
#' as a `reactive`.
#' @param raw_param scenario parameter raw setting as a `reactive`. 
#' @param raw_scenario raw scenario as a `reactive`.
#' @param locus_description genetic locus setting as a `reactive`.
simu_proj_action_server <- function(input, output, session,
                                    project_dir = reactive({NULL}),
                                    project_name = reactive({NULL}),
                                    validation = reactive({TRUE}),
                                    raw_param = reactive({NULL}),
                                    raw_scenario = reactive({NULL}),
                                    locus_description = reactive({NULL}),
                                    sample_sizes = reactive({NULL}),
                                    n_rep = reactive({NULL}),
                                    sex_ratio = reactive({NULL}),
                                    seq_mode = reactive({NULL}),
                                    locus_type = reactive({NULL})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        saved = FALSE,
        project_dir = NULL,
        project_name = NULL,
        validation = NULL,
        raw_param = NULL,
        raw_scenario = NULL,
        locus_description = NULL,
        sample_sizes = NULL,
        sex_ratio = NULL,
        n_rep = NULL,
        seq_mode = NULL,
        locus_type = NULL
    )
    # get input
    observe({
        local$project_dir <- project_dir()
        local$project_name <- project_name()
        local$validation <- validation()
        local$raw_param <- raw_param()
        local$raw_scenario <- raw_scenario()
        local$locus_description <- locus_description()
        local$sample_sizes <- sample_sizes()
        local$sex_ratio <- sex_ratio()
        local$n_rep <- n_rep()
        local$seq_mode <- seq_mode()
        local$locus_type <- locus_type()
    })
    # init output
    out <- reactiveValues(
        duplicate = NULL,
        save = NULL
    )
    
    # disable saved if not validated
    observeEvent(local$validation, {
        req(!is.null(local$validation))
        if(local$validation) {
            shinyjs::enable("save")
        } else {
            shinyjs::disable("save")
            # # directory not existing
            # showNotification(
            #     id = ns("project_dir_issue"),
            #     duration = 5,
            #     closeButton = TRUE,
            #     type = "warning",
            #     tagList(
            #         tags$p(
            #             icon("warning"),
            #             paste0("Directory does not exists. ", 
            #                    "Did you 'validate' the project?")
            #         )
            #     )
            # )
        }
    })
    
    # un-saved if modification to input
    observeEvent({
        local$project_dir
        local$project_name
        local$raw_param
        local$raw_scenario
        local$locus_description
        local$n_rep
        local$sex_ratio
    }, {
        local$saved <- FALSE
    })
    
    # deactivate simulate if not saved
    observeEvent(local$saved, {
        req(!is.null(local$saved))
        if(local$saved) {
            shinyjs::enable("simulate")
        } else {
            shinyjs::disable("simulate")
        }
    })
    
    # save project = write header file
    observeEvent(input$save, {
        # FIXME write header file
        local$saved <- TRUE
    })
    
    # FIXME project duplication
    observeEvent(input$duplicate, {
        out$duplicate <- ifelse(is.null(out$duplicate), 0, out$duplicate) + 1
    })
    
    ## prepare for simulation
    # # debugging
    # observe({
    #     req(local$raw_param)
    #     print(local$raw_param)
    # })
    
    ## write header file
    observeEvent(input$save, {
        req(!is.null(local$project_dir))
        req(!is.null(local$project_name))
        req(!is.null(local$raw_param))
        req(!is.null(local$raw_scenario))
        req(!is.null(local$locus_description))
        req(!is.null(local$n_rep))
        req(!is.null(local$sex_ratio))
        req(!is.null(local$seq_mode))
        req(!is.null(local$locus_type))
        
        # print(local$samples_sizes)
        # 
        # print(local$locus_type)
        
        write_headersim(local$project_name, local$project_dir, 
                        local$seq_mode, local$locus_type,
                        local$raw_scenario, local$raw_param, 
                        local$locus_description, 
                        local$sample_sizes,
                        local$n_rep, local$sex_ratio)
        
        showNotification(
            id = ns("headerfile_ok"),
            duration = 5,
            closeButton = TRUE,
            type = "message",
            tagList(
                tags$p(
                    icon("check"),
                    paste0("Project is ready to run simulations.")
                )
            )
        )
    })
    
    ## FIXME run simulation
    observeEvent(input$simulate, {
        logging("Running simulation")
        check <- tryCatch(
            run_diyabc(project_dir = local$project_dir, n_core = 1),
            error = function(e) return(e)
        )
        if(!is.null(check) & !is.null(check$message)) {
            showNotification(
                id = ns("run_not_ok"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"),
                        str_c(check$message)
                    )
                )
            )
        } else {
            showNotification(
                id = ns("run_ok"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("check"),
                        "Simulations are done."
                    )
                )
            )
        }
    })
}
