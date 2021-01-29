#' Synthetic data generation module ui
#' @keywords internal
#' @author Ghislain Durif
datagen_page_ui <- function(id) {
    ns <- NS(id)
    tagList(
        tags$style(HTML(".box-header{text-align: center;}")),
        fluidRow(
            box(
                title = tags$b("Project settings"),
                width = 12,
                status = "primary", solidHeader = FALSE,
                collapsible = FALSE,
                datagen_proj_set_ui(ns("proj_set"))
            ),
            box(
                title = "Historical model",
                width = 12,
                status = "info", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE,
                datagen_hist_model_ui(ns("hist_model"))
            ),
            box(
                title = "Genetic data",
                width = 12,
                status = "warning", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE,
                "WriteME"
                # genetic_loci_ui(ns("genetic_setting"))
            ),
            box(
                title = "Data file generation",
                width = 12,
                status = "warning", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE,
                "WriteME"
                # datafile_gen_ui(ns("datafile_gen"))
            ),
            box(
                title = tags$b("Project administration"),
                width = 12,
                status = "danger", solidHeader = FALSE,
                collapsible = FALSE, collapsed = FALSE,
                proj_admin_ui(ns("proj_admin"))
            )
        )
    )
}

#' Synthetic data generation module server
#' @keywords internal
#' @author Ghislain Durif
datagen_page_server <- function(input, output, session) {
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        raw_scenario = NULL
    )
    
    # init output
    out <- reactiveValues(
        reset = NULL
    )
    
    ## project setting
    proj_set <- callModule(datagen_proj_set_server, "proj_set")
    
    ## historical model
    hist_model <- callModule(
        datagen_hist_model_server, "hist_model",
        proj_dir = reactive(proj_set$proj_dir),
        raw_scenario = reactive(local$raw_scenario)
    )
    
    # update local
    observe({
        local$raw_scenario <- hist_model$raw_scenario
    })
    
    ## action
    proj_admin <- callModule(
        proj_admin_server, "proj_admin",
        proj_dir = reactive(proj_set$proj_dir),
        proj_name = reactive(proj_set$proj_name)
    )
    
    ## reset
    observeEvent(proj_admin$reset, {
        req(proj_admin$reset)
        out$reset <- proj_admin$reset
        session$reload()
    })
    
    # output
    return(out)
}

#' Data generation project setup module ui
#' @keywords internal
#' @author Ghislain Durif
datagen_proj_set_ui <- function(id) {
    ns <- NS(id)
    tagList(
        proj_name_ui(ns("proj_name_setup")),
        hr(),
        h3("Data type"),
        data_type_ui(ns("data_type"))
    )
}

#' Data generation project setup module server
#' @keywords internal
#' @author Ghislain Durif
datagen_proj_set_server <- function(input, output, session) {
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(valid_proj_name = FALSE)
    
    # init output
    out <- reactiveValues(
        proj_dir = mk_proj_dir("diyabc_datagen"),
        proj_name = NULL,
        locus_type = NULL,
        seq_mode = NULL
    )
    
    # clean on exit
    session$onSessionEnded(function() {
        isolate(tryCatch(fs::dir_delete(out$proj_dir)))
    })
    
    # debugging
    observe({
        logging("project directory:", out$proj_dir)
    })
    
    ## project name
    proj_name_setup <- callModule(proj_name_server, "proj_name_setup")
    
    observeEvent(proj_name_setup$proj_name, {
        req(proj_name_setup$proj_name)
        out$proj_name <- proj_name_setup$proj_name
    })
    
    observeEvent(proj_name_setup$valid_proj_name, {
        req(!is.null(proj_name_setup$valid_proj_name))
        local$valid_proj_name <- proj_name_setup$valid_proj_name
    })
    
    ## data type
    data_type <- callModule(data_type_server, "data_type")
    observe({
        req(data_type$locus_type)
        req(data_type$seq_mode)
        out$locus_type <- data_type$locus_type
        out$seq_mode <- data_type$seq_mode
    })
    
    # output
    return(out)
}

#' Data generation historical model setup module ui
#' @keywords internal
#' @author Ghislain Durif
datagen_hist_model_ui <- function(id) {
    ns <- NS(id)
    tagList(
        hist_model_ui(ns("hist_model")),
        hr(),
        datagen_model_param_ui(ns("param_setting"))
    )
}

#' Data generation historical model setup module
#' @keywords internal
#' @author Ghislain Durif
#' @param proj_dir project directory as a `reactive`.
#' @param raw_scenario raw scenario as a `reactive`.
#' @importFrom shinyjs disable enable
datagen_hist_model_server <- function(
    input, output, session,
    proj_dir = reactive({NULL}),
    raw_scenario = reactive({NULL})
) {
    # init local
    local <- reactiveValues(
        proj_dir = NULL,
        raw_scenario = NULL
    )
    
    # get input
    observe({
        local$proj_dir = proj_dir()
        local$raw_scenario = raw_scenario()
    })
    
    # init output
    out <- reactiveValues(
        raw_scenario = NULL,
        scenario_param = NULL
    )
    # define model
    hist_model <- callModule(
        hist_model_server, "hist_model",
        project_dir = reactive(local$proj_dir), 
        raw_scenario = reactive(local$raw_scenario)
    )
    
    # update output
    observe({
        out$raw_scenario <- hist_model$raw
        out$scenario_param <- hist_model$param
    })
    
    # scenario parameter values
    param_setting <- callModule(
        datagen_model_param_server,
        "param_setting",
        scenario_cond = reactive(hist_model$cond),
        scenario_param = reactive(hist_model$param)
    )
    
    # update output
    observe({
        out$param_setting <- reactiveValuesToList(param_setting)
        # print(out$param_setting)
    })
    # output
    return(out)
}

#' Data generation model parameter setup module ui
#' @keywords internal
#' @author Ghislain Durif
datagen_model_param_ui <- function(id) {
    ns <- NS(id)
    tagList(
        datagen_hist_model_param_ui(ns("hist_param")),
        hr(),
        datagen_sampling_param_ui(ns("samp_param"))
    )
}

#' Data generation model parameter setup module server
#' @keywords internal
#' @author Ghislain Durif
#' @param scenario_cond list of conditions on scenario parameters as a `reactive`.
#' @param scenario_param scenario parameter list as `reactive`.
datagen_model_param_server <- function(
    input, output, session,
    scenario_cond = reactive({NULL}),
    scenario_param = reactive({NULL})
) {
    # local reactive values
    local <- reactiveValues(
        scenario_cond = NULL,
        scenario_param = NULL,
        n_pop = NULL,
        n_sample = NULL
    )
    
    # get input
    observe({
        local$scenario_cond = scenario_cond()
        local$scenario_param = scenario_param()
        # pprint("check input")
        # pprint(local$scenario_cond)
        # pprint(local$scenario_param)
        
        if(isTruthy(local$scenario_param)) {
            if(isTruthy(local$scenario_param$npop)) {
                local$n_pop <- local$scenario_param$npop
            }
            if(isTruthy(local$scenario_param$event_type)) {
                local$n_sample <- sum(local$scenario_param$event_type ==
                                          "sample")
            }
        } 
    })
    
    # init output reactive values
    out <- reactiveValues(param_values = list(),
                          sample_sizes = list(),
                          n_rep = NULL)
    
    # setup parameter input
    hist_param <- callModule(
        datagen_hist_model_param_server, "hist_param",
        scenario_cond = reactive(local$scenario_cond),
        scenario_param = reactive(local$scenario_param)
    )
    
    # update output
    observe({
        req(hist_param$param_values)
        out$param_values <- hist_param$param_values
    })
    
    # sample
    samp_param <- callModule(
        datagen_sampling_param_server, "samp_param",
        n_pop = reactive(local$n_pop),
        n_sample = reactive(local$n_sample)
    )
    
    # update output
    observe({
        req(samp_param$sample_sizes)
        out$sample_sizes <- samp_param$sample_sizes
    })
    observe({
        req(samp_param$n_rep)
        out$n_rep <- samp_param$n_rep
    })
    # output
    return(out)
}


#' Simulation page ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyjs disabled
simu_page_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "Soon available",
                width = 12,
                collapsible = FALSE,
                tagList(
                    helpText(
                        "The 'synthetic data file generation' module",
                        "is not available at the moment.",
                        "We are working to make it available as soon as possible."
                    )
                )
            )
            # box(
            #     title = "Project settings",
            #     width = 12,
            #     status = "primary", solidHeader = TRUE,
            #     collapsible = TRUE,
            #     new_proj_set_ui(ns("proj_set")) %>% 
            #         helper(type = "markdown", 
            #                content = "simulation_project")
            # ),
            # box(
            #     title = "Historical model",
            #     width = 12,
            #     status = "info", solidHeader = TRUE,
            #     collapsible = TRUE, collapsed = TRUE,
            #     simu_hist_model_ui(ns("hist_model"))
            # ),
            # box(
            #     title = "Genetic data",
            #     width = 12,
            #     status = "warning", solidHeader = TRUE,
            #     collapsible = TRUE, collapsed = TRUE,
            #     genetic_loci_ui(ns("genetic_setting"))
            # ),
            # box(
            #     title = "Project action",
            #     width = 12,
            #     status = "danger", solidHeader = TRUE,
            #     collapsible = TRUE, collapsed = FALSE,
            #     simu_proj_action_ui(ns("proj_action"))
            # )
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
    # # namespace
    # ns <- session$ns
    # # init local
    # local <- reactiveValues(
    #     project_dir = NULL,
    #     project_name = NULL,
    #     raw_scenario = NULL
    # )
    # # get input
    # observe({
    #     local$project_dir = project_dir()
    #     local$project_name = project_name()
    #     local$raw_scenario = raw_scenario()
    # })
    # # init output
    # out <- reactiveValues(
    #     genetic_setting = NULL,
    #     setting = NULL,
    #     scenario = NULL
    # )
    # ## project setting
    # setting <- callModule(new_proj_set_server, "proj_set")
    # # update local
    # observe({
    #     local$project_dir <- setting$project_dir
    #     local$project_name <- setting$project_name
    # })
    # # update output
    # observe({
    #     out$setting <- setting
    # })
    # ## historical model
    # scenario <- callModule(simu_hist_model_server, "hist_model",
    #                        project_dir = reactive(local$project_dir),
    #                        project_name = reactive(local$name),
    #                        raw_scenario = reactive(local$raw_scenario))
    # # update local
    # observe({
    #     local$raw_scenario <- scenario$raw_scenario
    # })
    # # update output
    # observe({
    #     # pprint("---- scenario = ")
    #     # pprint(scenario)
    #     out$scenario <- scenario
    # })
    # ## genetic loci
    # genetic_setting <- callModule(genetic_loci_server, "genetic_setting")
    # # # update output
    # # observe({
    # #     # pprint("---- genetic_setting = ")
    # #     # pprint(genetic_setting)
    # #     # pprint(genetic_setting$locus_description)
    # #     # out$genetic_setting <- genetic_setting
    # # })
    # # ## debugging
    # # observe({
    # #     # pprint(scenario)
    # #     pprint("sample sizes = ")
    # #     pprint(scenario$param_setting$sample_sizes)
    # #     # pprint(genetic_setting)
    # # })
    # ## simulation project action
    # callModule(simu_proj_action_server, "proj_action",
    #            project_dir = reactive(setting$project_dir),
    #            project_name = reactive(setting$project_name),
    #            validation = reactive(setting$validation),
    #            raw_param = reactive(scenario$param_setting$param_values),
    #            raw_scenario = reactive(scenario$raw_scenario),
    #            locus_description = reactive(genetic_setting$locus_description),
    #            sample_sizes = reactive(scenario$param_setting$sample_sizes),
    #            n_rep = reactive(scenario$param_setting$n_rep),
    #            sex_ratio = reactive(genetic_setting$sex_ratio),
    #            seq_mode = reactive(genetic_setting$seq_mode),
    #            locus_type = reactive(genetic_setting$locus_type))
    # 
    # # output
    # return(out)
}

#' Data generation historical model parameter setup module ui
#' @keywords internal
#' @author Ghislain Durif
datagen_hist_model_param_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3("Parameter values"),
        uiOutput(ns("Ne_param_values")),
        uiOutput(ns("time_param_values")),
        uiOutput(ns("rate_param_values")),
        uiOutput(ns("conditions"))
    )
}

#' Data generation historical model parameter setup module server
#' @keywords internal
#' @author Ghislain Durif
#' @param scenario_cond list of conditions on scenario parameters as a `reactive`.
#' @param scenario_param scenario parameter list as `reactive`.
datagen_hist_model_param_server <- function(
    input, output, session,
    scenario_cond = reactive({NULL}),
    scenario_param = reactive({NULL})
) {
    # local reactive values
    local <- reactiveValues(
        scenario_cond = NULL,
        scenario_param = NULL,
        param_list = list(),
        param_df = data.frame(
            name = character(),
            type = character(),
            value = numeric(),
            stringsAsFactors=FALSE
        )
    )
    
    # get input
    observe({
        local$scenario_cond = scenario_cond()
        local$scenario_param = scenario_param()
        # pprint("check input")
        # pprint(local$scenario_cond)
        # pprint(local$scenario_param)
        
        # parse new parameter (if any)
        tmp_param_df = data.frame(
            name = character(),
            type = character(),
            value = numeric(),
            stringsAsFactors=FALSE
        )
        
        # parse scenario Ne parameters
        if(isTruthy(local$scenario_param$Ne_param)) {
            tmp_param_df <- bind_rows(
                tmp_param_df,
                data.frame(
                    name = local$scenario_param$Ne_param,
                    type = rep("N", length(local$scenario_param$Ne_param)),
                    value = rep(100, length(local$scenario_param$Ne_param)),
                    stringsAsFactors=FALSE
                )
            )
        }
        
        # parse scenario time parameters
        if(isTruthy(local$scenario_param$time_param)) {
            tmp_param_df <- bind_rows(
                tmp_param_df,
                data.frame(
                    name = local$scenario_param$time_param,
                    type = rep("T", length(local$scenario_param$time_param)),
                    value = rep(100, length(local$scenario_param$time_param)),
                    stringsAsFactors=FALSE
                )
            )
        }
        
        # parse scenario admixture rate parameters
        if(isTruthy(local$scenario_param$rate_param)) {
            tmp_param_df <- bind_rows(
                tmp_param_df,
                data.frame(
                    name = local$scenario_param$rate_param,
                    type = rep("A", length(local$scenario_param$rate_param)),
                    value = rep(0.5, length(local$scenario_param$rate_param)),
                    stringsAsFactors=FALSE
                )
            )
        }
        
        # remove old parameter not existing anymore
        keep_old_ind <- local$param_df$name %in% tmp_param_df$name
        
        # add unexisting parameters
        add_new_ind <- !(tmp_param_df$name %in% local$param_df$name)
        
        # merge
        local$param_df <- bind_rows(
            local$param_df[keep_old_ind,],
            tmp_param_df[add_new_ind,]
        )
    })
    
    # init output reactive values
    out <- reactiveValues(param_values = NULL,
                          sample_sizes = NULL,
                          n_rep = NULL)
    
    # # debugging
    # observe({
    #     pprint("parameter values")
    #     pprint(local$param_df)
    # })
    
    # setup parameter input
    observe({
        # rendering Ne param
        req(nrow(local$param_df) > 0)
        Ne_param_ind <- local$param_df$type == "N"
        req(sum(Ne_param_ind) > 0)
        tmp_param_df <- local$param_df[Ne_param_ind,]
        output$Ne_param_values <- renderUI({
            render_model_param(
                session, tmp_param_df,
                "Ne parameter(s)",
                "Effective population size.",
                min = 0, max = NA, step = 1
            )
        })
    })
    
    observe({
        # rendering time param
        req(nrow(local$param_df) > 0)
        time_param_ind <- local$param_df$type == "T"
        req(sum(time_param_ind) > 0)
        tmp_param_df <- local$param_df[time_param_ind,]
        output$time_param_values <- renderUI({
            render_model_param(
                session, tmp_param_df,
                "Time parameter(s)",
                "Time in <b>backward generations</b>.",
                min = 0, max = NA, step = 1
            )
        })
    })
    
    observe({
        # rendering rate param
        req(nrow(local$param_df) > 0)
        rate_param_ind <- local$param_df$type == "A"
        req(sum(rate_param_ind) > 0)
        tmp_param_df <- local$param_df[rate_param_ind,]
        output$time_param_values <- renderUI({
            render_model_param(
                session, tmp_param_df,
                "Admixture rate parameter(s)",
                "Rate between 0 and 1.",
                min = 0, max = 1, step = 0.001
            )
        })
    })
    
    # get input value
    observe({
        req(nrow(local$param_df) > 0)
        for(ind in seq(nrow(local$param_df))) {
            item_name <- local$param_df$name[ind]
            if(isTruthy(input[[ item_name ]])) {
                current_value <- input[[ item_name ]]
                if(local$param_df$value[ind] != current_value) {
                    local$param_df$value[ind] <- current_value
                }
            }
        }
    })
    
    # update output
    observe({
        req(nrow(local$param_df) > 0)
        out$param_values <- unlist(lapply(
            split(local$param_df, seq(nrow(local$param_df))),
            function(item) {
                return(
                    str_c(item$name, item$type, item$value, sep = " ")
                )
            }
        ))
    })
    
    # # debugging
    # observe({
    #     pprint("parameter setup")
    #     pprint(out$param_values)
    # })
    
    # info on conditions
    output$conditions <- renderUI({
        txt <- tagList()
        # pprint("scenario cond")
        # pprint(local$scenario_cond)
        if(isTruthy(local$scenario_cond) && length(local$scenario_cond) > 0) {
            txt <- helpText(
                h4(icon("warning"), "Conditions"),
                tags$p(
                    "You may want to consider the following conditions ",
                    "when setting the parameter values above:"
                ),
                tags$p(
                    do.call(
                        tags$ul,
                        lapply(local$scenario_cond, function(item) {
                            return(tags$li(item))
                        })
                    )
                ),
                tags$p(
                    "This is an advisory warning to avoid gene genealogy ",
                    "inconsistencies. You may prefer to ignore it."
                )
            )
        }
        txt
    })
    # output
    return(out)
}

#' Render model parameter ui
#' @keywords internal
#' @author Ghislain Durif
#' @param param_df data.frame with attributes `name`, `type` and `value`.
#' @param min numeric, input min value. Default is 0.
#' @param max numeric, input min value. Default is NA (no max value).
#' @param step numeric, step value for input.
render_model_param <- function(session, param_df, title, doc,
                               min = 0, max = NA, step = 1) {
    ns <- session$ns
    out <- tagList()

    if(nrow(param_df) > 0) {
        # lexicographic order
        param_df <- param_df[order(param_df$name),]
        # numeric input for each parameters
        numeric_input_list <- lapply(
            split(param_df, seq(nrow(param_df))), 
            function(item) {
                return(
                    column(
                        width = 6,
                        numericInput(
                            ns(item$name), label = item$name,
                            value = item$value, min = min,
                            max = max, step = step
                        )
                    )
                )
            }
        )
        names(numeric_input_list) <- NULL
        
        # Convert the list to a tagList
        # this is necessary for the list of items to display properly.
        out <- tagList(
            h5(title) %>%
                helper(
                    type = "inline",
                    title = title,
                    content = doc
                ),
            do.call(fluidRow, numeric_input_list)
        )
    }
    # out
    return(out)
}

#' Data generation sampling parameter setup module ui
#' @keywords internal
#' @author Ghislain Durif
datagen_sampling_param_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3("Sample sizes"),
        uiOutput(ns("sample_param")),
        hr(),
        h3("Simulation number"),
        numericInput(
            ns("n_rep"),
            label = "Number of repetitions",
            value = 1,
            min = 1, max = NA, step = 1
        )
    )
}

#' Data generation model parameter setup module server
#' @keywords internal
#' @author Ghislain Durif
#' @param n_pop number of population in the model, as a `reactive`.
#' @param n_sample number of sample in the model, as a `reactive`.
datagen_sampling_param_server <- function(
    input, output, session,
    n_pop = reactive({NULL}),
    n_sample = reactive({NULL})
) {
    # local reactive values
    local <- reactiveValues(
        n_pop = NULL
    )
    
    # get input
    observe({
        local$n_pop = n_pop()
    })
    
    # init output
    out <- reactiveValues(sample_sizes = list(), n_rep = NULL)
    
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
        out$n_rep <- input$n_rep
    })
    # output
    return(out)
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
    #     pprint(local$raw_param)
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
        
        # pprint(local$samples_sizes)
        # 
        # pprint(local$locus_type)
        
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
            diyabc_run_simu(project_dir = local$project_dir, n_core = 1),
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
