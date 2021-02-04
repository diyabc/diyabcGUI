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
                status = "info", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE,
                genetic_loci_ui(ns("gene_set"))
            ),
            box(
                title = "Data file generation",
                width = 12,
                status = "warning", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE,
                datafile_gen_ui(ns("datafile_gen"))
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
        raw_scenario = NULL,
        valid = FALSE
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
    
    ## genetic loci
    gene_set <- callModule(
        genetic_loci_server, "gene_set",
        locus_type = reactive(proj_set$locus_type),
        seq_mode = reactive(proj_set$seq_mode)
    )
    
    ## valid ?
    observe({
        req(!is.null(proj_set$valid))
        req(!is.null(hist_model$valid))
        req(proj_set$locus_type)
        local$valid <- proj_set$valid && hist_model$valid && gene_set$valid
        
        # pprint(str_c("valid setup ? ", local$valid))
    })
    
    ## data generation
    callModule(
        datafile_gen_server, "datafile_gen",
        proj_dir = reactive(proj_set$proj_dir),
        proj_name = reactive(proj_set$proj_name),
        valid = reactive(local$valid),
        raw_param = reactive(hist_model$param_values),
        raw_scenario = reactive(hist_model$raw_scenario),
        locus_description = reactive(gene_set$locus_description),
        mss_group_prior = reactive(gene_set$mss_group_prior),
        n_group = reactive(gene_set$n_group),
        sample_sizes = reactive(hist_model$sample_sizes),
        n_rep = reactive(hist_model$n_rep),
        sex_ratio = reactive(gene_set$sex_ratio),
        seq_mode = reactive(proj_set$seq_mode),
        locus_type = reactive(proj_set$locus_type)
    )
    
    ## admin
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
        seq_mode = NULL,
        valid = FALSE
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
    
    ## valid ?
    observe({
        req(!is.null(local$valid_proj_name))
        out$valid <- local$valid_proj_name
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
        scenario_param = NULL,
        valid = FALSE,
        param_values = NULL,
        sample_sizes = NULL,
        n_rep = NULL
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
        out$valid <- hist_model$valid
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
        out$param_values <- param_setting$param_values
        out$sample_sizes <- param_setting$sample_sizes
        out$n_rep <- param_setting$n_rep
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
        n_sample = NULL,
        sample_pop = NULL,
        sample_time = NULL
    )
    
    # get input
    observe({
        local$scenario_cond = scenario_cond()
        local$scenario_param = scenario_param()
        # pprint("check input")
        # pprint(local$scenario_cond)
        # pprint(local$scenario_param)
        
        if(isTruthy(local$scenario_param)) {
            # number of populations
            if(isTruthy(local$scenario_param$npop)) {
                local$n_pop <- local$scenario_param$npop
            }
            if(isTruthy(local$scenario_param$event_type)) {
                # number of sampling
                sample_event_ind <- local$scenario_param$event_type == "sample"
                local$n_sample <- sum(sample_event_ind)
                
                if(local$n_sample > 0) {
                    # populations concerned by sample 
                    if(isTruthy(local$scenario_param$event_pop)) {
                        local$sample_pop <- 
                            local$scenario_param$event_pop[sample_event_ind]
                    }
                    # time of sampling
                    if(isTruthy(local$scenario_param$event_time)) {
                        local$sample_time <- 
                            local$scenario_param$event_time[sample_event_ind]
                    }
                }
            }
        } 
    })
    
    # # debugging
    # observe({
    #     pprint(str_c("n_sample = ", local$n_sample))
    #     pprint(str_c("n_pop = ", local$n_pop))
    # })
    
    # init output reactive values
    out <- reactiveValues(param_values = NULL,
                          sample_sizes = NULL,
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
        n_sample = reactive(local$n_sample),
        sample_pop = reactive(local$sample_pop),
        sample_time = reactive(local$sample_time)
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
    n_sample = reactive({NULL}),
    sample_pop = reactive({NULL}),
    sample_time = reactive({NULL})
) {
    # local reactive values
    local <- reactiveValues(
        n_pop = NULL,
        n_sample = NULL,
        sample_pop = NULL,
        sample_time = NULL,
        sample_df = data.frame(
            id = character(),
            pop = character(),
            time = character(),
            tag = character(),
            n_f = numeric(),
            n_m = numeric(),
            stringsAsFactors=FALSE
        )
    )
    
    # get input
    observe({
        local$n_pop <- n_pop()
        local$n_sample <- n_sample()
        local$sample_pop <- sample_pop()
        local$sample_time <- sample_time()
    })
    
    # format input
    observe({
        req(local$n_sample)
        req(local$n_sample > 0)
        req(length(local$sample_pop) > 0)
        req(length(local$sample_time) > 0)
        
        # parse new samples (if any)
        local$sample_df <- Reduce(
            "bind_rows",
            lapply(
                1:local$n_sample, 
                function(ind) {
                    return(data.frame(
                        id = as.integer(ind),
                        pop = as.character(local$sample_pop[ind]),
                        time = as.character(local$sample_time[ind]),
                        tag = str_c("pop_", local$sample_pop[ind],
                                    "_time_", local$sample_time[ind]),
                        n_f = as.integer(25),
                        n_m = as.integer(25),
                        stringsAsFactors=FALSE
                    ))
                }
            )
        )
    })
    
    # # debugging
    # observe({
    #     pprint("sample_df")
    #     pprint(local$sample_df)
    # })
    
    # init output
    out <- reactiveValues(sample_sizes = NULL, n_rep = NULL)
    
    # setup sample size input
    observe({
        req(nrow(local$sample_df) > 0)
        output$sample_param <- renderUI({
            render_sample_sizes(
                session, local$sample_df
            )
        })
    })

    # get input value
    observe({
        req(nrow(local$sample_df) > 0)
        for(ind in seq(nrow(local$sample_df))) {
            item_tag <- local$sample_df$tag[ind]

            item_name <- str_c("sample_", item_tag, "_f")
            if(isTruthy(input[[ item_name ]])) {
                current_value <- input[[ item_name ]]
                if(local$sample_df$n_f[ind] != current_value) {
                    local$sample_df$n_f[ind] <- current_value
                }
            }

            item_name <- str_c("sample_", item_tag, "_m")
            if(isTruthy(input[[ item_name ]])) {
                current_value <- input[[ item_name ]]
                if(local$sample_df$n_m[ind] != current_value) {
                    local$sample_df$n_m[ind] <- current_value
                }
            }
        }
    })

    # update output
    observe({
        req(nrow(local$sample_df) > 0)
        out$sample_sizes <- unname(unlist(lapply(
            split(local$sample_df, seq(nrow(local$sample_df))),
            function(item) {
                return(
                    str_c(item$n_f, item$n_m, sep = " ")
                )
            }
        )))
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
render_sample_sizes <- function(session, sample_df) {
    
    ns <- session$ns
    out <- tagList()
    
    if(nrow(sample_df) > 0) {
        # numeric input for each samples
        numeric_input_list <- lapply(
            split(sample_df, seq(nrow(sample_df))), 
            function(item) {
                return(
                    tagList(
                        h5("From pop. ", tags$b(item$pop),
                           " at time ", tags$b(item$time)),
                        fluidRow(
                            column(
                                width = 10,
                                verticalLayout(
                                    numericInput(
                                        ns(str_c("sample_", item$tag, "_f")),
                                        label = "Female",
                                        value = item$n_f,
                                        min = 0
                                    ),
                                    numericInput(
                                        ns(str_c("sample_", item$tag, "_m")),
                                        label = "Male",
                                        value = item$n_m,
                                        min = 0
                                    )
                                )
                            )
                        )
                    )
                )
            }
        )
        names(numeric_input_list) <- NULL
        
        out <- do.call(flowLayout, numeric_input_list)
    }
    # out
    return(out)
}

#' Data simulation module ui for synthetic data generation
#' @keywords internal
#' @author Ghislain Durif
datafile_gen_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("feedback_valid")),
        actionBttn(
            inputId = ns("validate"),
            label = "Validate simulation project",
            style = "fill",
            block = TRUE,
            color = "success"
        ),
        uiOutput(ns("feedback_validate")),
        hr(),
        actionBttn(
            inputId = ns("simulate"),
            label = "Simulate",
            style = "fill",
            block = TRUE
        ),
        hr(),
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
            style = "width:60vw; overflow:scroll; overflow-y:scroll; height:130px; resize: both;"
        )
    )
}

#' Data simulation module server for synthetic data generation
#' @keywords internal
#' @author Ghislain Durif
#' @param proj_dir project directory as a `reactive`.
#' @param proj_name project name as a `reactive`.
#' @param valid boolean indicating if the project setting are valid, 
#' as a `reactive`.
#' @param raw_param scenario parameter raw setting as a `reactive`. 
#' @param raw_scenario raw scenario as a `reactive`.
#' @param locus_description genetic locus setting as a `reactive`.
datafile_gen_server <- function(
    input, output, session,
    proj_dir = reactive({NULL}),
    proj_name = reactive({NULL}),
    valid = reactive({FALSE}),
    raw_param = reactive({NULL}),
    raw_scenario = reactive({NULL}),
    locus_description = reactive({NULL}),
    mss_group_prior = reactive({NULL}),
    n_group = reactive({NULL}),
    sample_sizes = reactive({NULL}),
    n_rep = reactive({NULL}),
    sex_ratio = reactive({NULL}),
    seq_mode = reactive({NULL}),
    locus_type = reactive({NULL})
) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        validated = FALSE,
        diyabc_run_process = NULL,
        diyabc_run_result = NULL,
        log_file_content = NULL,
        # input
        proj_dir = NULL,
        proj_name = NULL,
        valid = FALSE,
        raw_param = NULL,
        raw_scenario = NULL,
        locus_description = NULL,
        mss_group_prior = NULL,
        n_group = NULL,
        sample_sizes = NULL,
        sex_ratio = NULL,
        n_rep = NULL,
        seq_mode = NULL,
        locus_type = NULL
    )
    # get input
    observe({
        local$proj_dir <- proj_dir()
        local$proj_name <- proj_name()
        local$valid <- valid()
        local$raw_param <- raw_param()
        local$raw_scenario <- raw_scenario()
        local$locus_description <- locus_description()
        local$mss_group_prior <- mss_group_prior()
        local$n_group <- n_group()
        local$sample_sizes <- sample_sizes()
        local$sex_ratio <- sex_ratio()
        local$n_rep <- n_rep()
        local$seq_mode <- seq_mode()
        local$locus_type <- locus_type()
        
        # pprint(str_c("valid_project ? ", local$valid))
        local$validated <- FALSE
    })
    
    ## init output
    out <- reactiveValues()
    
    ## feedback on project setup
    output$feedback_valid <- renderUI({
        req(!is.null(local$valid))
        if(!local$valid) {
            helpText(
                icon("warning"),
                "Issue with the configuration or paremeters",
                "of the simulation project.",
                "Please check the different setup panels above."
            )
        } else {
            NULL
        }
    })
    
    
    # deactivate validate if project not valid
    observeEvent(local$valid, {
        req(!is.null(local$valid))
        if(local$valid) {
            shinyjs::enable("validate")
        } else {
            shinyjs::disable("validate")
        }
    })
    
    # validate project
    observeEvent(input$validate, {
        req(input$validate)
        req(!is.null(local$valid))
        if(local$valid) {
            local$validated <- TRUE
        } else {
            local$validated <- FALSE
        }
    })
    
    # validate feedback
    output$feedback_validate <- renderUI({
        req(!is.null(local$validated))
        if(!local$validated) {
            helpText(
                icon("warning"),
                "You need to validate your project", 
                "to be able to run the simulation."
            )
        } else {
            NULL
        }
    })
    
    # deactivate simulate if project not validated
    observeEvent(local$validated, {
        req(!is.null(local$validated))
        if(local$validated) {
            shinyjs::enable("simulate")
        } else {
            shinyjs::disable("simulate")
        }
    })
    
    ## write header file
    observeEvent(input$validate, {
        
        local$validated <- FALSE
        
        # pprint("- proj_dir")
        # pprint(local$proj_dir)
        # pprint("- proj_name")
        # pprint(local$proj_name)
        # pprint("- raw_param")
        # pprint(local$raw_param)
        # pprint("- raw_scenario")
        # pprint(local$raw_scenario)
        # pprint("- locus_description")
        # pprint(local$locus_description)
        # pprint("- mss_group_prior")
        # pprint(local$mss_group_prior)
        # pprint("- n_group")
        # pprint(local$n_group)
        # pprint("- sample_sizes")
        # pprint(local$sample_sizes)
        # pprint("- n_rep")
        # pprint(local$n_rep)
        # pprint("- sex_ratio")
        # pprint(local$sex_ratio)
        # pprint("- seq_mode")
        # pprint(local$seq_mode)
        # pprint("- locus_type")
        # pprint(local$locus_type)
        
        req(local$proj_dir)
        req(local$proj_name)
        req(local$raw_param)
        req(local$raw_scenario)
        req(local$locus_description)
        req(local$n_group)
        req(local$sample_sizes)
        req(local$n_rep)
        req(local$sex_ratio)
        req(local$seq_mode)
        req(local$locus_type)
        
        if(local$locus_type == "mss") {
            req(local$mss_group_prior)
        }
        
        res <- tryCatch(
            write_headersim(
                local$proj_name, local$proj_dir, 
                local$seq_mode, local$locus_type,
                local$raw_scenario, local$raw_param, 
                local$locus_description, local$mss_group_prior,
                local$n_group,
                local$sample_sizes,
                local$n_rep, local$sex_ratio
            ), 
            error = function(e) {pprint(e); return(e)}
        )
        
        if("error" %in% class(res)) {
            local$validated <- FALSE
            showNotification(
                id = ns("headerfile_nok"),
                duration = 10,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"),
                        "Issue when writing simulation configuration file.",
                        "Check configuration panels above."
                    )
                )
            )
        } else {
            local$validated <- TRUE
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
        }
    })
    
    ## read log file
    log_file_content <- function() return(rep("", nlog))
    observeEvent(local$proj_dir, {
        req(local$proj_dir)
        log_file_content <<- reactiveFileReader(
            1000, session,
            file.path(local$proj_dir, "diyabc_run_call.log"),
            function(file) {
                if(file.exists(file)) {
                    readLines(file, warn=FALSE)
                } else {
                    character(0)
                }
            }
        )
    })
    
    observe({
        local$log_file_content <- log_file_content()
    })
    
    ## run simulation
    observeEvent(input$simulate, {
        
        req(!is.null(local$validated))
        req(local$proj_dir)
        
        ## run in progress
        if(!is.null(local$diyabc_run_process)) {
            showNotification(
                id = ns("run_in_progress"),
                duration = 5,
                closeButton = TRUE,
                type = "warning",
                tagList(
                    tags$p(
                        icon("warning"),
                        "Run in progress."
                    )
                )
            )
        } else {
            ## prepare run
            req(is.null(local$diyabc_run_process))
            
            ## init progress bar
            updateProgressBar(
                session = session,
                id = "simu_progress",
                value = 0, total = local$n_rep,
                title = "Running simulation:"
            )
            
            ## check if possible to run
            if(!local$validated) {
                local$feedback <- helpText(
                    icon("warning"), "Project is not validated."
                )
            } else if(! "headersim.txt" %in% list.files(local$proj_dir)) {
                local$feedback <- helpText(
                    icon("warning"), 
                    "Missing header configuration file."
                )
                showNotification(
                    id = ns("missing_files"),
                    duration = 5,
                    closeButton = TRUE,
                    type = "warning",
                    tagList(
                        tags$p(
                            icon("warning"), 
                            "Missing header configuration file."
                        )
                    )
                )
            } else {
                ## ready to run
                
                # debugging
                # pprint("check options")
                # pprint(getOption("diyabcGUI"))
                # pprint(getOption("shiny.maxRequestSize"))
                
                local$feedback <- helpText(
                    icon("spinner", class = "fa-spin"),
                    "Simulations are running."
                )
                
                logging("Running simulation")
                local$diyabc_run_process <- diyabc_run_datagen(
                    local$proj_dir
                )
            }
        }
    })
    
    ## monitor simulation run
    observeEvent(local$diyabc_run_process, {
        req(!is.null(local$diyabc_run_process))
        
        pprint("diyabc run process")
        pprint(local$diyabc_run_process)
        
        observe({
            req(!is.null(local$diyabc_run_process))
            proc <- local$diyabc_run_process
            req(!is.null(proc$is_alive()))
            if(proc$is_alive()) {
                invalidateLater(2000, session)
            } else {
                local$diyabc_run_result <- proc$get_exit_status()
            }
        })
    })
    
    ## clean up after run
    observeEvent(local$diyabc_run_result, {
        
        req(!is.null(local$diyabc_run_result))
        
        logging("diyabc simu run exit status:",
                local$diyabc_run_result)
        
        ## log
        output$run_log <- renderUI({
            req(!is.null(local$log_file_content))
            do.call(
                tagList,
                as.list(local$log_file_content)
            )
        })
        
        ## check run
        # run ok
        if(local$diyabc_run_result == 0) {
            local$feedback <- helpText(
                icon("check"), "Run succeeded."
            )
            updateProgressBar(
                session = session,
                id = "simu_progress",
                value = local$n_rep, total = local$n_rep,
                title = "Running simulation:"
            )
            showNotification(
                id = ns("run_ok"),
                duration = 5,
                closeButton = TRUE,
                type = "message",
                tagList(
                    tags$p(
                        icon("check"),
                        "Simulations are done."
                    )
                )
            )
        } else if(local$diyabc_run_result == -1000) {
            ## stopped run
            local$feedback <- helpText(
                icon("warning"), "Simulation run was stopped."
            )
            showNotification(
                id = ns("stop_run"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"),
                        "Simulation run was stopped."
                    )
                )
            )
        } else if(local$diyabc_run_result == -2000) {
            ## stopped run
            local$feedback <- helpText(
                icon("warning"), 
                "Error in simulation process:", 
                "check your scenarios, priors and conditions."
            )
            showNotification(
                id = ns("stop_run"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"),
                        "Error in simulation process:", 
                        "check your scenarios, priors and conditions."
                    )
                )
            )
        } else {
            ## error during run
            local$feedback <- helpText(
                icon("warning"), "Issues with run (see log panel below)."
            )
            showNotification(
                id = ns("run_not_ok"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"),
                        "A problem happened during simulations."
                    )
                )
            )
        }
        
        # reset
        local$diyabc_run_result <- NULL
        local$diyabc_run_process <- NULL
    })
    
    ## stop run
    observeEvent(input$stop, {
        ## if no current run
        if(is.null(local$diyabc_run_process)) {
            local$feedback <- helpText(
                icon("warning"), "No current run to stop."
            )
            showNotification(
                id = ns("no_run"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"), "No current run to stop."
                    )
                )
            )
        } else {
            ## stop current run
            proc <- local$diyabc_run_process
            if(proc$is_alive()) {
                proc$kill()
            }
            local$diyabc_run_result <- -1000
        }
    })
}
