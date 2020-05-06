#' Historical model settings module ui
#' @keywords internal
#' @author Ghislain Durif
hist_model_set_module_ui <- function(id) {
    ns <- NS(id)
    verticalLayout(
        tagList(
            hr(),
            h3("Parameter values"),
            uiOutput(ns("Ne_param_value")),
            uiOutput(ns("time_param_value")),
            uiOutput(ns("rate_param_value"))
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
}

#' Historical model settings module server
#' @keywords internal
#' @author Ghislain Durif
hist_model_set_module_server <- function(input, output, session, scenario_def) {
    # init output reactive values
    out <- reactiveValues(param_value = list(),
                          sampling_param = list(),
                          nsimu = NULL)
    # parameters
    observe({
        # get Ne params
        param_list <- scenario_def$scenario_param$Ne_param
        # rendering
        output$Ne_param_value <- renderUI({
            render_model_param(session, param_list,
                               "Ne parameter(s)",
                               "Effective population size.", 
                               value = 100, min = 0, max = NA, step = 1)
        })
        # return value
        out$param_value$Ne <- lapply(param_list, function(param) {
            return(input$param)
        })
    })
    
    observe({
        # get time params
        param_list <- scenario_def$scenario_param$time_param
        # rendering
        output$time_param_value <- renderUI({
            render_model_param(session, param_list, "Time parameter(s)",
                               "Time in <b>backward generations</b>.",
                               value = 100, min = 0, max = NA, step = 1)
        })
        # return value
        out$param_value$time <- lapply(param_list, function(param) {
            return(input$param)
        })
    })
    
    observe({
        # get rate params
        param_list <- scenario_def$scenario_param$rate_param
        # rendering
        output$rate_param_value <- renderUI({
            render_model_param(session, param_list, "Admixture rate parameter(s)",
                               "Rate between 0 and 1.",
                               value = 0.5, min = 0, max = 1, step = 0.001)
        })
        # return value
        out$param_value$rate_param <- lapply(param_list, function(param) {
            return(input$param)
        })
    })
    
    # sample
    observe({
        # parameter
        param <- scenario_def$scenario_param
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
                    render_sampling_param(session, sample_id,
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
        out$sampling_param <- lapply(
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


#' Historical model module ui
#' @keywords internal
#' @author Ghislain Durif
hist_model_module_ui <- function(id, label = "hist_model", add=FALSE) {
    ns <- NS(id)
    tagList(
        verticalLayout(
            hist_model_def_module_ui(ns("hist_model_def")),
            hist_model_set_module_ui(ns("hist_model_set"))
        )
    )
}

#' Historical model module server
#' @keywords internal
#' @author Ghislain Durif
hist_model_module_server <- function(input, output, session) {
    # init output reactive values
    out <- reactiveValues()
    # historical model definition
    scenario_def <- callModule(hist_model_def_module_server, "hist_model_def")
    # historical model setting
    scenario_set <- callModule(hist_model_set_module_server, "hist_model_set", 
                               scenario_def = scenario_def)
    # check
    observe({
        print(scenario_set)
    })
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
render_sampling_param <- function(session, sample_id, time_tag, pop_id) {
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