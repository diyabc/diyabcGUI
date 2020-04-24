#' Historical model settings module ui
#' @keywords internal
#' @author Ghislain Durif
hist_model_set_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        navbarPage(
            "Settings",
            tabPanel(
                "Parameters",
                tagList(
                    h4("Parameter values"),
                    tags$hr(),
                    uiOutput(ns("Ne_param_value")),
                    uiOutput(ns("time_param_value")),
                    uiOutput(ns("rate_param_value"))
                )
            ),
            tabPanel(
                "Samples",
                uiOutput(ns("sample_param"))
            ),
            tabPanel(
                "Simulations",
                numericInput(
                    ns("nsimu"),
                    label = "Number of repetitions",
                    value = 1
                )
            )
        )
    )
}

#' Historical model settings module server
#' @keywords internal
#' @author Ghislain Durif
hist_model_set_module_server <- function(input, output, session, scenario_def) {
    
    # parameters
    output$Ne_param_value <- renderUI({
        param_list <- scenario_def$param$Ne_param
        render_model_param(session, param_list,
                           "Ne parameter(s)",
                           "Effective population size.", 
                           value = 100, min = 0, max = NA, step = 1)
    })
    
    output$time_param_value <- renderUI({
        param_list <- scenario_def$param$time_param
        render_model_param(session, param_list, "Time parameter(s)",
                           "Time in <b>generations</b> backward.",
                           value = 100, min = 0, max = NA, step = 1)
    })
    
    output$rate_param_value <- renderUI({
        param_list <- scenario_def$param$rate_param
        render_model_param(session, param_list, "Admixture rate parameter(s)",
                           "Rate between 0 and 1.",
                           value = 0.5, min = 0, max = 1, step = 0.001)
    })
    
    # sample
    output$sample_param <- renderUI({
        param <- scenario_def$param
        
        nsample <- sum(param$event_type == "sample")
        npop <- param$npop

        sample_pop <- param$event_pop[param$event_type == "sample"]
        sample_time <- param$event_time[param$event_type == "sample"]

        # numeric input for each sample
        numeric_input_list <- list()
        if(!is.null(npop) & nsample > 0) {
            numeric_input_list <- lapply(1:nsample, function(sample_id) {
                render_sampling_param(session, sample_id,
                                      unlist(sample_time[sample_id]),
                                      unlist(sample_pop[sample_id]))
            })
        }
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        tagList(
            h4("Sample sizes"),
            tags$hr(),
            do.call(flowLayout, numeric_input_list)
        )
    })
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
    
    scenario_def <- callModule(hist_model_def_module_server, "hist_model_def")
    
    callModule(hist_model_set_module_server, "hist_model_set", 
               scenario_def = scenario_def)
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
    ns <- session$ns
    tag_name <- paste0("sample", sample_id , 
                       "_pop", pop_id, 
                       "_time", time_tag)
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