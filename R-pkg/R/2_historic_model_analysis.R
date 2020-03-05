#' Historical model choice module ui
#' @keywords internal
#' @author Ghislain Durif
hist_model_choice_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        flowLayout(
            selectInput(
                ns("select"), label = NULL, 
                choices = list("Scenario 1" = 1), 
                selected = 1
            ),
            actionGroupButtons(
                inputIds = c(ns("add"), ns("remove")),
                labels = list("Add", "Remove"),
                fullwidth = TRUE
            )
        ),
        hist_model_render_module_ui(ns("model_def")),
        hist_model_prior_module_ui(ns("priors"))
    )
}

#' Historical model choice module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_c
hist_model_choice_module_server <- function(input, output, session, context) {
    
    # choose scenario
    observeEvent(input$select, {
        context$scenarii$current_scenario <- as.numeric(input$select)
        print("select scenario (current/length/possible)")
        print(context$scenarii$current_scenario)
        print(context$scenarii$choice_length)
        print(context$scenarii$candidate_scenarii)
    })
    
    # add scenario
    observeEvent(input$add, {
        if(context$scenarii$choice_length < 100) {
            # update candidate scenarii id
            context$scenarii$candidate_scenarii <-
                c(context$scenarii$candidate_scenarii,
                  max(context$scenarii$candidate_scenarii) + 1)
            # update candidate scenarii length
            context$scenarii$choice_length <-
                context$scenarii$choice_length + 1
            # update current scenario
            context$scenarii$current_scenario <-
                tail(context$scenarii$candidate_scenarii, 1)
            # update selector
            updateSelectInput(
                session, "select",
                choices = setNames(
                    as.list(context$scenarii$candidate_scenarii),
                    str_c("Scenario",
                          1:(context$scenarii$choice_length),
                          sep = " ")
                ),
                selected = context$scenarii$current_scenario
            )
        }
    })

    # remove scenario
    observeEvent(input$remove, {
        if(context$scenarii$choice_length > 1) {
            # find index of current scenario
            ind <- which(context$scenarii$candidate_scenarii ==
                             context$scenarii$current_scenario)
            # new current scenario = preceding scenario
            new_ind <- ifelse(ind > 1, ind-1, ind)
            # update candidate scenarii id
            context$scenarii$candidate_scenarii <-
                context$scenarii$candidate_scenarii[-ind]
            # update candidate scenarii length
            context$scenarii$choice_length <-
                context$scenarii$choice_length - 1
            # update current scenario
            context$scenarii$current_scenario <-
                context$scenarii$candidate_scenarii[new_ind]
            # update selector
            updateSelectInput(
                session, "select",
                choices = setNames(
                    as.list(context$scenarii$candidate_scenarii),
                    str_c("Scenario",
                          1:(context$scenarii$choice_length),
                          sep = " ")
                ),
                selected = context$scenarii$current_scenario
            )
        }
    })
     
    ## Current scenarii definition
    callModule(hist_model_render_module_server, "model_def", context = context)
    # ## Priors
    # callModule(hist_model_prior_module_server, "priors", context = context)
}

#' Historical model choice module context init
#' @keywords internal
#' @author Ghislain Durif
init_hist_model_choice_module_context <- function() {
    return(
        reactiveValues(
            candidate_scenarii = as.vector(1),
            choice_length = 1,
            current_scenario = 1,
            scenarii_list = reactiveValues()
        )
    )
}

#' Historical model render module ui
#' @keywords internal
#' @author Ghislain Durif
hist_model_render_module_ui <- function(id) {
    ns <- NS(id)
    uiOutput(ns("render_model"))
}

#' Historical model render module server
#' @keywords internal
#' @author Ghislain Durif
hist_model_render_module_server <- function(input, output, session, context) {
    
    # observe({
    #     # ind <- as.character(context$scenarii$current_scenario)
    #     # if(is.null(context$scenarii$scenario_list[[ ind ]])) {
    #     #     context$scenarii$scenario_list[[ ind ]] <- 
    #     #         init_hist_model_module_context()
    #     # }
    # })
    
    # render current scenario
    output$render_model <- renderUI({
        ns <- session$ns

        hist_model_def_module_ui(
            ns(paste0("scenario", context$analysis$scenarii$current_scenario))
        )
    })
    
    observe({
        
        # ind <- as.character(context$scenarii$current_scenario)
        # context$scenarii$scenario_list[[ ind ]] <-
        callModule(
            hist_model_def_module_server,
            paste0("scenario", 
                   context$analysis$scenarii$current_scenario)
        )
    })
    
    
}


#' Historical model module ui
#' @keywords internal
#' @author Ghislain Durif
hist_model_prior_module_ui <- function(id, label = "hist_model") {
    ns <- NS(id)
    tagList(
        navbarPage(
            "Priors",
            tabPanel(
                "Scenarii",
            ),
            tabPanel(
                "Parameters",
                tagList(
                    h4("Parameter values"),
                    tags$hr(),
                    uiOutput(ns("Ne_param_value")),
                    uiOutput(ns("time_param_value")),
                    uiOutput(ns("rate_param_value"))
                )
            )
        )
    )
}

#' Historical model module server
#' @keywords internal
#' @author Ghislain Durif
hist_model_prior_module_server <- function(input, output, session, context) {

}