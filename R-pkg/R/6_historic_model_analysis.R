#' Historical model choice module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyWidgets actionGroupButtons
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
        )
    )
}

#' Historical model choice module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_c
hist_model_choice_module_server <- function(input, output, session, 
                                            scenario_choice = 1,
                                            scenario_number = 1) {
    # init local reactive values
    local <- reactiveValues(
        current_scenario = scenario_choice,
        candidate_scenarii = 1:scenario_number,
        choice_length = scenario_number
    )
    # init output reactive values
    out <- reactiveValues()
    # choose scenario
    observeEvent(input$select, {
        local$current_scenario <- as.integer(input$select)
        out$choice <- str_c("scenario", local$current_scenario)
        print("select scenario (current/length/possible)")
        print(local$current_scenario)
        print(local$choice_length)
        print(local$candidate_scenarii)
    })
    # add scenario
    observeEvent(input$add, {
        if(local$choice_length < 100) {
            # update candidate scenarii id
            local$candidate_scenarii <- c(local$candidate_scenarii,
                                          max(local$candidate_scenarii) + 1)
            # update candidate scenarii length
            local$choice_length <- local$choice_length + 1
            # update current scenario
            current_scenario <- tail(local$candidate_scenarii, 1)
            # update selector
            updateSelectInput(
                session, "select",
                choices = setNames(
                    as.list(local$candidate_scenarii),
                    str_c("Scenario",
                          1:(local$choice_length),
                          sep = " ")
                ),
                selected = current_scenario
            )
        }
    })
    # remove scenario
    observeEvent(input$remove, {
        if(local$choice_length > 1) {
            # find index of current scenario
            ind <- which(local$candidate_scenarii == local$current_scenario)
            # new current scenario = preceding scenario
            new_ind <- ifelse(ind > 1, ind-1, ind)
            # update candidate scenarii id
            local$candidate_scenarii <- local$candidate_scenarii[-ind]
            # update candidate scenarii length
            local$choice_length <- local$choice_length - 1
            # update current scenario
            current_scenario <- local$candidate_scenarii[new_ind]
            # update selector
            updateSelectInput(
                session, "select",
                choices = setNames(
                    as.list(local$candidate_scenarii),
                    str_c("Scenario",
                          1:(local$choice_length),
                          sep = " ")
                ),
                selected = current_scenario
            )
        }
    })
    # output
    return(out)
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
hist_model_render_module_server <- function(input, output, session, 
                                            scenario_key, scenario,
                                            project_dir = NULL) {
    # init output reactive values
    out <- reactiveValues()
    # module namespace
    ns <- session$ns
    # render current scenario
    observe({
        output$render_model <- renderUI({
            hist_model_def_module_ui(
                ns(scenario_key)
            )
        })
    })
    # server function
    observe({
        out$scenario <- callModule(
                                hist_model_def_module_server, scenario_key, 
                                scenario_raw = scenario$raw,
                                project_dir = project_dir)
        print("scenario update")
        print(out$scenario$raw)
    })
    # output
    return(out)
}


#' Historical model module ui
#' @keywords internal
#' @author Ghislain Durif
hist_model_prior_module_ui <- function(id, label = "hist_model") {
    ns <- NS(id)
    tagList(
        h3("Priors"),
        tagList(
            h4("Parameter values"),
            tags$hr(),
            uiOutput(ns("Ne_param_value")),
            uiOutput(ns("time_param_value")),
            uiOutput(ns("rate_param_value"))
        ),
        hr(),
        h3("Constraints")
    )
}

#' Historical model module server
#' @keywords internal
#' @author Ghislain Durif
hist_model_prior_module_server <- function(input, output, session) {}