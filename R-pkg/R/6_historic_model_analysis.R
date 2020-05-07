#' Adding historical model module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyWidgets actionGroupButtons
hist_model_add_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        flowLayout(
            selectInput(
                ns("select"), label = NULL, 
                choices = list("Scenario 1" = 1), 
                selected = 1
            ),
            actionGroupButtons(
                inputIds = c(ns("update"), ns("add"), ns("remove")),
                labels = list("Update", "Add", "Remove"),
                fullwidth = TRUE
            )
        ),
        hr(),
        hist_model_def_module_ui(ns("hist_model"))
    )
}

#' Adding historical model module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_c
hist_model_add_module_server <- function(input, output, session, 
                                         project_dir = NULL) {
    # init local reactive values
    local <- reactiveValues(
        current_scenario = 1,
        possible_scenario = 1,
        scenario_number = 1,
        current_key = str_c("scenario", 1)
    )
    # init output reactive values
    out <- reactiveValues(
        scenario_list = reactiveValues(scenario1 = list(raw = ""))
    )
    # module namespace
    ns <- session$ns
    # choose a scenario
    observeEvent(input$select, {
        local$current_scenario <- input$select
        local$current_key <- str_c("scenario", input$select)
        print("select scenario (current/length/possible)")
        print(local$current_scenario)
        print(local$scenario_number)
        print(local$possible_scenario)
    })
    # add scenario
    observeEvent(input$add, {
        if(local$scenario_number < 100) {
            # update candidate scenarii id
            local$possible_scenario <- c(local$possible_scenario, 
                                   max(local$possible_scenario) + 1)
            # update candidate scenarii length
            local$scenario_number <- local$scenario_number + 1
            # update current scenario
            tmp_current <- tail(local$possible_scenario, 1)
            # update selector
            updateSelectInput(
                session, "select",
                choices = setNames(
                    as.list(local$possible_scenario),
                    str_c("Scenario",
                          1:(local$scenario_number),
                          sep = " ")
                ),
                selected = tmp_current
            )
            # update scenario list
            new_key <- str_c("scenario", tmp_current)
            out$scenario_list[[ new_key ]] <- list(raw = "")
        }
    })
    # remove scenario
    observeEvent(input$remove, {
        if(local$scenario_number > 1) {
            # find index of current scenario
            ind <- which(local$possible_scenario == local$current_scenario)
            tmp_old <- local$current_scenario
            # new current scenario = preceding scenario
            new_ind <- ifelse(ind > 1, ind-1, ind)
            # update candidate scenarii id
            local$possible_scenario <- local$possible_scenario[-ind]
            # update candidate scenarii length
            local$scenario_number <- local$scenario_number - 1
            # update current scenario
            tmp_current <- local$possible_scenario[new_ind]
            # update selector
            updateSelectInput(
                session, "select",
                choices = setNames(
                    as.list(local$possible_scenario),
                    str_c("Scenario",
                          1:(local$scenario_number),
                          sep = " ")
                ),
                selected = tmp_current
            )
            # update scenario list
            old_key <- str_c("scenario", tmp_old)
            out$scenario_list[[ old_key ]] <- NULL
        }
    })
    # manage scenario content
    observeEvent(input$update, {
        
        # current scenario
        scenario_def <- out$scenario_list[[ local$current_key ]]
        print("scenario raw")
        print(scenario_def$raw)
        # update scenario list
        tmp <- callModule(
            hist_model_def_module_server, "hist_model",
            scenario_raw = scenario_def$raw,
            project_dir = project_dir)
        print(tmp$raw)
        out$scenario_list[[ local$current_key ]] <- tmp$raw
    })
    
    # debugging
    observe({
        print("scenario list")
        lapply(reactiveValuesToList(out$scenario_list), print)
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