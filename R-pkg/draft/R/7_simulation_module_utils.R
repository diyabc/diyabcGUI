#' Simulation project setting module ui
#' @keywords internal
#' @author Ghislain Durif
simu_project_setting_module_ui <- function(id, project_name = "project_name") {
    ns <- NS(id)
    tagList(
        project_input_module_ui(
            ns("project_name"), 
            label = "Project", 
            default = project_name
        ),
        dir_input_module_ui(
            ns("project_dir"),
            label = "Directory"
        )
    )
}

#' Simulation project setting module server function
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyjs disable enable
simu_project_setting_module_server <- function(
    input, output, session, project_name = NULL, project_dir = NULL,
    existing = FALSE) {
    # init output reactive values
    out <- reactiveValues(
        project_name = NULL,
        project_dir = NULL
    )
    # update name
    observe({
        out$project_name <- callModule(project_input_module_server, 
                                       "project_name", 
                                       project_name = project_name, 
                                       existing = existing)
    })
    # update directory
    observe({
        out$project_dir <- callModule(dir_input_module_server, 
                                      "project_dir",
                                      default_dir = project_dir)
    })
    # output
    return(out)
}

#' Simulation model setting module ui
#' @keywords internal
#' @author Ghislain Durif
simu_model_setting_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        hist_model_module_ui(ns("hist_model"))
    )
}

#' Simulation model setting module server function
#' @keywords internal
#' @author Ghislain Durif
simu_model_setting_module_server <- function(input, output, session, 
                                             scenario_raw = NULL, 
                                             project_dir = NULL) {
    # init output reactive values
    out <- reactiveValues()
    # historical model
    observe({
        out$hist_model <- callModule(hist_model_module_server, "hist_model",
                                     scenario_raw = scenario_raw,
                                     project_dir = project_dir)
    })
}

#' Simulation genetic spec module ui
#' @keywords internal
#' @author Ghislain Durif
simu_genetic_spec_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        genetic_data_module_ui(ns("genetic_data"))
    )
}

#' Simulation genetic spec module server
#' @keywords internal
#' @author Ghislain Durif
simu_genetic_spec_module_server <- function(input, output, session) {
    # historical model
    callModule(genetic_data_module_server, "genetic_data")
}

#' Simulation project action module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyWidgets actionGroupButtons
simu_project_action_module_ui <- function(id) {
        ns <- NS(id)
        tagList(
            actionGroupButtons(
                inputIds = c(ns("duplicate"), 
                             ns("simulate")),
                labels = list("Duplicate project", "Simulate data"),
                fullwidth = TRUE
            )
        )
    }

#' Simulation project setting module server function
#' @keywords internal
#' @author Ghislain Durif
simu_project_action_module_server <- function(input, output, session) {
    # init output values
    out <- reactiveValues(duplicate = NULL, 
                          simulate = NULL)
    # watch for actions
    observeEvent(input$duplicate, {
        out$duplicate <- input$duplicate
    })
    observeEvent(input$duplicate, {
        out$simulate <- input$simulate
    })
    # output
    return(out)
}