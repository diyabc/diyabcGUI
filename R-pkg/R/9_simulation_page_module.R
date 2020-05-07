#' Simulation page ui
#' @keywords internal
#' @author Ghislain Durif
simu_page_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "Project settings",
                width = 12,
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                simu_proj_set_ui(ns("proj_set")) %>% 
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
                "FILLME"
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
#' @param project_spec `reactiveValues` with `name`, `dir` attributes.
#' @param scenario `reactiveValues` with `raw` attribute.
simu_page_server <- function(input, output, session, 
                             project_spec = reactiveValues(name = NULL, 
                                                           dir = NULL),
                             scenario = reactiveValues(raw = NULL),
                             existing = FALSE) {
    # project setting
    observeEvent(project_spec, {
        callModule(simu_proj_set_server, "proj_set", 
                   project_spec = project_spec)
    })
    # historical model
    observeEvent(scenario, {
        callModule(simu_hist_model_server, "hist_model",
                   scenario = scenario)
    })
    
    
    
}

#' Simulation project setting ui
#' @keywords internal
#' @author Ghislain Durif
simu_proj_set_ui <- function(id) {
    ns <- NS(id)
    tagList(
        proj_name_ui(ns("project_name"), label = "Project", 
                     default = "project_name"),
        dir_choice_ui(ns("project_dir"), label = "Directory")
    )
}

#' Simulation project setting server function
#' @keywords internal
#' @author Ghislain Durif
#' @param project_spec `reactiveValues` with `name` and `dir` attributes.
#' @importFrom shinyjs disable enable
simu_proj_set_server <- function(input, output, session, 
                                 project_spec = reactiveValues(name = NULL,
                                                               dir = NULL),
                                 existing = FALSE) {
    # init local reactive values
    local <- reactiveValues()
    # init output reactive values
    out <- reactiveValues()
    # project name server side
    observe({
        out$project_name <- callModule(proj_name_server, "project_name", 
                                       project_name = project_spec,
                                       existing = existing)
    })
    # project dir server side
    observe({
        out$project_dir <- callModule(dir_choice_server, "project_dir", 
                                      default = project_spec)
    })
    # output
    return(out)
}

#' Simulation historical model ui
#' @keywords internal
#' @author Ghislain Durif
simu_hist_model_ui <- function(id) {
    ns <- NS(id)
    tagList()
}

#' Simulation historical model server
#' @keywords internal
#' @author Ghislain Durif
#' @param scenario `reactiveValues` with `raw` attribute.
#' @importFrom shinyjs disable enable
simu_hist_model_server <- function(input, output, session, 
                                 scenario = reactiveValues(raw = NULL)) {}
