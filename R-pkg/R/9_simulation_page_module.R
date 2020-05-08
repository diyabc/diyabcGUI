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
                "FILLME"
                # simu_hist_model_ui(ns("hist_model"))
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
#' @param project_dir
#' @param scenario `reactiveValues` with `raw` attribute.
simu_page_server <- function(input, output, session) {
    # init output reactive values
    out <- reactiveValues(
        setting = NULL,
        scenario = NULL
    )
    # project setting
    observe({
        out$setting <- callModule(simu_proj_set_server, "proj_set")
    })
    # # historical model
    # observeEvent(scenario, {
    #     out$scenario <- callModule(simu_hist_model_server, "hist_model",
    #                                project_dir = out$setting$project_dir, 
    #                                scenario = scenario)
    # })
    # output
    return(out)
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
#' @param project `reactiveValues` with `name` and `dir` attributes.
#' @importFrom shinyjs disable enable
simu_proj_set_server <- function(input, output, session) {
    # init output reactive values
    out <- reactiveValues(
        project_name = NULL,
        parent_folder = NULL,
        project_dir = reactiveValues(path = NULL)
    )
    # project name server side
    observe({
        out$project_name <- callModule(proj_name_server, "project_name")
    })
    # project folder server side
    observe({
        out$parent_folder <- callModule(dir_choice_server, "project_dir")
    })
    # project directory
    observe({
        out$project_dir$path <- file.path(out$parent_folder$path,
                                          out$project_name$fullname)
    })
    # output
    return(out)
}
#' 
#' #' Simulation historical model ui
#' #' @keywords internal
#' #' @author Ghislain Durif
#' simu_hist_model_ui <- function(id) {
#'     ns <- NS(id)
#'     tagList(
#'         hist_model_ui(ns("hist_model"))
#'     )
#' }
#' 
#' #' Simulation historical model server
#' #' @keywords internal
#' #' @author Ghislain Durif
#' #' @param project_dir `reactiveValues` with `path` attributes.
#' #' @param scenario `reactiveValues` with `raw` attribute.
#' #' @importFrom shinyjs disable enable
#' simu_hist_model_server <- function(input, output, session,
#'                                    project_dir = reactiveValues(path = NULL),
#'                                    scenario = reactiveValues(raw = NULL)) {
#'     callModule(hist_model_server, "hist_model",
#'                project_dir = project_dir,
#'                scenario = scenario)
#' }
