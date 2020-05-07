#' Home module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard box
#' @importFrom shinyWidgets actionGroupButtons
home_module_ui <- function(id, label = "home") {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "Data analysis",
                width = 12, 
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                tags$div(
                    tags$p("Including the two modules of DIYABC Random Forest:"),
                    tags$ul(
                        tags$li("training set simulation"),
                        tags$li("random forest analyses")
                    )
                ) %>% 
                    helper(type = "markdown", 
                           content = "data_analysis"),
                project_module_ui(ns("analysis_project"))
            )
        ),
        fluidRow(
            box(
                title = "Pseudo-observed dataset simulation",
                width = 12, 
                status = "info", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                tags$div(
                    tags$p("Direct use of DIYABC-RF simulation engine.") %>% 
                        helper(type = "markdown", 
                               content = "data_simulation")
                ),
                project_module_ui(ns("simu_project"))
            )
        )
    )
}

#' Home module server
#' @keywords internal
#' @author Ghislain Durif
home_module_server <- function(input, output, session) {
    callModule(project_module_server, "analysis_project")
    callModule(project_module_server, "simu_project")
}
