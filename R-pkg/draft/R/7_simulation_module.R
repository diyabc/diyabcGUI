#' Simulation module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard box
#' @importFrom shinyWidgets actionGroupButtons
simu_module_ui <- function(id, project_name = "project_name") {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "Project settings",
                width = 12,
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                simu_project_setting_module_ui(
                    ns("project_settings"),
                    project_name
                ) %>% 
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
                simu_model_setting_module_ui(ns("model_settings"))
            )
        ),
        fluidRow(
            box(
                title = "Genetic data",
                width = 12,
                status = "warning", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                simu_genetic_spec_module_ui(ns("genetic_spec"))
            )
        )
        ,
        fluidRow(
            box(
                title = "Project action",
                width = 12,
                status = "danger", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE,
                simu_project_action_module_ui(ns("project_action"))
            )
        )
    )
}

#' Simulation module server
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
simu_module_server <- function(input, output, session) {
    # init local reactive values
    local <- reactiveValues()
    # project settings
    callModule(simu_project_setting_module_server,
               "project_settings")
    # model setting
    callModule(simu_model_setting_module_server,
               "model_settings")
    # genetic spec
    callModule(simu_genetic_spec_module_server,
               "genetic_spec")
    # project actions
    local$action <- callModule(simu_project_action_module_server, 
                               "project_action")
    
    # data_info <- reactiveValues()
    # 
    # observe({
    #     data_info$type <- input$type
    # })
    # 
    # callModule(project_input_module_server, "project")
    # callModule(dir_input_module_server, "directory")
    # 
    # 
    # callModule(genetic_data_module, "genetic_data_simu", data_info = data_info)
}
