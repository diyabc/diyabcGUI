#' Analysis module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard box
#' @importFrom shinyWidgets actionGroupButtons
analysis_module_ui <- function(id, project_name = "project_name") {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "Project settings",
                width = 12,
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                analysis_project_setting_module_ui(
                    ns("project_settings"),
                    project_name
                ) %>% 
                    helper(type = "markdown", 
                           content = "analysis_project")
            )
        ),
        fluidRow(
            box(
                title = "Training set simulation",
                width = 12,
                status = "info", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                trainset_simu_module_ui(ns("trainset_simu"))
            )
        ),
        fluidRow(
            box(
                title = "Random Forest Analysis",
                width = 12,
                status = "warning", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                rf_module_ui(ns("rf_analysis"))
            )
        )
    )
}

#' Analysis module server
#' @keywords internal
#' @author Ghislain Durif
analysis_module_server <- function(input, output, session, 
                                   project_name = "project_name", 
                                   project_dir = NULL,
                                   data_dir = NULL) {
    
    
    callModule(analysis_project_setting_module_server, 
               "project_settings",
               project_name = project_name, 
               project_dir = project_dir)
    
    callModule(trainset_simu_module_server, "trainset_simu")
    callModule(rf_module_server, "rf_analysis")

    # context <- reactiveValues(
    #     scenarii = init_hist_model_choice_module_context()
    # )
    # 
    # callModule(project_input_module_server, "project")
    # callModule(dir_input_module_server, "directory")
    # callModule(data_input_file_module_server, "data_file")
    # callModule(hist_model_choice_module_server, "hist_model_choice", context)
}
