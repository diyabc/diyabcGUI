#' Analysis module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyWidgets actionGroupButtons
analysis_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        sidebarPanel(
            project_input_module_ui(ns("project"), label = "Project", 
                                    default = "project_name") %>% 
                helper(type = "markdown", 
                       content = "analysis_project"),
            dir_input_module_ui(ns("directory"), label = "Folder"),
            hr(),
            data_input_file_module_ui(ns("data_file"), 
                                      label = "Data input file")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Historical model",
                    hist_model_choice_module_ui(ns("hist_model_choice"))
                ),
                tabPanel(
                    "Summary statistics",
                    "WORK-IN-PROGRESS"
                )
            )
        )
    )
}

#' Analysis module server
#' @keywords internal
#' @author Ghislain Durif
analysis_module_server <- function(input, output, session) {
    
    context <- reactiveValues(
        scenarii = init_hist_model_choice_module_context()
    )
    
    callModule(project_input_module_server, "project")
    callModule(dir_input_module_server, "directory")
    callModule(data_input_file_module_server, "data_file")
    callModule(hist_model_choice_module_server, "hist_model_choice", context)
}
