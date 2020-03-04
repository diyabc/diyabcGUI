#' Analysis module ui
#' @keywords internal
#' @author Ghislain Durif
analysis_module_ui <- function(id, label = "analysis") {
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
                    "scenarii/parameters"
                ),
                tabPanel(
                    "Summary statistics",
                    "TODO"
                )
            )
        )
    )
}

#' Analysis module server
#' @keywords internal
#' @author Ghislain Durif
analysis_module_server <- function(input, output, session) {
    
    callModule(project_input_module_server, "project")
    callModule(dir_input_module_server, "directory")
    callModule(data_input_file_module_server, "data_file")
    
}