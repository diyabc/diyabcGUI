#' Analysis project setting module ui
#' @keywords internal
#' @author Ghislain Durif
analysis_project_setting_module_ui <- 
    function(id, project_name = "project_name") {
    ns <- NS(id)
    tagList(
        project_input_module_ui(
            ns("project_name"), 
            label = "Project", 
            default = project_name
        ),
        dir_input_module_ui(
            ns("project_dir"),
            label = "Folder"
        ),
        hr(),
        data_input_file_module_ui(
            ns("data_file"), 
            label = "Data input file"
        )
    )
}

#' Analysis project setting module server function
#' @keywords internal
#' @author Ghislain Durif
analysis_project_setting_module_server <- 
    function(input, output, session, project_name = NULL, project_dir = NULL,
             existing_model = FALSE) {
    callModule(project_input_module_server, "project_name")
    callModule(dir_input_module_server, "project_dir")
    callModule(data_input_file_module_server, "data_file")
}
