#' Analysis module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard box
#' @importFrom shinyWidgets actionGroupButtons
analysis_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "Project",
                width = 12,
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                textInput("test", label = "test")
            )
        )
        # fluidRow(
        #     box(
        #         title = "Historical model",
        #         width = 12, 
        #         status = "info", solidHeader = TRUE,
        #         collapsible = TRUE, collapsed = TRUE,
        #         fluidRow(
        #             box(
        #                 title = "test2",
        #                 textInput("test2", label = "test")
        #             ),
        #             box(
        #                 title = "test3",
        #                 textInput("test3", label = "test")
        #             )
        #         )
        #     )
        # )
    )
}




analysis_module_ui_old <- function(id) {
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
    
    # context <- reactiveValues(
    #     scenarii = init_hist_model_choice_module_context()
    # )
    # 
    # callModule(project_input_module_server, "project")
    # callModule(dir_input_module_server, "directory")
    # callModule(data_input_file_module_server, "data_file")
    # callModule(hist_model_choice_module_server, "hist_model_choice", context)
}
