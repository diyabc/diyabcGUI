#' Simulation ui
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
simu_ui <- function() {
    tagList(
        sidebarPanel(
            text_input("project", label = "Project", default = "my_project"),
            dir_input("directory", label = "Directory"),
            radioButtons("type", label = "Simulation type",
                         choices = list("MicroSAT/sequence" = 1, "SNP" = 2), 
                         selected = 1)
        ),
        mainPanel(
            hist_model_ui()
        )
    )
}

#' Simulation server
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
simu_server <- function(input, output, session) {
    callModule(text_module, "project")
    callModule(dir_module, "directory")
    # callModule(text_area_module, "scenario")
    
    hist_model_server(input, output, session)
}