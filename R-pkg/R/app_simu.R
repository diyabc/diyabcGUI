#' Historical model
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
hist_model <- function(add=FALSE) {
    tagList()
}

#' Simulation ui
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
simu_ui <- function() {
    tagList(
        text_input("project", label = "Project name", default = "my_project"),
        dir_input("directory", label = "Directory")
    )
}

#' Simulation server
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
simu_server <- function(input, output) {
    callModule(text_module, "project")
    callModule(dir_module, "directory")
}