#' Home module ui
#' @keywords internal
#' @author Ghislain Durif
home_module_ui <- function(id, label = "home") {
    ns <- NS(id)
    tagList(
        includeMarkdown(
            file.path(help_dir(), "data_simulation.md")
        ),
        includeMarkdown(
            file.path(help_dir(), "data_analysis.md")
        )
    )
}

#' Home module server
#' @keywords internal
#' @author Ghislain Durif
home_module_server <- function(input, output, session) {}
