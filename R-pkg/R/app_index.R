#' Index module ui
#' @keywords internal
#' @author Ghislain Durif
index_module_ui <- function(id, label = "index") {
    ns <- NS(id)
    navbarPage("diyABC", id = ns("navbar_index"),
        tabPanel(
            "Home", 
            tagList(
                includeMarkdown(
                    file.path(help_dir(), "data_simulation.md")
                ),
                includeMarkdown(
                    file.path(help_dir(), "data_analysis.md")
                )
            )
        ),
        tabPanel(
            "Data simulations", 
            simu_module_ui(ns("simu"))
        ),
        tabPanel(
            "Data analysis", 
            analysis_module_ui(ns("anaysis"))
        )
    )
}

#' Index module server
#' @keywords internal
#' @author Ghislain Durif
index_module_server <- function(input, output, session) {
    callModule(simu_module_server, "simu")
    callModule(analysis_module_server, "analysis")
}