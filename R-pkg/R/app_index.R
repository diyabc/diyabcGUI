#' Index module input function
#' @keywords internal
#' @author Ghislain Durif
index_ui <- function(id, label = "index") {
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
            simu_ui(ns("simu"))
        ),
        tabPanel(
            "Data analysis", 
            analysis_ui(ns("anaysis"))
        )
    )
}

#' Index module server function
#' @keywords internal
#' @author Ghislain Durif
index_module <- function(input, output, session) {
    callModule(simu_module, "simu")
    callModule(analysis_module, "analysis")
}