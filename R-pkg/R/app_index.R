#' Index module ui
#' @keywords internal
#' @author Ghislain Durif
index_module_ui <- function(id, label = "index") {
    ns <- NS(id)
    navbarPage(
        "diyABC", id = ns("navbar_index"),
        tabPanel(
            "Home", 
            home_module_ui(ns("home"))
        ),
        tabPanel(
            "Data simulations", 
            simu_module_ui(ns("simu"))
        ),
        tabPanel(
            "Data analysis",
            analysis_module_ui(ns("analysis"))
        )
    )
}

#' Index module server
#' @keywords internal
#' @author Ghislain Durif
index_module_server <- function(input, output, session) {
    callModule(home_module_server, "home")
    callModule(simu_module_server, "simu")
    callModule(analysis_module_server, "analysis")
}