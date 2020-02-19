#' Index module input function
#' @keywords internal
#' @author Ghislain Durif
index_ui <- function(id, label = "index") {
    ns <- NS(id)
    navbarPage("diyABC", id = ns("navbar_index"),
        tabPanel("Home", "Home"),
        tabPanel("Simulations", simu_ui(ns("simu"))),
        tabPanel("Analysis", analysis_ui(ns("anaysis"))))
}

#' Index module server function
#' @keywords internal
#' @author Ghislain Durif
index_module <- function(input, output, session) {
    callModule(simu_module, "simu")
    callModule(analysis_module, "analysis")
}