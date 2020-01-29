index_ui <- function() {
    navbarPage("diyABC", id = "navbar_index",
        tabPanel("Home", "Home"),
        tabPanel("Simulations", simu_ui()),
        tabPanel("Analysis", analysis_ui()))
}