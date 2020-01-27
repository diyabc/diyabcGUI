diyabc_index <- function() {
    navbarPage("",
        tabPanel("Home", "Home"),
        tabPanel("Simulations", diyabc_simu()),
        tabPanel("Analysis", diyabc_analysis()))
}