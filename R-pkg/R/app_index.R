diyabc_index <- function() {
    navlistPanel(
            tabPanel("Simulations", diyabc_simu()),
            tabPanel("Analysis", diyabc_analysis()))
}