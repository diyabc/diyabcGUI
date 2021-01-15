#' Run data generation module as an external shiny app for debugging
#' @keywords internal
#' @author Ghislain Durif
datagen_debug_app <- function() {
    shiny::shinyApp(
        ui = dashboardPage(
            dashboardHeader(),
            dashboardSidebar(),
            dashboardBody(
                datagen_page_ui("datagen_testing")
            )
        ),
        server = function(input, output, session) {
            callModule(
                datagen_page_server, "datagen_testing"
            )
        }
    )
}