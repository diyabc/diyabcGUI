#' Run data generation module as an external shiny app for debugging
#' @keywords internal
#' @author Ghislain Durif
datagen_debug_app <- function() {
    shiny::shinyApp(
        ui = fluidPage(
            datagen_ui("datagen_testing")
        ),
        server = function(input, output, session) {
            callModule(
                datagen_server, "datagen_testing"
            )
        }
    )
}