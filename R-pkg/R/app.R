#' Launch diyabc graphical user interface (diyabcGUI)
#' @description
#' FIXME
#' @details
#' FIXME
#' @author Ghislain Durif
#' @return shiny application object
#' @importFrom shiny shinyApp
#' @example \dontrun {launchApp()}
#' @export
diyabc <- function() {
    shinyApp(ui = diyabc_ui, server = diyabc_server)
}