#' Launch diyabc graphical user interface (diyabcGUI)
#' @description
#' FIXME
#' @details
#' FIXME
#' @author Ghislain Durif
#' @return shiny application object
#' @import shiny
#' @examples
#' \dontrun{
#' diyabc()
#' }
#' @export
diyabc <- function() {
    shiny::runApp(appDir = system.file("application", package = "diyabcGUI"))
}