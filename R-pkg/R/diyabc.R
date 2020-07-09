#' Launch diyabc graphical user interface (diyabcGUI)
#' @description
#' FIXME
#' @details
#' FIXME
#' @author Ghislain Durif
#' @return shiny application object
#' @examples
#' \dontrun{
#' diyabc()
#' }
#' @export
diyabc <- function(options = list()) {
    shiny::runApp(
        appDir = system.file("application", package = "diyabcGUI"),
        options = options
    )
}