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
    shinyApp(ui = diyabc_ui, server = diyabc_server)
}