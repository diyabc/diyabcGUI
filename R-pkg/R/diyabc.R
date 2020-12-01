#' Launch DIYABC-RF graphical user interface
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
diyabc <- function() {
    shiny::runApp(
        appDir = system.file("application", package = "diyabcGUI")
    )
}

#' Launch DIYABC-RF graphical user interface for standalone app
#' @description
#' FIXME
#' @details
#' FIXME
#' @author Ghislain Durif
#' @export
standalone_run_app <- function(options = list()) {
    con <- file(file.path(dirname(tempdir()), "DIYABC-RF_GUI.log"))
    sink(con, append=TRUE)
    sink(con, append=TRUE, type="message")
    on.exit(reset_sink())
    
    shiny::shinyApp(
        ui = diyabc_ui(),
        server = diyabc_server,
        options = options
    ) 
}
