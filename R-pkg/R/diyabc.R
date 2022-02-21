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
    # log file
    enable_logging()
    # run app
    shiny::runApp(
        appDir = system.file("application", package = "diyabcGUI")
    )
}

#' Launch DIYABC-RF graphical user interface and force to open web browser
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
browser_diyabc <- function() {
    # log file
    enable_logging()
    # run app
    shiny::runApp(
        appDir = system.file("application", package = "diyabcGUI"),
        launch.browser = TRUE
    )
}

#' Launch DIYABC-RF graphical user interface in debug mode
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
debug_diyabc <- function() {
    # log file
    enable_logging()
    # run app
    options(shiny.error = browser)
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
    # log file
    enable_logging()
    # run app
    shiny::shinyApp(
        ui = diyabcGUI::diyabc_ui(),
        server = diyabcGUI::diyabc_server,
        onStart = diyabcGUI::redirect_output,
        options = options,
    )
}
