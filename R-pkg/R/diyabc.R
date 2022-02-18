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
    shiny::shinyApp(
        ui = diyabcGUI::diyabc_ui(),
        server = diyabcGUI::diyabc_server,
        onStart = diyabcGUI::redirect_output,
        options = options,
    )
}


#' Launch shiny console output to a file
#' @description
#' FIXME
#' @details
#' FIXME
#' @author Ghislain Durif
#' @importFrom lubridate now
#' @export
redirect_output <- function() {
    logfile <- file.path(
        dirname(tempdir()), 
        str_c(
            "DIYABC-RF_GUI", 
            str_replace_all(lubridate::now(), pattern = " ", replacement = "_"),
            ".log"
        )
    )
    pprint(str_c("log file: ", logfile))
    con <- file(logfile)
    sink(con, append=TRUE)
    sink(con, append=TRUE, type="message")
    
    shiny::onStop(function() {
        reset_sink()
        print(str_c("interactive ? ", interactive()))
        if(!interactive()) {
            q("no")
        }
    })
}
