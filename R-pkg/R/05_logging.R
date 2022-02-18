#' Enable logging during app run
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Enable specific logging of errors, messages and warnings.
#' Append log message to console output and to a log file.
#' 
#' Issue with log_messages/log_warnings/log_errors being enabled mutliple 
#' times (see <https://github.com/daroczig/logger/issues/88>)
#' @export
enable_logging <- function() {
    log_appender(appender_tee(log_file()))
    log_info("log file: ", log_file())
    
    log_enable <- is_log_enabled()
    
    if(!log_enable) {
        # temp fix for https://github.com/daroczig/logger/issues/88
        log_errors()
        log_messages()
        log_warnings()
        diyabc_options <- getOption("diyabcGUI")
        diyabc_options$enable_log <- TRUE
        options(diyabcGUI = diyabc_options)
    }
}

#' Get log file from diyabc options
#' @keywords internal
#' @author Ghislain Durif
log_file <- function() {
    return(get_option("log_file"))
}

#' Check if log is already enabled from diyabc options
#' @keywords internal
#' @author Ghislain Durif
is_log_enabled <- function() {
    return(exists("enable_log", getOption("diyabcGUI")))
}

#' Enable logging during app run
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' The possible log level are the one from the `logger` package, i.e.
#' `"TRACE"`, `"DEBUG"`, `"INFO"`, `"SUCCESS"`, `"WARN"`, `"ERROR"`, `"FATAL"`.
#' See [logger::log_levels].
#' @param level character string, log message level (see description).
#' @export
logging_level <- function(level) {
    log_threshold(str_to_upper(level))
}
