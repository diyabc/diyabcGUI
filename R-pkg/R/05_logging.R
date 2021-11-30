#' Enable logging during app run
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Enable specific logging of errors, messages and warnings.
#' Append log message to console output and to a log file.
#' @param log_file character string, filename where to write log messages.
#' @export
enable_logging <- function(log_file) {
    log_appender(appender_tee(log_file))
    log_info("log file: ", log_file)
    log_errors()
    log_messages()
    log_warnings()
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
