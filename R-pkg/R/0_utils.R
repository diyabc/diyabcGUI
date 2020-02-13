#' return integer regex
#' @keywords internal
#' @author Ghislain Durif
int_regex <- function() {
    return("[0-9]+")
}

#' return event param regex
#' @keywords internal
#' @author Ghislain Durif
param_regex <- function() {
    return("([a-zA-Z_][a-zA-Z0-9_]*|[0-9]+)")
}

#' return event rate regex
#' @keywords internal
#' @author Ghislain Durif
rate_regex <- function() {
    return("([a-zA-Z_][a-zA-Z0-9_]*|[0-9]+\\.?[0-9]*)")
}

#' return event numerical rate regex
#' @keywords internal
#' @author Ghislain Durif
num_rate_regex <- function() {
    return("[0-9]+\\.?[0-9]*")
}

#' return event time regex
#' @keywords internal
#' @author Ghislain Durif
time_regex <- function() {
    return(param_regex())
}

