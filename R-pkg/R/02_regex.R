#' return integer regex
#' @keywords internal
#' @author Ghislain Durif
int_regex <- function() {
    return("[0-9]+")
}

#' return numerical (float) regex
#' @keywords internal
#' @author Ghislain Durif
num_regex <- function() {
    return("[0-9]+\\.?[0-9]*")
}

#' return numerical (xxEyy notation) regex
#' @keywords internal
#' @author Ghislain Durif
numexp_regex <- function() {
    return("[0-9]+\\.?[0-9]*(E\\-?[0-9]+)?")
}

#' return event numerical rate regex
#' @keywords internal
#' @author Ghislain Durif
num_rate_regex <- function() {
    return("[0-9]+\\.?[0-9]*")
}

#' return event single param regex
#' @keywords internal
#' @author Ghislain Durif
single_param_regex <- function() {
    return("[a-zA-Z_][a-zA-Z0-9_]*")
}

#' return event alphanum param regex
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_c
alphanum_param_regex <- function() {
    return(str_c("(", single_param_regex(), 
                 "([\\+\\-]", single_param_regex(), ")?", ")"))
}

#' return event param regex
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_c
param_regex <- function() {
    return(str_c("(", alphanum_param_regex(), "|", 
                 int_regex(), ")"))
}

#' return event rate regex
#' @keywords internal
#' @author Ghislain Durif
rate_regex <- function() {
    return("([a-zA-Z_][a-zA-Z0-9_]*|[0-9]+\\.?[0-9]*)")
}

#' return event time regex
#' @keywords internal
#' @author Ghislain Durif
time_regex <- function() {
    return(param_regex())
}

#' return event single time regex
#' @keywords internal
#' @author Ghislain Durif
single_time_regex <- function() {
    return(str_c("(", single_param_regex(), "|", 
                 int_regex(), ")"))
}
