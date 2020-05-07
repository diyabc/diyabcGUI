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

#' find help directory
#' @keywords internal
#' @author Ghislain Durif
help_dir <- function() {
    pkgdir <- find.package("diyabcGUI")
    helpdir <- file.path(pkgdir, "help")
    if(!dir.exists(helpdir)) {
        helpdir <- file.path(pkgdir, "inst", "help")
        if(!dir.exists(helpdir)) {
            stop("Help directory not found")
        }
    }
    return(helpdir)
}

#' Extract project name from path
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_split
#' @importFrom tibble lst
project_path2name <- function(path) {
    # check input
    is_path <- dir.exists(path)
    # project directory
    project_name <- tail(str_split(path, .Platform$file.sep)[[1]], 1)
    # timestamp ?
    timestamp <- str_detect(project_name, 
                            pattern = "_[0-9]{4}-[0-9]{2}-[0-9]{2}$")
    # output
    return(lst(is_path, project_name, timestamp))
} 

