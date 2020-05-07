#' Logging function for debugging
#' @keywords internal
#' @author Ghislain Durif
logging <- function(...) {
    print(str_c(..., sep = " "))
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
