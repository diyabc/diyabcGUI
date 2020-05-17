#' Find diyabcGUI related binary files
#' @keywords internal
#' @author Ghislain Durif
find_bin <- function(bin_name = "diyabc") {
    # check input
    if(!bin_name %in% c("diyabc", "abcranger")) {
        stop("Wrong input")
    }
    # binary directory
    path <- bin_dir()
    # platform
    os <- str_extract(string = R.version$os, pattern = "windows|darwin|linux")
    # binary file
    bin_file <- str_c(bin_name, os, sep = "-")
    # check if bin file exists
    if(!bin_file %in% list.files(path)) {
        stop(str_c("Missing", bin_file, "binary file", sep = " "))
    }
    # output
    return(file.path(path, bin_file))
}

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
