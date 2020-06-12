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

#' Set up diyabcGUI options
#' @keywords internal
#' @author Ghislain Durif
set_diyabcGUI_options <- function(ncore = parallel::detectCores()/2) {
    # set up package options
    diyabcGUI_options <- lst(ncore)
    options("diyabcGUI" = diyabcGUI_options)
}
