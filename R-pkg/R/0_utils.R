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
    os <- str_extract(string = R.version$os, 
                      pattern = "mingw32|windows|darwin|linux")
    
    if(is.na(os)) {
        stop(str_c("Issue with os:", os, "not supported.", sep = " "))
    }
    
    os_id <- switch(
        os,
        "linux"  = "linux",
        "darwin" = "macos",
        "mingw32" = "windows",
        "windows" = "windows",
    )
    # binary file
    bin_name <- str_c(bin_name, os_id, sep = "-")
    # check if bin file exists
    if(!any(str_detect(list.files(path), bin_name))) {
        stop(str_c("Missing", bin_name, "binary file", sep = " "))
    }
    # find latest binary file
    bin_candidates <- list.files(path)[str_detect(list.files(path), bin_name)]
    if(length(bin_candidates) == 0) {
        stop(str_c("Missing", bin_name, "binary file", sep = " "))
    }
    latest_bin <- which.max(
        file.info(file.path(path, bin_candidates))$mtime
    )
    bin_file <- file.path(path, bin_candidates[latest_bin])
    # output
    return(bin_file)
}

#' Download latest diyabcGUI related binary files if missing
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom fs file_chmod
#' @importFrom jsonlite fromJSON
#' @export
dl_latest_bin <- function(prog = "diyabc") {
    # check
    if(!prog %in% c("diyabc", "abcranger")) {
        stop("Wrong 'prog' input argument")
    }
    
    # bin directory
    path <- bin_dir()
    
    # release url
    release_url <- str_c(
        "https://api.github.com/repos/diyabc", 
        prog,
        "releases/latest",
        sep = "/"
    )
    # check latest release info
    release_info <- fromJSON(release_url)
    
    # get latest release list
    release <- release_info$assets
    
    # already existing binary file
    existing_bin_files <- list.files(path)
    
    # download release
    out <- lapply(
        release$browser_download_url, 
        function(single_url) {
            out <- 0
            bin_name <- basename(single_url)
            if(!bin_name %in% existing_bin_files) {
                out <- download.file(
                    single_url, 
                    destfile = file.path(path, bin_name)
                )
            } else {
                warning(str_c(bin_name, "is already the latest release",
                              sep = " "))
            }
            return(out)
        }
    )
    if(!all(out == 0)) {
        stop("Issue with download")
    }
    
    # set up rights
    bin_files <- list.files(file.path(path))
    bin_files <- bin_files[str_detect(bin_files, prog)]
    fs::file_chmod(file.path(path, bin_files), "a+rx")
}

#' Download all latest diyabcGUI related binary files if missing
#' @keywords internal
#' @author Ghislain Durif
#' @export
dl_all_latest_bin <- function() {
    dl_latest_bin("diyabc")
    dl_latest_bin("abcranger")
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
set_diyabcGUI_options <- function(ncore = parallel::detectCores()/2,
                                  simu_loop_size = 100) {
    # set up package options
    diyabcGUI_options <- lst(ncore, simu_loop_size)
    options("diyabcGUI" = diyabcGUI_options)
}
