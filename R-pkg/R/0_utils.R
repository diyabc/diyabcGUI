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
    os_id <- get_os()
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

#' Clean binary directory
#' @keywords internal
#' @author Ghislain Durif
clean_bin_dir <- function() {
    # bin directory
    path <- bin_dir()
    # existing binary file
    existing_bin_files <- list.files(path)
    existing_bin_files <- existing_bin_files[str_detect(existing_bin_files, 
                                                        "diyabc|abcranger")]
    # delete diyabc/abcranger files
    if(length(existing_bin_files) > 0) {
        fs::file_delete(file.path(path, existing_bin_files))
    }
}

#' Find which OS is running
#' @keywords internal
#' @author Ghislain Durif
get_os <- function() {
    # get OS id given by R
    os_id <- str_extract(string = R.version$os, 
                         pattern = "mingw32|windows|darwin|linux")
    # check if error
    if(is.na(os_id)) {
        stop(str_c("Issue with os:", os_id, "not supported.", sep = " "))
    }
    # return OS name
    os_name <- switch(
        os_id,
        "linux"  = "linux",
        "darwin" = "macos",
        "mingw32" = "windows",
        "windows" = "windows",
    )
    return(os_name)
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
    
    # platform
    os_id <- get_os()
    
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
            check <- 1
            bin_name <- basename(single_url)
            if(str_detect(bin_name, os_id)) {
                if(!bin_name %in% existing_bin_files) {
                    check <- download.file(
                        single_url, 
                        destfile = file.path(path, bin_name),
                        mode = "wb"
                    )
                } else {
                    check <- 0
                    warning(str_c(bin_name, "is already the latest release",
                                  sep = " "))
                }
            }
            return(check)
        }
    )
    if(!any(out == 0)) {
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
