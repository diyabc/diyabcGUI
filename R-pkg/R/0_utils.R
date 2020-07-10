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
    os_id <- switch(
        os,
        "linux"  = "linux",
        "darwin" = "macos",
        "windows" = "windows"
    )
    # binary file
    bin_name <- str_c(bin_name, os_id, sep = "-")
    # check if bin file exists
    if(!any(str_detect(list.files(path), bin_name))) {
        stop(str_c("Missing", bin_file, "binary file", sep = " "))
    }
    # find latest binary file
    bin_candidates <- str_extract(list.files(path), bin_name)
    bin_candidates <- bin_candidates[!is.na(bin_candidates)]
    latest_bin <- which.max(
        file.info(file.path(path, bin_candidates))$mtime
    )
    bin_file <- file.path(path, bin_candidates[latest_bin])
    # output
    return(bin_file)
}

#' Download latest diyabcGUI related binary files
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
    
    # download release
    out <- lapply(
        release$browser_download_url, 
        function(single_url) {
            download.file(
                single_url, 
                destfile = file.path(path, basename(single_url))
            )
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
