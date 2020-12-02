#' Return diyabcGUI related program file name
#' @keywords internal
#' @author Ghislain Durif
prog_name <- function(prog = "diyabc") {
    out <- switch(
        prog,
        "diyabc" = "diyabc-RF",
        "abcranger" = "abcranger",
        stop("Bad input for 'prog' arg.")
    )
    return(out)
}



#' Find diyabcGUI related binary files
#' @keywords internal
#' @author Ghislain Durif
find_bin <- function(prog = "diyabc") {
    # check input
    if(!prog %in% c("diyabc", "abcranger")) {
        stop("Wrong input")
    }
    # binary directory
    path <- bin_dir()
    # platform
    os_id <- get_os()
    # binary file
    bin_name <- str_c(prog_name(prog), os_id, sep = "-")
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
                                                        "diyabc|abcranger|dll")]
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
#' @param prog string, name of the program to download, eligible name are
#' `"diyabc-RF"` and `"abcranger"`.
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
    release <- release_info$assets[,c("name", "browser_download_url")]
    
    # check if file in release
    if(nrow(release) < 1) {
        stop(str_c("Issue with available files at ", release_url, ". ",
                   "Contact DIYABC-RF support.",
                   sep = ""))
    }
    
    # already existing binary file
    existing_bin_files <- list.files(path)
    
    # download release
    out <- lapply(
        split(release, seq(nrow(release))), 
        function(single_file) {
            # output
            #   check:  0 if dl is ok or latest version already here,
            #           1 if dl failed
            #           -1 if no binary files was available
            check <- 1
            bin_name <- single_file$name
            bin_url <- single_file$browser_download_url
            
            # abcranger/diyabc binary files
            if(str_detect(bin_name, str_c(prog_name(prog), "-", 
                                          os_id, sep = ""))) {
                if(!bin_name %in% existing_bin_files) {
                    # avoid blacklisting
                    Sys.sleep(2)
                    # dl
                    check <- download.file(
                        bin_url, 
                        destfile = file.path(path, bin_name),
                        mode = "wb"
                    )
                } else {
                    check <- 0
                    warning(str_c(
                        "The latest release", bin_name, 
                        "was already downloaded.", sep = " "
                    ))
                }
            } else {
                check <- -1
            }
            return(check)
        }
    )
    
    # binary files not available at all among files in latest release
    if(all(out == -1)) {
        warning(str_c("No binary file available for ", prog, " on ", 
                      os_id, ". ",
                      "Contact DIYABC-RF support.",
                      sep = ""))
    }
    
    # no download success among files in latest release
    if(!any(out == 0)) {
        stop("Issue with download")
    }
    
    # zip extraction for diyabc on Windows
    zip_files <- list.files(path, pattern = "\\.zip$")
    if(length(zip_files) > 0) {
        latest_zip <- which.max(file.info(file.path(path, zip_files))$mtime)
        tmp <- unzip(file.path(path, zip_files[latest_zip]), exdir = path)
        if(length(tmp) == 0) {
            stop(str_c("Issue when unzipping ", zip_files[latest_zip]))
        }
        fs::file_delete(file.path(path, zip_files))
    }
    
    # set up rights
    bin_files <- list.files(path, pattern = prog)
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

#' Custom print
#' @keywords internal
#' @author Ghislain Durif
pprint <- function(...) {
    # message(as.character(...))
    print(...)
}

#' Reset sink (console output redirection)
#' @keywords internal
#' @author Ghislain Durif
reset_sink <- function() {
    if(sink.number() > 0) {
        sink(type = "message")
        sink()
    }
}

#' Logging function for debugging
#' @keywords internal
#' @author Ghislain Durif
logging <- function(...) {
    if(getOption("diyabcGUI")$verbose)
        pprint(str_c(..., sep = " ", collapse = " "))
}

#' Enable logging verbosity
#' @keywords internal
#' @author Ghislain Durif
enable_logging <- function() {
    # current option status
    diyabcGUI_options <- getOption("diyabcGUI")
    # enable logging
    diyabcGUI_options$verbose <- TRUE
    # save change
    options("diyabcGUI" = diyabcGUI_options)
}

#' Disable logging verbosity
#' @keywords internal
#' @author Ghislain Durif
disable_logging <- function() {
    # current option status
    diyabcGUI_options <- getOption("diyabcGUI")
    # enable logging
    diyabcGUI_options$verbose <- FALSE
    # save change
    options("diyabcGUI" = diyabcGUI_options)
}

#' Set up diyabcGUI options
#' @keywords internal
#' @author Ghislain Durif
#' @param ncore integer, number of cores to used for parallel computations, 
#' default is half available cores.
#' @param simu_loop_size integer, batch size for simulation loop, default 
#' is 100.
#' @param image_ext string, possible ggplot extensions among `"eps"`, `"ps"`, 
#' `"tex"`, `"pdf"`, `"jpeg"`, `"tiff"`, `"png"`, `"bmp"`, `"svg"`
#' @param verbose boolean, enable/disable logging verbosity, default is FALSE.
set_diyabcGUI_options <- function(ncore = parallel::detectCores()/2,
                                  simu_loop_size = 100, 
                                  image_ext = "png",
                                  verbose = FALSE) {
    # cast
    ncore <- as.integer(ncore)
    simu_loop_size <- as.integer(simu_loop_size)
    image_ext <- as.character(image_ext)
    # set up package options
    diyabcGUI_options <- lst(ncore, simu_loop_size, image_ext, verbose)
    options("diyabcGUI" = diyabcGUI_options)
}

#' Get current diyabcGUI option states
#' @keywords internal
#' @author Ghislain Durif
get_option <- function(option_name) {
    # current option state
    diyabcGUI_options <- getOption("diyabcGUI")
    # check input
    if(! option_name %in% names(diyabcGUI_options))
        stop("Bad input for 'option_name' arg")
    # output
    return(diyabcGUI_options[[ option_name ]])
}
