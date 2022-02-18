.onLoad <- function(libname, pkgname) {
    # setup package global environment
    assign("env", new.env(parent = emptyenv()), .GlobalEnv)
    
    # setup options
    set_diyabcGUI_options()
    
    # log file
    enable_logging()
    logging_level("trace")
    
    # init environment
    init_diyabc_env()
    
    # check if binary files are available
    diyabc_bin <- tryCatch(
        find_bin("diyabc"),
        error = function(e) return(e)
    )
    abcranger_bin <- tryCatch(
        find_bin("abcranger"),
        error = function(e) return(e)
    )
    if("error" %in% class(diyabc_bin) | "error" %in% class(abcranger_bin)) {
        warning(
            "Warning: ",
            "Missing binary file(s), please run ",
            "'diyabcGUI::dl_all_latest_bin()'"
        )
    }
}
