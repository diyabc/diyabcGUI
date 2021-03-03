env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
    # set up options
    set_diyabcGUI_options(ncore = parallel::detectCores())
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
    # setup package global environment
    init_diyabc_env()
}

# .onAttach <- function() {
#     # check if binary files are available
#     diyabc_bin <- tryCatch(
#         find_bin("diyabc"),
#         error = function(e) return(e)
#     )
#     abcranger_bin <- tryCatch(
#         find_bin("abcranger"),
#         error = function(e) return(e)
#     )
#     if("error" %in% class(diyabc_bin) | "error" %in% class(abcranger_bin)) {
#         packageStartupMessage(
#             "Warning: ",
#             "Missing binary file(s), please run ",
#             "'diyabcGUI::dl_all_latest_bin()'"
#         )
#     }
# }