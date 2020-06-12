.onLoad <- function(libname, pkgname) {
    set_diyabcGUI_options(ncore = parallel::detectCores()/2)
}