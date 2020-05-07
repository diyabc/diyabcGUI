#' find bin directory
#' @keywords internal
#' @author Ghislain Durif
bin_dir <- function() {
    pkgdir <- find.package("diyabcGUI")
    bindir <- file.path(pkgdir, "bin")
    if(!dir.exists(bindir)) {
        binpdir <- file.path(pkgdir, "inst", "help")
        if(!dir.exists(binpdir)) {
            stop("bin directory not found")
        }
    }
    return(binpdir)
}

#' find help directory
#' @keywords internal
#' @author Ghislain Durif
help_dir <- function() {
    pkgdir <- find.package("diyabcGUI")
    helpdir <- file.path(pkgdir, "help")
    if(!dir.exists(helpdir)) {
        helpdir <- file.path(pkgdir, "inst", "help")
        if(!dir.exists(helpdir)) {
            stop("Help directory not found")
        }
    }
    return(helpdir)
}
