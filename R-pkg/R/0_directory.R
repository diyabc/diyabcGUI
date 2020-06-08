#' find bin directory
#' @keywords internal
#' @author Ghislain Durif
bin_dir <- function() {
    pkgdir <- find.package("diyabcGUI")
    bindir <- file.path(pkgdir, "bin")
    if(!dir.exists(bindir)) {
        bindir <- file.path(pkgdir, "inst", "bin")
        if(!dir.exists(bindir)) {
            stop("bin directory not found")
        }
    }
    return(bindir)
}

#' find example directory
#' @keywords internal
#' @author Ghislain Durif
example_dir <- function() {
    pkgdir <- find.package("diyabcGUI")
    exampledir <- file.path(pkgdir, "example")
    if(!dir.exists(exampledir)) {
        exampledir <- file.path(pkgdir, "inst", "example")
        if(!dir.exists(exampledir)) {
            stop("example directory not found")
        }
    }
    return(exampledir)
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
