## local .Rprofile

# default repos
local({
    r <- getOption("repos")
    r["CRAN"] <- "https://cran.r-project.org"
    options(repos=r)
})

# install package list
install_pkg <- function(pkg_list, force_update = TRUE) {
    # current installed packages
    cur_pkg_list <- installed.packages()[,1]
    # current out-dated packages
    old_pkg_list <- old.packages()[,1]
    # install missing or out-dated packages from pkg_list
    tmp <- sapply(
        pkg_list, 
        function(pkg) {
            message("---------------------------------")
            message(paste("Package", pkg))
            message(paste("- available :", pkg %in% cur_pkg_list))
            message(paste("- update needed :", pkg %in% old_pkg_list))
            if(!(pkg %in% cur_pkg_list) || 
                    ((pkg %in% old_pkg_list) && force_update)) {
                message(paste("--> installing", pkg))
                install.packages(pkg)
            }
            message("---------------------------------")
        }
    )
}

## path to local directories

# project directory
proj_dir <- system("git rev-parse --show-toplevel", intern = TRUE)

# package source
src_dir <- file.path(proj_dir, "R-pkg")

# build directory
build_dir <- file.path(proj_dir, "build")

# dist directory
dist_dir <- file.path(build_dir, "dist")
