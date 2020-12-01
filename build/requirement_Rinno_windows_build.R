# default repos
local({
    r <- getOption("repos")
    r["CRAN"] <- "https://cran.r-project.org"
    options(repos=r)
})

# install package for RInno standalone build for windows
pkg_list <- c(
    "devtools", 
    "fs", 
    "gtools", 
    "RInno", 
    "stringr"
)

install.packages(pkg_list)