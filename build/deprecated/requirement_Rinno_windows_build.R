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
    "httr", 
    "jsonlite", 
    "magrittr", 
    "RInno", 
    "stringr"
)

install.packages(pkg_list)

## RInno setup
# see https://github.com/ficonsulting/RInno

# load library
library(RInno)
# Use RInno to get Inno Setup
install_inno()