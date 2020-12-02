## build diyabcGUI zip package for Windows

# project directory
proj_dir <- system("git rev-parse --show-toplevel", intern = TRUE)
setwd(proj_dir)

# package source
src_dir <- file.path(proj_dir, "R-pkg")

# build directory
build_dir <- file.path(proj_dir, "build")

# local R_lib folder
R_lib <- file.path(build_dir, "build")

# default repos
local({
    r <- getOption("repos")
    r["CRAN"] <- "https://cran.r-project.org"
    options(repos=r)
})

# requirement
library(devtools)
library(gtools)
library(stringr)

# load diyabcGUI
devtools::load_all(src_dir)

# dependencies
dep <- gtools::getDependencies(
    "diyabcGUI", 
    dependencies = c("Depends", "Imports", "LinkingTo")
)

# version
Version <- as.character(packageVersion("diyabcGUI"))

# local install of dependencies
install.packages(dep, lib = R_lib)

# local install of package
devtools::install(
    pkg = file.path(proj_dir, "R-pkg"),
    upgrade = "never",
    lib = R_lib
)

# # remote install of package
# devtools::install_github(
#     "diyabc/diyabcGUI",
#     subdir = "R-pkg",
#     ref = "prod",
#     upgrade = "never",
#     lib = R_lib
# )

# zip local install of package
zip(
    zipfile = file.path(build_dir, str_c("diyabcGUI_", Version, ".zip")), 
    files = file.path(R_lib, "diyabcGUI")
)