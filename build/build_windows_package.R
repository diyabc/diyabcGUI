## build diyabcGUI zip package for Windows

# default repos
local({
    r <- getOption("repos")
    r["CRAN"] <- "https://cran.r-project.org"
    options(repos=r)
})

# requirement
library(devtools)
library(fs)
library(gtools)
library(stringr)

# project directory
proj_dir <- system("git rev-parse --show-toplevel", intern = TRUE)
setwd(proj_dir)

# package source
src_dir <- file.path(proj_dir, "R-pkg")

# build directory
build_dir <- file.path(proj_dir, "build")

# dist directory
dist_dir <- file.path(build_dir, "dist")
if(!dir.exists(dist_dir)) fs::dir_create(dist_dir)

# load diyabcGUI
devtools::load_all(src_dir)

# dependencies
dep <- sort(gtools::getDependencies(
    "diyabcGUI", 
    dependencies = c("Depends", "Imports", "LinkingTo")
))

write.table(
    dep, 
    file = file.path(dist_dir, "requirements.txt"), 
    row.names = FALSE, col.names = FALSE, quote = FALSE
)

# version
Version <- as.character(packageVersion("diyabcGUI"))

# local install of diyabcGUI package
devtools::install(
    pkg = file.path(proj_dir, "R-pkg"),
    upgrade = "never"
)

# # remote install of diyabcGUI package
# devtools::install_github(
#     "diyabc/diyabcGUI",
#     subdir = "R-pkg",
#     ref = "prod",
#     upgrade = "never"
# )

# get latest required bin
library(diyabcGUI)
diyabcGUI::dl_all_latest_bin()

# zip local install of package
zipfile <- file.path(dist_dir, str_c("diyabcGUI_", Version, ".zip"))
if(file.exists(zipfile)) fs::file_delete(zipfile)
setwd(dirname(system.file(package = "diyabcGUI")))
zip(
    zipfile = zipfile, 
    files = basename(system.file(package = "diyabcGUI"))
)
setwd(proj_dir)
