# generate standalone interface for Windows

## RInno setup
# see https://github.com/ficonsulting/RInno

# project directory
proj_dir <- system("git rev-parse --show-toplevel", intern = TRUE)
setwd(proj_dir)

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
library(RInno)
library(stringr)

# install diyabcGUI
devtools::install(file.path(proj_dir, "R-pkg"))
library(diyabcGUI)
dl_all_latest_bin()

# dependencies
Dependencies <- gtools::getDependencies(
    "diyabcGUI", 
    dependencies = c("Depends", "Imports", "LinkingTo")
)

# version
Version <- as.character(packageVersion("diyabcGUI"))

# standalone build path
build_path <- file.path(proj_dir, "build", "windows")

# prepare build path
if(!dir.exists(build_path))
    fs::dir_create(build_path, recursive = TRUE)

# Build an installer
create_app(
    app_name = str_c("DIYABC-RF_GUI_", Version),
    app_dir = file.path(proj_dir, "R-pkg", "inst", "application"),
    app_icon = file.path(proj_dir, "icon", "coccicon.png"),
    pkgs = Dependencies,
    remotes = c("diyabc/diyabcGUI"),
    dir_out = build_path,
    include_R = TRUE
)
compile_iss()

