# generate standalone interface for Windows

# default repos
local({
    r <- getOption("repos")
    r["CRAN"] <- "https://cran.r-project.org"
    options(repos=r)
})

# requirement
library(devtools) # install.packages("devtools")
library(gtools) # install.packages("gtools")
library(RInno) # install.packages("RInno")

# install diyabcGUI
devtools::install("R-pkg")

# dependencies
dep <- c(
    "dplyr",
    "fs",
    "ggplot2",
    "lubridate", 
    "magrittr", 
    "parallel", "processx",
    "shiny", "shinybusy", "shinydashboard", "shinyFiles", 
    "shinyhelper", "shinyjs", "shinyWidgets", 
    "stringr", 
    "tibble",
    "tools"
)

# see https://github.com/ficonsulting/RInno

# load library
library(RInno)
# Use RInno to get Inno Setup
install_inno()

# standalone build path
build_path <- file.path(getwd(), "build", "windows")

# prepare build path
if(!dir.exists(build_path))
    fs::dir_create(build_path, recursive = TRUE)

# Build an installer
create_app(
    app_name = "diyabcGUI", 
    app_dir = "R-pkg/inst/application",
    app_icon = "coccicon.png",
    pkgs = c(gtools::getDependencies("diyabcGUI"),  "diyabcGUI"),
    dir_out = build_path,
    include_R = TRUE
)
compile_iss()

