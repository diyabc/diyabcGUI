# generate standalone interface for Windows/MacOS

# default repos
local({
    r <- getOption("repos")
    r["CRAN"] <- "https://cran.r-project.org"
    options(repos=r)
})

# requirement (see `prepare_build.R`)
library(devtools)
library(fs)
library(electricShine)

# update diyabc/abcranger bin
source("update_bin_release.R")

# install diyabcGUI
devtools::install("R-pkg", upgrade = "never")

# get bin files
library(diyabcGUI)
diyabcGUI::dl_all_latest_bin()

# standalone build path
build_path <- file.path(getwd(), "build")

# prepare build path
if(!dir.exists(build_path))
    fs::dir_create(build_path)
if(dir.exists(file.path(build_path, "DIYABC-RF")))
    fs::dir_delete(file.path(build_path, "DIYABC-RF"))

# create standalone app
electricShine::electrify(
    app_name = "DIYABC-RF",
    short_description = "DIYABC-RF application",
    semantic_version = "1.0.0",
    build_path = build_path,
    cran_like_url = "https://cran.r-project.org",
    function_name = "standalone_run_app",
    local_package_path = file.path(getwd(), "R-pkg"),
    package_install_opts = list(type = "binary"),
    run_build = TRUE,
    permission = TRUE,
    mac_url = "https://mac.r-project.org/high-sierra/R-4.0-branch/x86_64/R-4.0-branch.tar.gz"
)
