# generate standalone interface for Windows/MacOS

# project directory
proj_dir <- system("git rev-parse --show-toplevel", intern = TRUE)
setwd(proj_dir)

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
library(stringr)

# install diyabcGUI
devtools::install(file.path(proj_dir, "R-pkg"), upgrade = "never")

# get bin files
library(diyabcGUI)
diyabcGUI::dl_all_latest_bin()

# standalone build path
build_path <- file.path(proj_dir, "build", "windows")

# app name
app_name <- "DIYABC-RF_GUI"

# prepare build path
if(!dir.exists(build_path))
    fs::dir_create(build_path)
if(dir.exists(file.path(build_path, app_name)))
    fs::dir_delete(file.path(build_path, app_name))

# switch depending on OS
os <- stringr::str_extract(string = R.version$os, 
                           pattern = "mingw32|windows|darwin|linux")

# create standalone app
if(os %in% c("mingw32", "windows")) {
    electricShine::electrify(
        app_name = app_name,
        short_description = "DIYABC-RF GUI application",
        semantic_version = as.character(packageVersion("diyabcGUI")),
        build_path = build_path,
        cran_like_url = "https://cran.r-project.org",
        function_name = "standalone_run_app",
        local_package_path = file.path(proj_dir, "R-pkg"),
        package_install_opts = list(type = "binary"),
        run_build = TRUE,
        permission = TRUE
    )
    
    # rename exe file
    exec_filename <- stringr::str_c(
        "DIYABC-RF_GUI Setup ", 
        as.character(packageVersion("diyabcGUI")), 
        ".exe", sep = ""
    )
    fs::file_copy(
        path = file.path(
            build_path, "DIYABC-RF_GUI", "dist", exec_filename
        ), 
        new_path = file.path(
            build_path, "DIYABC-RF_GUI", "dist", 
            stringr::str_replace_all(exec_filename, " ", "_")
        )
    )
    
} else if(os == "darwin") {
    electricShine::electrify(
        app_name = "DIYABC-RF",
        short_description = "DIYABC-RF application",
        semantic_version = as.character(packageVersion("diyabcGUI")),
        build_path = build_path,
        cran_like_url = "https://cran.r-project.org",
        function_name = "standalone_run_app",
        local_package_path = file.path(getwd(), "R-pkg"),
        package_install_opts = list(type = "binary"),
        run_build = TRUE,
        permission = TRUE,
        nodejs_path = "/usr/local/bin",
        mac_url = "https://mac.r-project.org/high-sierra/R-4.0-branch/x86_64/R-4.0-branch.tar.gz"
    )
    
} else if(os == "linux") {
    warning("Linux not supported at the moment")
}

