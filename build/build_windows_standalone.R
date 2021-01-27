## build DIYABC-RF_GUI zip standalone for Windows

## check OS
if(!R.version$os %in% c("mingw32", "windows"))
    stop("Windows standalone build can only be done on Windows OS")

# default repos
local({
    r <- getOption("repos")
    r["CRAN"] <- "https://cran.r-project.org"
    options(repos=r)
})

# requirement
library(fs)
library(stringr)

# current working directory
cwd <- getwd()

# project directory
proj_dir <- system("git rev-parse --show-toplevel", intern = TRUE)

# package source
src_dir <- file.path(proj_dir, "R-pkg")

# build directory
build_dir <- file.path(proj_dir, "build")

# dist directory
dist_dir <- file.path(build_dir, "dist")

# DesktopDeployR directory
ddr_dir <- file.path(build_dir, "DIYABC-RF_GUI_windows")
if(!dir.exists(ddr_dir))
    stop("Missing submodule DesktopDeployR, please init Git submodules")

# dependencies
fs::file_copy(
    file.path(dist_dir, "requirements.txt"),
    file.path(ddr_dir, "app", "packages.txt"),
    overwrite = TRUE
)

# diyabcGUI windows source
win_zip <- tail(sort(list.files(dist_dir, pattern = "diyabcGUI*")), 1)
fs::file_copy(
    file.path(dist_dir, win_zip),
    file.path(ddr_dir, "src", win_zip),
    overwrite = TRUE
)

# Rcpp sources (temporary fix for R-Portable 4.0.0)
Rcpp_src <- file.path(ddr_dir, "src", "Rcpp")
if(!dir.exists(Rcpp_src))
    system(str_c("git clone https://github.com/RcppCore/Rcpp ", Rcpp_src))

# Environment setup
setwd(ddr_dir)
on.exit(setwd(cwd))
system("cmd.exe /c prepare.bat")
setwd(cwd)

Sys.sleep(2)

# standalone name
app_version <- as.character(packageVersion(
    "diyabcGUI", lib.loc = file.path(ddr_dir, "app", "library")
))
app_name <- "DIYABC-RF_GUI"
full_app_name <- str_c(app_name, "_", app_version)

# zip standalone
zip_dir <- file.path(build_dir, full_app_name)
file.rename(ddr_dir, zip_dir)
on.exit(file.rename(zip_dir, ddr_dir))
setwd(build_dir)
on.exit(setwd(cwd))
zipfile <- file.path(dist_dir, str_c(full_app_name, "_windows.zip"))
if(file.exists(zipfile)) fs::file_delete(zipfile)
zip(
    zipfile = zipfile, 
    files = basename(zip_dir)
)
file.rename(zip_dir, ddr_dir)
