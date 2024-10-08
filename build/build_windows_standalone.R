## build DIYABC-RF_GUI zip standalone for Windows

# requirement: .Renviron file generated by 'generate_Renviron.R'
if(!file.exists(".Renviron"))
    stop("You should run 'generate_Renviron.R'")

## check OS
if(!R.version$os %in% c("mingw32", "windows"))
    stop("Windows standalone build can only be done on Windows OS")

# requirement
library(fs)
library(stringr)

# current working directory
cwd <- getwd()

# DesktopDeployR directory
win_dir <- file.path(build_dir, "DIYABC-RF_GUI_windows")
if(!dir.exists(win_dir))
    stop("Missing submodule DesktopDeployR, please init Git submodules")

# dependencies
fs::file_copy(
    file.path(dist_dir, "requirements.txt"),
    file.path(win_dir, "app", "packages.txt"),
    overwrite = TRUE
)

# version
pkg_version <- readLines(file.path(dist_dir, "version"))
fs::file_copy(
    file.path(dist_dir, "version"),
    file.path(win_dir, "src", "version"),
    overwrite = TRUE
)

# diyabcGUI windows source
win_zip <- paste0("diyabcGUI_", pkg_version, ".zip")
fs::file_copy(
    file.path(dist_dir, win_zip),
    file.path(win_dir, "src", win_zip),
    overwrite = TRUE
)

# Environment setup
setwd(win_dir)
on.exit(setwd(cwd))
system("cmd.exe /c prepare.bat")
setwd(cwd)

Sys.sleep(2)

# clean standalone source dir before zipping
fs::file_delete(
    list.files(
        file.path(win_dir, "src"), pattern = "diyabcGUI_", 
        full.names = TRUE
    )
)

# standalone name
app_version <- as.character(packageVersion(
    "diyabcGUI", lib.loc = file.path(win_dir, "app", "library")
))
app_name <- "DIYABC-RF_GUI"
full_app_name <- str_c(app_name, "_", app_version)

# zip standalone
zip_dir <- file.path(build_dir, full_app_name)
file.rename(win_dir, zip_dir)
on.exit(file.rename(zip_dir, win_dir))
setwd(build_dir)
on.exit(setwd(cwd))
zipfile <- file.path(dist_dir, str_c(full_app_name, "_windows.zip"))
if(file.exists(zipfile)) fs::file_delete(zipfile)
zip(
    zipfile = zipfile, 
    files = basename(zip_dir)
)
file.rename(zip_dir, win_dir)
