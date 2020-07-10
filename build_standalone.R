# generate standalone interface for Windows/MacOS

# default repos
local({
    r <- getOption("repos")
    r["CRAN"] <- "https://cran.r-project.org"
    options(repos=r)
})

# requirement
install.packages("devtools")
install.packages("fs")
devtools::install_github("chasemc/electricShine")

# install diyabcGUI
devtools::install("R-pkg")

# build standalone
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
    function_name = "run_app",
    local_package_path = file.path(getwd(), "R-pkg"),
    package_install_opts = list(type = "binary"),
    run_build = TRUE,
    permission = TRUE
)
