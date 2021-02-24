# default repos
local({
    r <- getOption("repos")
    r["CRAN"] <- "https://cran.r-project.org"
    options(repos=r)
})

# Dependencies:
# Ubuntu: apt-get install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev

pkg_list <- c(
    "devtools",
    "ggplot2",
    "lubridate",
    "magrittr",
    "pbapply",
    "shiny",
    "shinyBS",
    "shinydashboard",
    "shinyFiles",
    "shinyhelper",
    "shinyjs",
    "shinyWidgets",
    "stringr",
    "tibble")

install_pkg(pkg_list)
