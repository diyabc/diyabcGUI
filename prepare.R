# default repos
local({
    r <- getOption("repos")
    r["CRAN"] <- "https://cran.r-project.org"
    options(repos=r)
})

# Dependencies:
# Ubuntu: apt-get install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev

pkg_list <- c(
    "dplyr",
    "devtools",
    "fs",
    "ggplot2",
    "lubridate",
    "magrittr",
    "pbapply",
    "processx",
    "readr",
    "roxygen2",
    "shiny",
    "shinyBS",
    "shinybusy",
    "shinydashboard",
    "shinyFeedback",
    "shinyFiles",
    "shinyhelper",
    "shinyjs",
    "shinyWidgets",
    "stringr",
    "testthat",
    "tibble",
    "tidyr"
)

install_pkg(pkg_list)
