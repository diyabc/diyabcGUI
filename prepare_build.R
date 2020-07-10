# default repos
local({
    r <- getOption("repos")
    r["CRAN"] <- "https://cran.r-project.org"
    options(repos=r)
})

# Dependencies:
# Ubuntu: apt-get install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev
install.packages("devtools")
install.packages("fs")

library(devtools)
devtools::install_github("chasemc/electricShine", upgrade = "never")

# for windows
install.packages(
    c('dplyr', 'ggplot2', 'lubridate')
)