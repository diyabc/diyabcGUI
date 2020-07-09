# generate standalone interface for Windows

# requirement
install.packages("devtools")
install.packages("gtools")
install.packages("RInno")

# install diyabcGUI
devtools::install("R-pkg", upgrade = "always")

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

# Build an installer
create_app(
    app_name = "diyabcGUI", 
    app_dir = "R-pkg/inst/application",
    app_icon = "coccicon.png",
    pkgs = c(gtools::getDependencies("diyabcGUI"),  "diyabcGUI"),
    dir_out = "build/windows",
    include_R = TRUE
)
compile_iss()

