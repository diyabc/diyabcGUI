# generate Renviron file for build scripts

# R lib directory
R_lib <- file.path(build_dir, ".R_libs")
if(!dir.exists(R_lib)) dir.create(R_lib, recursive = TRUE)

# write R environ
Renviron_file <- file.path(build_dir, ".Renviron")
writeLines(
    c(
        paste0("R_LIBS_USER=", '"', R_lib, '"')
    ),
    file(Renviron_file)
)