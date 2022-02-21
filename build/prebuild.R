## pre-build script to update DESCRIPTION file
# only tested on GNU/Linux

# prebuild function
prebuild <- function() {
    ## DESCRIPTION file
    filename <- file.path(src_dir, "DESCRIPTION")
    # update Version
    command <- paste0("sed -i -e ",
                      "\"s/Version: .*/Version: $(cat ", 
                      file.path(proj_dir, "version"), ")/\" ",
                      filename)
    tmp <- system(command)
    # update Date
    command <- paste0(
        "sed -i -e ",
        "\"s/Date: .*/Date: ", Sys.Date(), "/\" ",
        filename
    )
    tmp <- system(command)
}

# run
prebuild()
