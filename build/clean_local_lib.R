# clean local bin
library(fs)

fs::dir_delete(list.dirs(
    file.path(build_dir, ".R_libs"), full.names = TRUE, recursive = FALSE
))