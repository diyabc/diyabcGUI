# clean local bin
library(fs)

fs::dir_delete(list.dirs(
    file.path("DIYABC-RF_GUI_windows", "app", "library"), 
    full.names = TRUE, recursive = FALSE
))