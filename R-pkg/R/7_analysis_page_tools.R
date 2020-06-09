#' Write diyabc project file
#' @keywords internal
#' @author Ghislain Durif
write_proj_file <- function(proj_dir, proj_name) {
    filename <- "diyabcGUI_proj.txt"
    content <- str_c("project_name=", proj_name)
    writeLines(content, file.path(proj_dir, filename))
}