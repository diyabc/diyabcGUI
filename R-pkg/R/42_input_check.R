#' Check file_name
#' @keywords internal
#' @author Ghislain Durif
check_file_name <- function(file_name) {
    valid <- TRUE
    if((length(file_name) != 1) || !is.character(file_name) || 
       !file.exists(file_name)) valid <- FALSE
    return(valid)
}