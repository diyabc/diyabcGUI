#' Read and parse statobsRF.txt file
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Content: see doc
#' @param file_name string, (server-side) path to a headersim file.
#' @param file_type string, MIME file type.
#' @param n_stat integer, number of summary statistics in reftable.
read_statobs <- function(file_name, file_type, n_stat) {
    
    # init output
    out <- list(msg = list(), valid = TRUE)
    
    # check file_name
    tmp <- check_file_name(file_name)
    if(!tmp) {
        out$valid <- FALSE
        msg <- tagList("Invalid file file.")
        out$msg <- append(out$msg, list(msg))
    }
    
    # check file_type
    if(file_type != "text/plain") {
        out$valid <- FALSE
        msg <- tagList("Wrong file type.")
        out$msg <- append(out$msg, list(msg))
    }
    
    # continue ?
    if(!out$valid) {
        return(out)
    }
    
    # try reading it
    tmp <- tryCatch(
        read.table(file_name), 
        error = function(e) return(NULL)
    )
    
    if(is.null(tmp)) {
        out$valid <- FALSE
        msg <- tagList("Issue with statobs file format.")
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # check number of rows and columns
    if(nrow(tmp) != 2) {
        out$valid <- FALSE
        msg <- tagList(
            "The statobs file is supposed to contain two lines:",
            "one with the names of the summary statistics,",
            "and a second with the corresponding values",
            "computed on the dataset."
        )
        out$msg <- append(out$msg, list(msg))
    }
    
    if(ncol(tmp) != n_stat) {
        out$valid <- FALSE
        msg <- tagList(
            "The number of summary statistics in the statobs file",
            "does not correspond to the number of summary statistics",
            "in the reftable file."
        )
        out$msg <- append(out$msg, list(msg))
    }
    
    # output
    return(out)
}
