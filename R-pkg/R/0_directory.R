#' find bin directory
#' @keywords internal
#' @author Ghislain Durif
bin_dir <- function() {
    bindir <- system.file("bin", package = "diyabcGUI")
    if(str_length(bindir) == 0) {
        stop("bin directory not found")
    }
    return(bindir)
}

#' find data directory
#' @keywords internal
#' @author Ghislain Durif
#' @param data_type string, `"indseq_snp"`, `"poolseq_snp"` or `"mss"`. Default 
#' is NULL and parent data directory is returned.
data_dir <- function(data_type = NULL) {
    datadir <- system.file("data4test", package = "diyabcGUI")
    if(str_length(datadir) == 0) {
        stop("data directory not found")
    }
    if(!is.null(data_type)) {
        subdir <- switch(
            data_type,
            "indseq_snp" = "IndSeq_SNP",
            "poolseq_snp" = "PoolSeq_SNP",
            "mss" = "Microsat_Sequences"
        )
        datadir <- file.path(datadir, subdir)
    }
    return(datadir)
}

#' find example directory
#' @keywords internal
#' @author Ghislain Durif
example_dir <- function() {
    exampledir <- system.file("example", package = "diyabcGUI")
    if(str_length(exampledir) == 0) {
        stop("example directory not found")
    }
    return(exampledir)
}

#' find help directory
#' @keywords internal
#' @author Ghislain Durif
help_dir <- function() {
    helpdir <- system.file("help", package = "diyabcGUI")
    if(str_length(helpdir) == 0) {
        stop("help directory not found")
    }
    return(helpdir)
}

#' find test_input directory
#' @keywords internal
#' @author Ghislain Durif
test_input_dir <- function() {
    testinputdir <- system.file("test_input", package = "diyabcGUI")
    if(str_length(testinputdir) == 0) {
        stop("test_input directory not found")
    }
    return(testinputdir)
}

#' Create a project directory server-side
#' @keywords internal
#' @author Ghislain Durif
mk_proj_dir <- function(tag = "diyabc") {
    # create tmp dir
    tmp_dir <- tempfile(tag)
    dir.create(tmp_dir, showWarnings = FALSE)
    # output
    return(tmp_dir)
}
