#' Check file_name
#' @keywords internal
#' @author Ghislain Durif
check_file_name <- function(file_name) {
    valid <- TRUE
    if((length(file_name) != 1) || !is.character(file_name) || 
       !file.exists(file_name)) valid <- FALSE
    return(valid)
}

#' Check header file prior definition
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Content: see doc
#' @param cstrng string, prior description.
check_header_prior <- function(strng) {
    # check
    pttrn <- str_c(single_param_regex(), " ",
                   "(N|T|A)", " ",
                   "(UN|LU|NO|LN)", "\\[",
                   str_c(rep(num_regex(), 4), collapse = ","),
                   "\\]")
    valid <- str_detect(strng, pttrn)
    ## output
    return(valid)
}

#' Check header file condition definition
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Content: see doc
#' @param strng string, prior description.
check_header_cond <- function(strng) {
    # check
    pttrn <- str_c("^", single_param_regex(), "(<|=<|>|>=)",
                   single_param_regex(),  "$")
    valid <- str_detect(strng, pttrn)
    ## output
    return(valid)
}

#' Check header file loci description
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Content: see doc
#' @param strng string, prior description.
#' @param type string, `"mss"` or `"snp"`
check_header_loci_desc <- function(strng, type = "mss") {
    # init output
    valid <- FALSE
    # Microsat/Sequence
    if(type == "mss") {
        # Locus_M_A_1_ <A> [M] G1 2 40
        pttrn1 <- str_c("^", single_param_regex(), " ",
                        "<(A|H|X|Y|M)>", " ",
                        "\\[M\\]", " ",
                        "G[0-9]+", " ",
                        "[0-9]+", " ", "[0-9]+", "$")
        # Locus_S_A_21_ <A> [S] G4 1000
        pttrn2 <- str_c("^", single_param_regex(), " ",
                        "<(A|H|X|Y|M)>", " ",
                        "\\[S\\]", " ",
                        "G[0-9]+", " ",
                        "[0-9]+", "$")
        
        # check
        valid <- str_detect(strng, pttrn1) || str_detect(strng, pttrn2)
    } else if(type == "snp") {
        # 5000 <A> G1 from 1
        pttrn <- str_c("^", "[0-9]+", " ",
                       "<(A|H|X|Y|M)>", " ",
                       "G[0-9]+", " ", 
                       "from ", "[0-9]+", "$")
        
        # check
        valid <- str_detect(strng, pttrn)
    }
    ## output
    return(valid)
}
