#' Check filename
#' @keywords internal
#' @author Ghislain Durif
check_filename <- function(filename) {
    valid <- TRUE
    if(!is.character(filename))
        valid <- FALSE
    else if(!file.exists(filename))
        valid <- FALSE
    return(valid)
}

#' Parse diyabcGUI project file
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Standard name: "diyabcGUI_proj.txt"
#' Content:
#' ```
#' project_name: <project_name>
#' ```
#' @param filename string, (server-side) path to a project file.
parse_diyabc_project <- function(filename, type) {
    # init output
    proj_name <- NULL
    valid <- TRUE
    # check file name
    valid <- check_filename(filename)
    # check file type
    if(type != "text/plain")
        valid <- FALSE
    ## read file
    if(valid) {
        raw_content <- str_split(
            readLines(filename),
            pattern = "\n"
        )
        ## extract project name
        strng <- raw_content[[1]]
        pttrn <- "(?<=project_name=).*"
        if(!str_detect(strng, pttrn)) {
            valid <- FALSE
        } else {
            proj_name <- str_extract(strng, pttrn)
        }
    }
    ## output
    return(lst(proj_name, valid))
}

#' Parse diyabc header file
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Content: see doc
#' @param filename string, (server-side) path to a header file.
parse_diyabc_header <- function(filename, type) {
    
}

#' Parse diyabc simulation header file
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Content: see doc
#' @param filename string, (server-side) path to a headersim file.
parse_diyabc_headersim <- function(filename, type) {
    
}