#' Manage uploading of existing project related files
#' @keywords internal
#' @author Ghislain Durif
#' @param file_input data.frame with fields name (chr), size (int), 
#' type (chr), datapath (chr) storing new uploaded files.
#' @param proj_dir character string, path to project directory. 
proj_file_input <- function(file_input, proj_dir) {
    
    # init output
    out <- list(msg = list(), valid = FALSE)
    
    # # debugging
    # pprint("file input")
    # pprint(file_input)
    
    # check if project zip files was provided
    is_zip <- proj_zip_file_input(file_input)
    out$msg <- append(out$msg, is_zip$msg)
    
    if(is_zip$zip_file) {
        if(!is_zip$valid) {
            out$valid <- FALSE
            return(out)
        }
        file_input <- is_zip$file_input
    }
    
    # # debugging
    # pprint("file input")
    # pprint(file_input)
    
    # manage new uploaded project files
    if(nrow(file_input) > 0) {
        
        # empty project directory
        fs::dir_delete(list.dirs(proj_dir, recursive = FALSE))
        fs::file_delete(list.files(proj_dir, full.names = TRUE))
        
        # copy files to project directory
        lapply(
            split(file_input, seq(nrow(file_input))),
            function(item) {
                if(item$type == "diyabc_dir") {
                    fs::dir_copy(
                        item$datapath,
                        file.path(proj_dir, item$name),
                        overwrite = TRUE
                    )
                    if(dir.exists(item$datapath)) {
                        # logging("deleting:", item$datapath)
                        fs::dir_delete(item$datapath)
                    }
                } else {
                    fs::file_copy(item$datapath,
                                  file.path(proj_dir, item$name),
                                  overwrite = TRUE)
                    if(file.exists(item$datapath)) {
                        # logging("deleting:", item$datapath)
                        fs::file_delete(item$datapath)
                    }
                }
            }
        )
        
        # valid upload ?
        out$valid <- TRUE
    
    } else {
        msg <- tagList(
            "No file was provided."
        )
        out$msg <- append(out$msg, list(msg))
        out$valid <- FALSE
    }
    
    # output
    return(out)
}


#' Manage existing project zip file
#' @keywords internal
#' @author Ghislain Durif
#' @param file_input data.frame with fields name (chr), size (int), 
#' type (chr), datapath (chr) storing new uploaded files.
proj_zip_file_input <- function(file_input) {
    
    # init output
    out <- list(
        file_input = NULL, msg = list(), zip_file = FALSE, valid = FALSE
    )
    
    # any uploaded zip file ?
    zip_file_ind <- (file_input$type == "application/zip")
    
    ## IF NOT
    if(!any(zip_file_ind)) {
        return(out)
    }
    
    ## ELSE
    out$zip_file <- TRUE
    
    # a single or multiple zip files ?
    if(sum(zip_file_ind) > 1) {
        msg <- tagList(
            "You provided more than", tags$b("one"), "project", 
            tags$code("zip"), "files."
        )
        out$msg <- append(out$msg, list(msg))
        out$valid <- FALSE
        return(out)
    }
    
    # a zip file and other files ?
    if(nrow(file_input) > 1) {
        msg <- tagList(
            "You provided a project", tags$code("zip"), "file",
            tags$b("and"), "other file(s)."
        )
        out$msg <- append(out$msg, list(msg))
        out$valid <- FALSE
        return(out)
    }
    
    ## READY TO EXTRACT PROJECT FILES
    out$valid <- TRUE
    
    # tmp dir for extraction
    tmp_dir <- mk_proj_dir("diyabc_zip_extract")
    
    # extract project files
    unzip(
        file_input$datapath[1], 
        exdir = tmp_dir
    )
    
    # list content of zip file
    tmp_file_list <- list.files(tmp_dir)
    
    # check if zip file content was at zip root or inside a root directory
    if(length(tmp_file_list) == 1) {
        
        # if extracted project files inside a root directory
        if(file.info(tmp_dir)$isdir) {
            # move into extracted project directory
            tmp_dir <- file.path(tmp_dir, tmp_file_list)
            # update list content of zip file
            tmp_file_list <- list.files(tmp_dir)
        }
    }
    
    # check extracted project files
    if(length(tmp_file_list) > 0) {
        
        # modify file_input with extracted files
        #   data.frame with 4 columns:
        #       name (chr), size (int), type (chr), datapath (chr)
        out$file_input <- Reduce(
            "rbind",
            lapply(
                tmp_file_list,
                function(tmp_file) {
                    tmp_file_info <- file.info(
                        file.path(tmp_dir, tmp_file)
                    )
                    return(
                        data.frame(
                            name = tmp_file,
                            size = tmp_file_info$size,
                            type = ifelse(
                                tmp_file_info$isdir,
                                "diyabc_dir",
                                "diyabc_file"
                            ),
                            datapath = file.path(tmp_dir, tmp_file),
                            stringsAsFactors = FALSE
                        )
                    )
                }
            )
        )
        
        ## specific file type
        ind <- which(out$file_input$name == "headerRF.txt")
        out$file_input$type[ind] <- "text/plain"
        ind <- which(out$file_input$name == "header.txt")
        out$file_input$type[ind] <- "text/plain"
        ind <- which(out$file_input$name == "reftableRF.bin")
        out$file_input$type[ind] <- "application/octet-stream"
        ind <- which(out$file_input$name == "statobsRF.txt")
        out$file_input$type[ind] <- "text/plain"
        
    } else {
        
        out$file_input <- head(file_input, 0)
        out$valid <- FALSE
        msg <- tagList(
            "You provided an empty project", tags$code("zip"), "file.",
        )
        out$msg <- append(out$msg, list(msg))
    }
    
    # output
    return(out)
}

#' Check existing project related files
#' @keywords internal
#' @author Ghislain Durif
#' @param proj_dir character string, path to project directory.
#' @param locus_type character string, `"snp"` or `"mss"`.
#' @importFrom mime guess_type
check_proj_file <- function(proj_dir, locus_type = "snp") {
    
    # init output
    out <- list(
        msg = list(), valid = TRUE, 
        header_check = NULL, statobs_check = NULL,
        reftable_check = NULL
    )
    
    # check project directory
    if(!dir.exists(proj_dir)) {
        out$valid <- FALSE
        msg <- tagList("Input project directory does not exist.")
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # project files
    proj_file_list <- list.files(proj_dir)
    
    # check header
    if(any(c("header.txt", "headerRF.txt") %in% proj_file_list)) {
        header_file <- file.path(
            proj_dir,
            ifelse(
                "headerRF.txt" %in% proj_file_list,
                "headerRF.txt", "header.txt"
            )
        )
        out$header_check <- tryCatch(
            read_header(
                file_name = header_file, 
                file_type = mime::guess_type(header_file),
                locus_type = locus_type
            ),
            error = function(e) return(NULL)
        )
        if(is.null(out$header_check)) {
            out$valid <- FALSE
            msg <- tagList("Issue when checking project header file.")
            out$msg <- append(out$msg, list(msg))
        }
    }
    
    # check reftable
    if("reftableRF.bin" %in% proj_file_list) {
        reftable_file <- file.path(proj_dir, "reftableRF.bin")
        out$reftable_check <- tryCatch(
            read_reftable(
                file_name = reftable_file, 
                file_type = mime::guess_type(reftable_file)
            ),
            error = function(e) return(NULL)
        )
        if(is.null(out$reftable_check)) {
            out$valid <- FALSE
            msg <- tagList("Issue when checking project reftable file.")
            out$msg <- append(out$msg, list(msg))
        }
    }
    
    # check statobs
    if("statobsRF.txt" %in% proj_file_list) {
        if(!is.null(out$reftable_check) && 
           !is.null(out$reftable_check$n_stat)) {
            statobs_file <- file.path(proj_dir, "statobsRF.txt")
            out$statobs_check <- tryCatch(
                read_statobs(
                    file_name = statobs_file, 
                    file_type = mime::guess_type(statobs_file),
                    n_stat = out$reftable_check$n_stat
                ),
                error = function(e) return(NULL)
            )
            if(is.null(out$statobs_check)) {
                out$valid <- FALSE
                msg <- tagList("Issue when checking project statobs file.")
                out$msg <- append(out$msg, list(msg))
            }
        } else {
            out$valid <- FALSE
            msg <- tagList(
                "Impossible to check project statobs file", 
                "because issue with reftable file.")
            out$msg <- append(out$msg, list(msg))
        }
    }
    
    # output
    return(out)
}