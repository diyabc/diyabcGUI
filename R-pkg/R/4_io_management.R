#' Check file_name
#' @keywords internal
#' @author Ghislain Durif
check_file_name <- function(file_name) {
    valid <- TRUE
    if(!is.character(file_name))
        valid <- FALSE
    else if(!file.exists(file_name))
        valid <- FALSE
    return(valid)
}

#' Check data file
#' @keywords internal
#' @author Ghislain Durif
#' @param data_file string, data file name.
#' @param data_dir string, path to directory where data file is stored.
#' @param locus_type string, locus type `"mss"` or `"snp"`.
#' @param seq_mode string, `"indseq"` or `"poolseq"`.
#' @param expected_data_file string, expected data file name for 
#' existing project, default is NULL.
#' @importFrom tools file_ext
check_data_file <- function(data_file, data_dir, locus_type, seq_mode, 
                            expected_data_file = NULL) {
    # output
    out <- NULL
    # ## debugging
    # logging("data_file = ", data_file)
    ## mss locus
    if(locus_type == "mss") {
        out <- check_mss_data_file(data_file, data_dir, expected_data_file)
    ## snp locus / indseq
    } else if((locus_type == "snp") & (seq_mode == "indseq")) {
        out <- check_indseq_snp_data_file(data_file, data_dir, 
                                          expected_data_file)
    ## snp locus / poolseq
    } else if((locus_type == "snp") & (seq_mode == "poolseq")) {
        out <- check_poolseq_snp_data_file(data_file, data_dir, 
                                           expected_data_file)
    } else {
        stop("Issue with input arguments")
    }
    
    ## output
    return(out)
}

#' Check IndSeq SNP data file
#' @keywords internal
#' @author Ghislain Durif
#' @param data_file string, data file name.
#' @param data_dir string, path to directory where data file is stored.
#' @param expected_data_file string, expected data file name for 
#' existing project, default is NULL.
#' @importFrom tools file_ext
check_indseq_snp_data_file <- function(data_file, data_dir, 
                                       expected_data_file = NULL) {
    # output
    header <- NULL
    info <- NULL
    spec <- NULL
    valid <- TRUE
    
    err <- list()
    msg <- list()
    
    # data path
    data_path <- file.path(data_dir, data_file)
    
    ## file exists and is not empty?
    if(!file.exists(data_path)) {
        err <- append(err, "Input data file does not exist")
        valid <- FALSE
    } else if(file.info(data_path)$size == 0) {
        err <- append(err, "Input data file is empty")
        valid <- FALSE
    } else {
        ## init output
        locus <- NULL
        n_loci <- NULL
        n_pop <- NULL
        n_indiv <- NULL
        ## check file type
        if(tools::file_ext(data_path) != "snp") {
            err <- append(
                err, 
                "IndSeq SNP files should have an extension '.snp'."
            )
            valid <- FALSE
        } else {
            ## info
            info <- readLines(data_path, n = 1, warn = FALSE)
            # sex ratio
            pttrn <- "(?<=<)NM=[0-9]+\\.?[0-9]*NF(?=>)"
            if(str_detect(info, pttrn)) {
                msg <- append(
                    msg,
                    str_c("Sex ratio:", str_extract(info, pttrn), sep = " ")
                )
            } else {
                err <- append(
                    err,
                    str_c(
                        "Issue with IndSeq SNP file header first line:",
                        "missing 'sex ratio', see manual", sep = " "
                    )
                )
                valid <- FALSE
            }
            # MAF
            pttrn <- "(?<=<)MAF=[:graph:]+(?=>)"
            if(str_detect(info, pttrn)) {
                msg <- append(
                    msg,
                    str_c("Minimum allele frequency criterion:", 
                          str_extract(info, pttrn), sep = " ")
                )
            } else {
                msg <- append(
                    msg,
                    str_c(
                        "Missing 'minimum allele frequency criterion (MAF)'",
                        "in IndSeq SNP file header first line:",
                        "Hudson algorithm will be used.", sep = " "
                    )
                )
            }
            # additional info
            pttrn <- str_c(
                "(<NM=[0-9]+\\.?[0-9]*NF>)", 
                "(<MAF=[:graph:]+>)",
                sep = "|"
            )
            add_info <- str_trim(str_replace_all(info, pttrn, ""))
            if(str_length(add_info) > 0) {
                msg <- append(
                    msg,
                    str_c("Additional information:", add_info, sep = " ")
                )
            }
            ## header
            header <- tryCatch(
                as.vector(read.table(file = data_path, skip = 1, nrows = 1)), 
                error = function(e) e
            )
            if("error" %in% class(header)) {
                err <- append(
                    err, 
                    stR_c(
                        "Issue with IndSeq SNP file header second line:",
                        header$message, sep = " "
                    )
                )
                valid <- FALSE
            } else {
                # header format
                if(header[1] != "IND" & header[2] != "SEX" & 
                   header[3] != "POP" &
                   !all(header[-(1:3)] %in% c("A", "H", "X", "Y", "M"))) {
                    err <- append(
                        err, 
                        str_c(
                            "Issue with IndSeq SNP file header second line:",
                            "required format is 'IND SEX POP' followed by",
                            "a letter indicator among 'A', 'H', 'X', 'Y', 'M'",
                            "for each SNP locus in the file (see manual).",
                            sep = " "
                        )
                    )
                    valid <- FALSE
                }
                # nb of locus
                n_loci <- length(header) - 3
                msg <- append(
                    msg,
                    str_c("Number of loci =", n_loci, sep = " ")
                )
                # locus type
                candidate_locus <- c("A", "H", "X", "Y", "M")
                locus_encoding <- str_c(header[-(1:3)], collapse = " ")
                locus <- unlist(lapply(
                    candidate_locus, 
                    function(pttrn) {
                        count <- str_count(locus_encoding, pttrn)
                        if(count > 0)
                            return(str_c(count, " <", pttrn, ">"))
                    }
                ))
                msg <- append(
                    msg,
                    str_c("loci:", str_c(locus, collapse =  ", "), sep = " ")
                )
                # merge header
                header_length <- length(header)
                header <- str_c(header[1:min(20,header_length)], 
                                collapse = " ")
                if(header_length > 20)
                    header <- str_c(header, "...", sep = " ")
                msg <- append(
                    msg,
                    str_c("header:", str_c("'", header, "'"), sep = " ")
                )
                ## content
                content <- tryCatch(
                    read.table(file = data_path, skip = 2), 
                    error = function(e) e
                )
                if("error" %in% class(content)) {
                    err <- append(
                        err, 
                        str_c(
                            "Issue with IndSeq SNP data file content:",
                            content$message, sep = " "
                        )
                    )
                    valid <- FALSE
                } else {
                    # check number of locus
                    if(n_loci != (ncol(content) - 3)) {
                        err <- append(
                            err, 
                            str_c(
                                "Issue with IndSeq SNP data file content:",
                                "number of loci not consistent between",
                                "file header and file content.",
                                sep = " "
                            )
                        )
                        valid <- FALSE
                    }
                    # check sex content
                    if(!all(content[,2] %in% c("F", "M"))) {
                        err <- append(
                            err, 
                            str_c(
                                "Issue with IndSeq SNP data file content:",
                                "'SEX' column should contain only",
                                "'F' for female or 'M' for male (see manual).",
                                sep = " "
                            )
                        )
                        valid <- FALSE
                    }
                    # number of pop
                    n_pop <- length(unique(content[,3]))
                    # number of individuals
                    n_indiv <- nrow(content)
                    msg <- append(
                        msg,
                        str_c(
                            "Sample:",
                            n_indiv, "individuals", 
                            "from", n_pop, "populations", 
                            sep = " ")
                    )
                    # check SNP encoding
                    check_snp_encoding <- mclapply(
                        1:nrow(content[,-(1:3)]), 
                        function(ind)
                            !all(content[ind,-(1:3)] %in% c(0,1,2,9)), 
                        mc.cores = getOption("diyabcGUI")$ncore
                    )
                    seek_error <- unlist(lapply(
                        check_snp_encoding, 
                        function(item) "try-error" %in% class(item)
                    ))
                    if(any(seek_error)) {
                        error_msg <- attributes(
                            check_snp_encoding[[ which(seek_error[1]) ]]
                        )$condition$message
                        err <- append(
                            err, 
                            str_c(
                                "Issue when checking IndSeq SNP data file",
                                "content:",
                                error_msg,
                                sep = " "
                            )
                        )
                        valid <- FALSE
                    } else {
                        if(any(unlist(check_snp_encoding))) {
                            err <- append(
                                err, 
                                str_c(
                                    "Issue with IndSeq SNP file data content:",
                                    "SNP encoding should be '0', '1', '2'",
                                    "or '9' for missing data (see manual).",
                                    sep = " "
                                )
                            )
                            valid <- FALSE
                        }
                    }
                }
            }
        }
        # output
        spec <- lst(locus, n_indiv, n_loci, n_pop)
    }
    ## expected data file ?
    if(!is.null(expected_data_file)) {
        if(data_file != expected_data_file) {
            err <- append(
                err, 
                str_c(
                    "Data file expected by provided 'header.txt'",
                    "or 'headerRF.txt' file(s)",
                    "and data file provided by user are different.",
                    sep = " "
                )
            )
            valid <- FALSE
        }
    }
    ## output    
    out <- lst(header, info, spec, valid, err, msg)
    return(out)
}

#' Check PoolSeq SNP data file
#' @keywords internal
#' @author Ghislain Durif
#' @param data_file string, data file name.
#' @param data_dir string, path to directory where data file is stored.
#' @param expected_data_file string, expected data file name for 
#' existing project, default is NULL.
#' @importFrom tools file_ext
check_poolseq_snp_data_file <- function(data_file, data_dir, 
                                        expected_data_file = NULL) {
    # output
    header <- NULL
    info <- NULL
    spec <- NULL
    valid <- TRUE
    
    err <- list()
    msg <- list()
    
    # data path
    data_path <- file.path(data_dir, data_file)
    
    ## file exists and is not empty?
    if(!file.exists(data_path)) {
        err <- append(err, "Input data file does not exist")
        valid <- FALSE
    } else if(file.info(data_path)$size == 0) {
        err <- append(err, "Input data file is empty")
        valid <- FALSE
    } else {
        ## init output
        locus <- NULL
        n_loci <- NULL
        n_pop <- NULL
        n_indiv <- NULL
        ## check file type
        if(tools::file_ext(data_path) != "snp") {
            err <- append(
                err, 
                "PoolSeq SNP files should have an extension '.snp'."
            )
            valid <- FALSE
        } else {
            ## info
            info <- readLines(data_path, n = 1, warn = FALSE)
            # sex ratio
            pttrn <- "(?<=<)NM=[0-9]+\\.?[0-9]*NF(?=>)"
            if(str_detect(info, pttrn)) {
                msg <- append(
                    msg,
                    str_c("Sex ratio:", str_extract(info, pttrn), sep = " ")
                )
            } else {
                err <- append(
                    err,
                    str_c(
                        "Issue with PoolSeq SNP file header first line:",
                        "missing 'sex ratio', see manual", sep = " "
                    )
                )
                valid <- FALSE
            }
            # MRC
            pttrn <- "(?<=<)MRC=[:graph:]+(?=>)"
            if(str_detect(info, pttrn)) {
                msg <- append(
                    msg,
                    str_c(
                        "Minimum read count:", 
                        str_extract(info, pttrn), sep = " "
                    )
                )
            } else {
                err <- append(
                    err,
                    str_c(
                        "Issue with PoolSeq SNP file header first line:",
                        "missing 'minimum read count (MRC)', see manual", 
                        sep = " "
                    )
                )
                valid <- FALSE
            }
            # MAF
            pttrn <- "(?<=<)MAF=[:graph:]+(?=>)"
            if(str_detect(info, pttrn)) {
                msg <- append(
                    msg,
                    str_c("Minimum allele frequency criterion:", 
                          str_extract(info, pttrn), sep = " ")
                )
            } else {
                msg <- append(
                    msg,
                    str_c(
                        "Missing 'minimum allele frequency criterion (MAF)'",
                        "in PoolSeq SNP file header first line:",
                        "Hudson algorithm will be used.", sep = " "
                    )
                )
            }
            # additional info
            pttrn <- str_c(
                "(<NM=[0-9]+\\.?[0-9]*NF>)", 
                "(<MAF=[:graph:]+>)", 
                "(<MRC=[:graph:]+>)",
                sep = "|"
            )
            add_info <- str_trim(str_replace_all(info, pttrn, ""))
            if(str_length(add_info) > 0) {
                msg <- append(
                    msg,
                    str_c("Additional information:", add_info, sep = " ")
                )
            }
            ## header
            header <- tryCatch(
                as.vector(read.table(file = data_path, skip = 1, nrows = 1)), 
                error = function(e) e
            )
            if("error" %in% class(header)) {
                err <- append(
                    err, 
                    stR_c(
                        "Issue with PoolSeq SNP file header second line:",
                        header$message, sep = " "
                    )
                )
                valid <- FALSE
            } else {
                # header format
                if(header[1] != "POOL" &
                   header[2] != "POP_NAME:HAPLOID_SAMPLE_SIZE" & 
                   !all(str_detect(header[-(1:2)], "POP[0-9]+:[0-9]+"))) {
                    err <- append(
                        err, 
                        str_c(
                            "Issue with PoolSeq SNP file header second line:",
                            "required format is", 
                            "'POOL POP_NAME:HAPLOID_SAMPLE_SIZE' followed by",
                            "a character string 'POP<pop_id>:<sample_size>'",
                            "for each population in the file (see manual).",
                            sep = " "
                        )
                    )
                    valid <- FALSE
                }
                # number of population
                n_pop <- length(header) - 2
                # merge header
                header <- str_c(header, collapse = " ")
                msg <- append(
                    msg,
                    str_c("header:", str_c("'", header, "'"), sep = " ")
                )
                ## content
                content <- tryCatch(
                    read.table(file = data_path, skip = 2), 
                    error = function(e) e
                )
                if("error" %in% class(content)) {
                    err <- append(
                        err, 
                        str_c(
                            "Issue with PoolSeq SNP data file content:",
                            content$message, sep = " "
                        )
                    )
                    valid <- FALSE
                } else {
                    # check number of population
                    if(ncol(content) %% 2 != 0) {
                        err <- append(
                            err, 
                            str_c(
                                "Issue with PoolSeq SNP data file content:",
                                "number of column should be even,",
                                "providing pair of counts for reference",
                                "and alternate alleles at each locus",
                                "in each popultation.",
                                sep = " "
                            )
                        )
                        valid <- FALSE
                    } 
                    if((ncol(content) / 2) != n_pop) {
                        err <- append(
                            err, 
                            str_c(
                                "Issue with PoolSeq SNP data", 
                                "file second-line header",
                                "or content:",
                                "number of population indicated in",
                                "file second-line header",
                                "does not correspond to",
                                "number of columns in file content.",
                                sep = " "
                            )
                        )
                        valid <- FALSE
                    }
                    # nb of locus
                    n_loci <- nrow(content)
                    msg <- append(
                        msg,
                        str_c("Number of loci =", n_loci, sep = " ")
                    )
                    # locus type
                    locus <- str_c(n_loci, "<A>", sep = " ")
                    msg <- append(
                        msg,
                        str_c(
                            "loci:", str_c(locus, collapse =  ", "), 
                            sep = " "
                        )
                    )
                    # check data encoding
                    check_snp_encoding <- apply(content, 2, is.integer)
                    if(!all(unlist(check_snp_encoding))) {
                        err <- append(
                            err, 
                            str_c(
                                "Issue with PoolSeq SNP file data content:",
                                "SNP encoding should be read counts,",
                                "i.e. positive integer (see manual).",
                                sep = " "
                            )
                        )
                        valid <- FALSE
                    }
                    if(any(is.na(content))) {
                        err <- append(
                            err, 
                            str_c(
                                "Issue with PoolSeq SNP file data content:",
                                "no missing values are allowed.",
                                sep = " "
                            )
                        )
                        valid <- FALSE
                    }
                }
            }
        }
        # output
        spec <- lst(locus, n_indiv, n_loci, n_pop)
    }
    ## expected data file ?
    if(!is.null(expected_data_file)) {
        if(data_file != expected_data_file) {
            err <- append(
                err, 
                str_c(
                    "Data file expected by provided 'header.txt'",
                    "or 'headerRF.txt' file(s)",
                    "and data file provided by user are different.",
                    sep = " "
                )
            )
            valid <- FALSE
        }
    }
    ## output    
    out <- lst(header, info, spec, valid, err, msg)
    return(out)
}

#' Check Microsat/Sequence (GenePop) data file
#' @keywords internal
#' @author Ghislain Durif
#' @param data_file string, data file name.
#' @param data_dir string, path to directory where data file is stored.
#' @param expected_data_file string, expected data file name for 
#' existing project, default is NULL.
#' @importFrom tools file_ext
check_mss_data_file <- function(data_file, data_dir, 
                                expected_data_file = NULL) {
    # output
    header <- NULL
    info <- NULL
    spec <- NULL
    valid <- TRUE
    
    err <- list()
    msg <- list()
    
    # data path
    data_path <- file.path(data_dir, data_file)
    
    ## file exists and is not empty?
    if(!file.exists(data_path)) {
        err <- append(err, "Input data file does not exist")
        valid <- FALSE
    } else if(file.info(data_path)$size == 0) {
        err <- append(err, "Input data file is empty")
        valid <- FALSE
    } else {
        ## init output
        locus <- NULL
        locus_mode <- NULL
        locus_name <- NULL
        seq_length <- NULL
        n_loci <- NULL
        n_pop <- NULL
        n_indiv <- NULL
        pop_size <- NULL
        ## check file type
        if(tools::file_ext(data_path) != "mss") {
            err <- append(
                err, 
                "Microsat/Sequence files should have an extension '.mss'"
            )
            valid <- FALSE
        } else {
            ## info
            info <- readLines(data_path, n = 1, warn = FALSE)
            # sex ratio
            pttrn <- "(?<=<)NM=[0-9]+\\.?[0-9]*NF(?=>)"
            if(str_detect(info, pttrn)) {
                msg <- append(
                    msg,
                    str_c("Sex ratio:", str_extract(info, pttrn), sep = " ")
                )
            } else {
                err <- append(
                    err,
                    str_c(
                        "Issue with Microsat/Sequence file header first line:",
                        "missing 'sex ratio', see manual", sep = " "
                    )
                )
                valid <- FALSE
            }
            # additional info
            pttrn <- "<NM=[0-9]+\\.?[0-9]*NF>"
            add_info <- str_trim(str_replace_all(info, pttrn, ""))
            if(str_length(add_info) > 0) {
                msg <- append(
                    msg,
                    str_c("Additional information:", add_info, sep = " ")
                )
            }
            
            ## file content
            file_content <- readLines(data_path, warn = FALSE)
            
            ## locus description
            pttrn <- "(?<=<)(A|H|X|Y|M)(?=>)"
            locus_pttrn_match <- str_extract(file_content, pttrn)
            # check if present
            if(all(is.na(locus_pttrn_match))) {
                err <- append(
                    err,
                    str_c(
                        "Issue with Microsat/Sequence file locus description:",
                        "missing locus description, see manual", sep = " "
                    )
                )
                valid <- FALSE
            } else {
                # check if description follows title line
                locus_match_ind <- which(!is.na(locus_pttrn_match))
                if(!all(locus_match_ind == 2:tail(locus_match_ind, 1))) {
                    err <- append(
                        err,
                        str_c(
                            "Issue with Microsat/Sequence file locus",
                            "description:",
                            "locus description should be located",
                            "at beginning of data file, after title line,", 
                            "with a single locus per line, see manual", 
                            sep = " "
                        )
                    )
                    valid <- FALSE
                }
                
                # check locus format
                # pttrn <- "^(Locus )?[A-Za-z0-9_-]+ <(A|H|X|Y|M)>$"
                pttrn <- "^[A-Za-z0-9\\s_\\-]+ <(A|H|X|Y|M)>$"
                locus_spec_match_ind <- which(str_detect(file_content, pttrn))
                if(!all(locus_spec_match_ind %in% locus_match_ind) & 
                   !all(locus_match_ind %in% locus_spec_match_ind)) {
                    err <- append(
                        err,
                        str_c(
                            "Issue with Microsat/Sequence file locus",
                            "description format at rows:",
                            str_c(
                                locus_spec_match_ind[!locus_spec_match_ind %in% locus_match_ind],
                                collapse = ", "
                            ), 
                            sep = " "
                        ),
                        str_c(
                            "You can use the following character to specify",
                            "locus names:",
                            "'A-Z', 'a-z', '0-9', '_', '-' and ' '",
                            sep = " "
                        )
                    )
                    valid <- FALSE
                }
                
                ## number of locus
                n_loci <- length(locus_match_ind)
                msg <- append(
                    msg,
                    str_c("Number of loci =", n_loci, sep = " ")
                )
                
                ## locus info
                pttrn <- "(?<=<)(A|H|X|Y|M)(?=>$)"
                locus <- str_extract(file_content[locus_match_ind], pttrn)
                
                ## locus name
                pttrn <- "^[A-Za-z0-9\\s_\\-]+(?= <)"
                locus_name <- str_extract(file_content[locus_match_ind], pttrn)
                locus_name <- str_replace_all(locus_name, " +", "_")
                
                if(length(unique(locus_name)) != length(locus_name)) {
                    err <- append(
                        err,
                        str_c(
                            "Issue with Microsat/Sequence file locus",
                            "description:",
                            "each locus should have a unique name", 
                            sep = " "
                        )
                    )
                    valid <- FALSE
                }
            }
            
            ## remove unnecessary space
            file_content <- str_replace_all(
                str_trim(file_content),
                " +", " "
            )
            
            ## population
            pttrn <- "^(?i)pop(?-i)$"
            pop_match_ind <- which(str_detect(file_content, pttrn))
            # check
            if(length(pop_match_ind) == 0) {
                err <- append(
                    err,
                    str_c(
                        "Issue with Microsat/Sequence file locus",
                        "data description:",
                        "keyword 'POP' is missing, see manual", 
                        sep = " "
                    )
                )
                valid <- FALSE
            } else {
                # number of pop
                n_pop <- length(pop_match_ind)
                # pop size
                pop_size <- diff(c(pop_match_ind, length(file_content))) - 
                                c(rep(1, n_pop - 1), 0)
                # number of individuals
                n_indiv <- sum(pop_size)
                # msg
                msg <- append(
                    msg,
                    str_c(
                        "Sample:",
                        n_indiv, "individuals", 
                        "from", n_pop, "populations", 
                        sep = " ")
                )
                
                ## DATA content
                data_ind <- head(pop_match_ind, 1):length(file_content)
                data_content <- file_content[data_ind]
                data_content <- data_content[data_content != "POP"]
                
                # write data content to a temporary file
                data_content <- str_replace_all(
                    str_replace_all(data_content, ",", " "),
                    " +",  ";"
                )
                tmpFile <- file.path(data_dir, "tmp_data_file.csv")
                tmp <- writeLines(data_content, tmpFile)
                on.exit({
                    if(file.exists(tmpFile))
                        fs::file_delete(tmpFile)
                })
                
                # read data as data.frame
                data_content <- tryCatch(
                    read.table(tmpFile, sep = ";", stringsAsFactors = FALSE,
                               colClasses = "character"), 
                    error = function(e) e
                )
                if("error" %in% class(data_content)) {
                    err <- append(
                        err, 
                        str_c(
                            "Issue with Microsat/Sequences data file content:",
                            content$message, sep = " "
                        )
                    )
                    valid <- FALSE
                } else {
                    ##### check data
                    ## check number of loci
                    if(ncol(data_content) != n_loci + 1) {
                        err <- append(
                            err, 
                            str_c(
                                "Issue with Microsat/Sequences data file",
                                "content:",
                                "number of declared loci at beginning of file", 
                                "does not correspond to number of actual loci", 
                                "in the data", sep = " "
                            )
                        )
                        valid <- FALSE
                    }
                    
                    # format data.frame
                    colnames(data_content) <- c("indiv", 
                                                str_c("locus", 1:n_loci))
                    data_content$pop <- as.character(rep(1:n_pop, pop_size))
                    
                    ## indiv name first column
                    if(!all(str_detect(data_content$indiv, 
                                       "[A-Za-z0-9_-]+"))) {
                        err <- append(
                            err,
                            str_c(
                                "Issue with Microsat/Sequences data file",
                                "content:",
                                "First column should provide individual names,",
                                "you can use the following character to specify",
                                "such names:",
                                "'A-Z', 'a-z', '0-9', '_', '-'",
                                sep = " "
                            )
                        )
                        valid <- FALSE
                    }
                    
                    ## check locus encoding
                    microsat_hap_encoding <- "^[0-9]{3}$"
                    microsat_dip_encoding <- "^[0-9]{6}$"
                    microsat_x_encoding <- "^[0-9]{3}|[0-9]{6}$"
                    nucleotid_encoding <- "[ATCG]*"
                    seq_hap_encoding <- str_c("^<\\[", 
                                              nucleotid_encoding, 
                                              "\\]>")
                    seq_dip_encoding <- str_c("^<\\[", 
                                              nucleotid_encoding, 
                                              "\\]\\[",
                                              nucleotid_encoding,
                                              "\\]>")
                    seq_x_encoding <- str_c("^<\\[", 
                                            nucleotid_encoding, 
                                            "\\](\\[",
                                            nucleotid_encoding,
                                            "\\])?>")
                    
                    # locus data
                    locus_data <- data_content[,2:(ncol(data_content) - 1)]
                    
                    if(ncol(data_content) == 3) {
                        locus_data <- data.frame(locus1 = locus_data)
                    }
                    
                    # microsat locus
                    microsat_hap_locus <- which(
                        apply(
                            locus_data, 2, function(loc) {
                                return(all(str_detect(loc, 
                                                      microsat_hap_encoding)))
                            }
                        ) & (! locus == "X")
                    )
                    microsat_dip_locus <- which(
                        apply(
                            locus_data, 2, function(loc) {
                                return(all(str_detect(loc, 
                                                      microsat_dip_encoding)))
                            }
                        ) & (! locus == "X")
                    )
                    microsat_x_locus <- which(
                        apply(
                            locus_data, 2, function(loc) {
                                return(all(str_detect(loc, 
                                                      microsat_x_encoding)))
                            }
                        ) & (locus == "X")
                    )
                    
                    # seq locus
                    seq_hap_locus <- which(
                        apply(
                            locus_data, 2, function(loc) {
                                return(all(str_detect(loc, 
                                                      seq_hap_encoding)))
                            }
                        ) & (! locus == "X")
                    )
                    seq_dip_locus <- which(
                        apply(
                            locus_data, 2, function(loc) {
                                return(all(str_detect(loc, 
                                                      seq_dip_encoding)))
                            }
                        ) & (! locus == "X")
                    )
                    seq_x_locus <- which(
                        apply(
                            locus_data, 2, function(loc) {
                                return(all(str_detect(loc, 
                                                      seq_x_encoding)))
                            }
                        ) & (locus == "X")
                    )
                    
                    # check if issue with formating
                    if(!all(sort(c(microsat_hap_locus, microsat_dip_locus,
                                   microsat_x_locus,
                                   seq_hap_locus, seq_dip_locus,
                                   seq_x_locus)) == 
                            1:n_loci)) {
                        err <- append(
                            err,
                            str_c(
                                "Issue with Microsat/Sequences data file",
                                "content in columns:",
                                str_c(
                                    (1:n_loci)[!(1:n_loci) %in% 
                                                   c(microsat_hap_locus, 
                                                     microsat_dip_locus,
                                                     microsat_x_locus,
                                                     seq_hap_locus, 
                                                     seq_dip_locus,
                                                     seq_x_locus)],
                                    collapse = ", "
                                ),
                                sep = " "
                            )
                        )
                        valid <- FALSE
                    } else {
                        ## locus mode (microsat or sequence, hap or dip)
                        tmp_locus_mode <- data.frame(
                            mode = c(rep("microsat_hap", 
                                         length(microsat_hap_locus)), 
                                     rep("microsat_dip", 
                                         length(microsat_dip_locus)), 
                                     rep("microsat_xy", 
                                         length(microsat_x_locus)),
                                     rep("seq_hap", 
                                         length(seq_hap_locus)), 
                                     rep("seq_dip", 
                                         length(seq_dip_locus)), 
                                     rep("seq_xy", 
                                         length(seq_x_locus))),
                            index = c(microsat_hap_locus, microsat_dip_locus,
                                      microsat_x_locus,
                                      seq_hap_locus, seq_dip_locus,
                                      seq_x_locus)
                        )
                        locus_mode <- 
                            tmp_locus_mode$mode[order(tmp_locus_mode$index)]
                        
                        ## check that A are diploid locus
                        if(!all(which(locus == "A") %in% 
                                c(microsat_dip_locus, seq_dip_locus))) {
                            err <- append(
                                err,
                                str_c(
                                    "Issue with Microsat/Sequences data file",
                                    "content:",
                                    "Autosomal diploid (A)",
                                    "should all be diploid loci.",
                                    sep = " "
                                )
                            )
                            valid <- FALSE
                        }
                        
                        ## check that H, M and Y are haploid locus
                        if(!all(which(locus %in% c("H", "M", "Y")) %in% 
                                c(microsat_hap_locus, seq_hap_locus))) {
                            err <- append(
                                err,
                                str_c(
                                    "Issue with Microsat/Sequences data file",
                                    "content:",
                                    "Autosomal haploid (H), Y-linked (Y)",
                                    "and Mitochondrial (M) loci",
                                    "should all be haploid loci.",
                                    sep = " "
                                )
                            )
                            valid <- FALSE
                        }
                    }
                    
                    ## check seq locus length
                    seq_length <- unlist(lapply(
                        1:n_loci,
                        function(col_ind) {
                            if(col_ind %in% c(seq_hap_locus, seq_dip_locus,
                                              seq_x_locus)) {
                                
                                tmp <- unlist(lapply(
                                    1:n_indiv,
                                    function(row_ind) {
                                        return(
                                            str_length(str_extract_all(
                                                locus_data[row_ind,col_ind],
                                                "\\[[ATCG]*\\]", 
                                                simplify = TRUE
                                            )) - 2
                                        )
                                    }
                                ))
                                
                                out <- max(tmp)
                                if(!all(unique(tmp) %in% c(0, out))) {
                                    return(-100)
                                } else {
                                    return(out)
                                }
                            } else {
                                return(NA)
                            }
                        }
                    ))
                    
                    if(any(seq_length[!is.na(seq_length)] == -100)) {
                        err <- append(
                            err,
                            str_c(
                                "Issue with Microsat/Sequences data file",
                                "content:",
                                "non-missing sequence data attached to",
                                "following loci",
                                str_c(which(seq_length == -100), 
                                      collapse = ", "),
                                "do not have the same length in all",
                                "individuals",
                                sep = " "
                            )
                        )
                        valid <- FALSE
                    }
                }
            }
        }
        
        # output
        spec <- lst(locus, locus_mode, locus_name, seq_length, 
                    n_indiv, n_loci, n_pop, pop_size)
    }
    ## expected data file ?
    if(!is.null(expected_data_file)) {
        if(data_file != expected_data_file) {
            err <- append(
                err, 
                str_c(
                    "Data file expected by provided 'header.txt'",
                    "or 'headerRF.txt' file(s)",
                    "and data file provided by user are different.",
                    sep = " "
                )
            )
            valid <- FALSE
        }
    }
    ## output    
    out <- lst(header, info, spec, valid, err, msg)
    return(out)
}

#' Parse diyabc header file
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Content: see doc
#' @param file_name string, (server-side) path to a header file.
#' @param file_type string, MIME file type.
#' @param locus_type string, `"mss"` for MicroSat/Sequence or `"snp"` for SNP.
parse_diyabc_header <- function(file_name, file_type, locus_type) {
    # init output
    data_file <- NULL
    issues <- list()
    loci_description <- NULL
    n_loci_des <- NULL
    n_param <- NULL
    n_prior <- NULL
    n_sumstat <- NULL
    raw_cond_list <- NULL
    raw_prior_list <- NULL
    raw_group_prior_list <- NULL
    raw_scenario_list <- NULL
    simu_mode <- NULL
    valid <- TRUE
    # check file name
    valid <- check_file_name(file_name)
    # check file type
    if(file_type != "text/plain")
        valid <- FALSE
    ## read file
    if(valid) {
        # local variable
        next_sec_line <- 3
        # file content
        raw_content <- readLines(file_name, warn = FALSE)
        raw_content <- raw_content[raw_content != ""]
        ## data file
        data_file <- raw_content[1]
        ## number of paramters and statistics
        strng <- raw_content[2]
        pttrn <- "^[0-9]+ parameters and [0-9]+ summary statistics$"
        if(!str_detect(strng, pttrn)) {
            issues <- append(issues, pttrn)
            valid <- FALSE
        } else {
            pttrn <- "[0-9]+(?= parameters)"
            n_param <- as.integer(str_extract(strng, pttrn))
            pttrn <- "[0-9]+(?= summary statistics)"
            n_sumstat <- as.integer(str_extract(strng, pttrn))
        }
        
        ## scenarios
        pttrn <- "^[0-9]+ scenarios:( [0-9]+)+ ?$"
        # find section
        if(!any(str_detect(raw_content, pttrn))) {
            issues <- append(issues, pttrn)
            valid <- FALSE
        } else if(!(which(str_detect(raw_content, pttrn)) == next_sec_line)) {
            issues <- append(issues, pttrn)
            valid <- FALSE
        } else {
            strng <- raw_content[3]
            pttrn <- "[0-9]+(?= scenarios:)"
            n_scenario <- as.integer(str_extract(strng, pttrn))
            pttrn <- "(?<= )[0-9]+"
            lines_per_scenario <- as.integer(unlist(str_extract_all(strng, 
                                                                    pttrn)))
            # extract scenarii
            line_seq <- cumsum(c(1, lines_per_scenario+1))
            scenario_list <- lapply(
                split(
                    raw_content[(min(line_seq):(max(line_seq)-1)) 
                                + next_sec_line], 
                    rep(seq(line_seq), diff(c(line_seq, max(line_seq))))
                ), 
                function(content) {
                    parse_diyabc_header_scenarii(content)
                })
            # check extracted scnenarii
            valid <- all(unlist(lapply(scenario_list, 
                                       function(item) item$valid)))
            # extract raw scenario list
            raw_scenario_list <- unlist(unname(
                lapply(
                    scenario_list, 
                    function(item) item$raw_scenario
                )
            ))
            # next section
            next_sec_line <- next_sec_line + max(line_seq)
        }
        
        ## historical parameters
        pttrn <- "^historical parameters priors \\([0-9]+,[0-9]+\\)$"
        # find section
        if(!any(str_detect(raw_content, pttrn))) {
            issues <- append(issues, pttrn)
            valid <- FALSE
        } else if(!(which(str_detect(raw_content, pttrn)) == next_sec_line)) {
            issues <- append(issues, pttrn)
            valid <- FALSE
        } else {
            strng <- raw_content[next_sec_line]
            next_sec_line <- next_sec_line + 1
            # number of priors
            pttrn <- "(?<= \\()[0-9]+"
            n_prior <- as.integer(str_extract(strng, pttrn))
            # number of conditions
            pttrn <- "[0-9]+(?=\\)$)"
            n_cond <- as.integer(str_extract(strng, pttrn))
            # extract priors
            raw_prior_list <- raw_content[next_sec_line:(next_sec_line 
                                                         + n_prior - 1)]
            next_sec_line <- next_sec_line + n_prior
            # check extracted priors
            valid <- all(unlist(lapply(raw_prior_list, check_header_prior)))
            # extract conditions
            if(n_cond > 0) {
                raw_cond_list <- raw_content[next_sec_line:(next_sec_line 
                                                            + n_cond - 1)]
                next_sec_line <- next_sec_line + n_cond
                # check extracted conditions
                valid <- all(unlist(lapply(raw_cond_list, check_header_cond)))
            }
            # generation mode
            simu_mode <- raw_content[next_sec_line]
            next_sec_line <- next_sec_line + 1
        }
        
        ## loci description
        pttrn <- "^loci description \\([0-9]+\\)$"
        # find section
        if(!any(str_detect(raw_content, pttrn))) {
            issues <- append(issues, pttrn)
            valid <- FALSE
        } else if(!(which(str_detect(raw_content, pttrn)) == next_sec_line)) {
            issues <- append(issues, pttrn)
            valid <- FALSE
        } else {
            strng <- raw_content[next_sec_line]
            next_sec_line <- next_sec_line + 1
            # number of loci description
            pttrn <- "(?<=^loci description \\()[0-9]+"
            n_loci_des <- as.integer(str_extract(strng, pttrn))
            # extract loci description
            loci_description <- raw_content[next_sec_line:(next_sec_line 
                                                           + n_loci_des - 1)]
            next_sec_line <- next_sec_line + n_loci_des
            # check loci description
            valid <- all(unlist(lapply(loci_description, 
                                       check_header_loci_des)))
        }
        
        ## group prior (for microsat/sequence)
        if(locus_type == "mss") {
            pttrn <- "^group priors \\([0-9]+\\)$"
            # find section
            if(!any(str_detect(raw_content, pttrn))) {
                issues <- append(issues, pttrn)
                valid <- FALSE
            } else if(!(which(str_detect(raw_content, pttrn)) == next_sec_line)) {
                issues <- append(issues, pttrn)
                valid <- FALSE
            } else {
                strng <- raw_content[next_sec_line]
                next_sec_line <- next_sec_line + 1
                # check next section
                pttrn <- "^group summary statistics \\([0-9]+\\)$"
                tmp_next <- head(which(str_detect(raw_content, pttrn)), 1)
                if(length(tmp_next) == 0) {
                    issues <- append(issues, pttrn)
                    valid <- FALSE
                } else if(tmp_next <= next_sec_line) {
                    issues <- append(issues, pttrn)
                    valid <- FALSE
                } else {
                    # extract info
                    raw_group_prior_list <- raw_content[next_sec_line:(tmp_next-1)]
                    
                    # next section
                    next_sec_line <- tmp_next
                }
            }
        }
        
        ## group summary statistics
        pttrn <- "^group summary statistics \\([0-9]+\\)$"
        # find section
        if(!any(str_detect(raw_content, pttrn))) {
            issues <- append(issues, pttrn)
            valid <- FALSE
        } else if(!(head(which(str_detect(raw_content, pttrn)), 1) 
                    == next_sec_line)) {
            issues <- append(issues, pttrn)
            valid <- FALSE
        } else {
            strng <- raw_content[next_sec_line:length(raw_content)]
            next_sec_line <- next_sec_line + 1
            # number of summary stats
            pttrn <- "(?<=^group summary statistics \\()[0-9]+"
            tmp_n_sumstat <- sum(as.integer(str_extract(strng, pttrn)), 
                                 na.rm = TRUE)
            # check
            valid <- (n_sumstat == tmp_n_sumstat)
            # TODO: parse end of file
        }
    }
    ## output
    return(lst(data_file, loci_description, 
               n_loci_des, n_param, n_prior, n_sumstat, 
               raw_cond_list, raw_prior_list, raw_group_prior_list, 
               raw_scenario_list, simu_mode, valid))
}

#' Parse diyabc header file scenarii
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Content: see doc
#' @param content string vector, scenario description
parse_diyabc_header_scenarii <- function(content) {
    # init output
    issues <- list()
    id <- NULL
    n_param <- NULL
    prior <- NULL
    raw_scenario <- NULL
    valid <- TRUE
    # first line
    strng <- content[1]
    pttrn <- str_c("^scenario [0-9]+ \\[", num_regex(), "\\] \\([0-9]+\\)$")
    if(!str_detect(strng, pttrn)) {
        issues <- append(issues, pttrn)
        valid <- FALSE
    } else {
        # scenario id
        pttrn <- "(?<=^scenario )[0-9]+"
        id <- as.integer(str_extract(strng, pttrn))
        # scenario prior
        pttrn <- str_c("(?<= \\[)", num_regex(), "(?=\\] )")
        prior <- as.numeric(str_extract(strng, pttrn))
        # number of parameters in scenario
        pttrn <- "(?<= \\()[0-9]+(?=\\)$)"
        n_param <- as.integer(str_extract(strng, pttrn))
        ## raw scenario
        raw_scenario <- str_c(content[-1], collapse = "\n")
    }
    ## output
    return(lst(id, n_param, prior, raw_scenario, valid))
}

#' Check header file prior definition
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Content: see doc
#' @param cstrng string, prior description.
check_header_prior <- function(strng) {
    # init output
    issues <- list()
    valid <- TRUE
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
    # init output
    issues <- list()
    valid <- TRUE
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
check_header_loci_des <- function(strng, type = "mss") {
    # init output
    issues <- list()
    valid <- TRUE
    # init local
    pttrn <- NULL
    # Microsat/Sequence
    if(type == "mss") {
        # Locus_M_A_1_ <A> [M] G1 2 40
        # Locus_S_A_21_ <A> [S] G4 1000
        pttrn <- str_c("^", single_param_regex(), " ",
                       "<(A|H|X|Y|M)>", " ",
                       "\\[(M|S)\\]", " ",
                       "G[0-9]+", " ",
                       "[0-9]+", "( [0-9]+)?", "$")
    } else if(type == "snp") {
        # 5000 <A> G1 from 1
        pttrn <- str_c("^", "[0-9]+", " ",
                       "<(A|H|X|Y|M)>", " ",
                       "G[0-9]+", " ", 
                       "from ", "[0-9]+", "$")
    }
    # check
    valid <- str_detect(strng, pttrn)
    ## output
    return(valid)
}

#' Parse diyabc simulation header file
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Content: see doc
#' @param file_name string, (server-side) path to a headersim file.
#' @param type string, MIME file type.
parse_diyabc_headersim <- function(file_name, type) {
    
}