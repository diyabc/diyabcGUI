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
    ## snp locus / indseq
    if((locus_type == "snp") & (seq_mode == "indseq")) {
        out <- check_indseq_snp_data_file(data_file, data_dir, 
                                          expected_data_file)
    ## snp locus / poolseq
    } else if((locus_type == "snp") & (seq_mode == "poolseq")) {
        out <- check_poolseq_snp_data_file(data_file, data_dir, 
                                           expected_data_file)
    ## mss locus
    } else if(locus_type == "mss") {
        warning("Not implemented yet")
        # FIXME
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
    
    ## file exists?
    if(!file.exists(data_path)) {
        err <- append(err, "Input file does not exist")
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
            info <- readLines(data_path, n = 1)
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
    
    ## file exists?
    if(!file.exists(data_path)) {
        err <- append(err, "Input file does not exist")
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
            info <- readLines(data_path, n = 1)
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

#' Parse diyabcGUI project file
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Standard name: "diyabcGUI_proj.txt"
#' Content:
#' ```
#' project_name: <project_name>
#' ```
#' @param file_name string, (server-side) path to a project file.
#' @param type string, MIME file type.
parse_diyabc_project <- function(file_name, file_type) {
    # init output
    issues <- list()
    proj_name <- NULL
    valid <- TRUE
    # check file name
    valid <- check_file_name(file_name)
    # check file type
    if(file_type != "text/plain")
        valid <- FALSE
    ## read file
    if(valid) {
        raw_content <- readLines(file_name)
        ## extract project name
        strng <- raw_content[1]
        pttrn <- "(?<=^project_name=).*$"
        if(!str_detect(strng, pttrn)) {
            issues <- append(issues, pttrn)
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
#' @param file_name string, (server-side) path to a header file.
#' @param file_type string, MIME file type.
#' @param data_type string, `"mss"` for MicroSat/Sequence or `"snp"` for SNP.
parse_diyabc_header <- function(file_name, file_type, data_type) {
    # init output
    data_file <- NULL
    issues <- list()
    loci_description <- NULL
    n_loci_des <- NULL
    n_param <- NULL
    n_sumstat <- NULL
    raw_cond_list <- NULL
    raw_prior_list <- NULL
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
        raw_content <- readLines(file_name)
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
                    raw_content[(min(line_seq):(max(line_seq)-1)) + next_sec_line], 
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
            raw_prior_list <- raw_content[next_sec_line:(next_sec_line + n_prior - 1)]
            next_sec_line <- next_sec_line + n_prior
            # check extracted priors
            valid <- all(unlist(lapply(raw_prior_list, check_header_prior)))
            # extract conditions
            raw_cond_list <- raw_content[next_sec_line:(next_sec_line + n_cond - 1)]
            next_sec_line <- next_sec_line + n_cond
            # check extracted conditions
            valid <- all(unlist(lapply(raw_cond_list, check_header_cond)))
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
            loci_description <- raw_content[next_sec_line:(next_sec_line + n_loci_des - 1)]
            next_sec_line <- next_sec_line + n_loci_des
            # check loci description
            valid <- all(unlist(lapply(loci_description, check_header_loci_des)))
        }
        ## group summary statistics
        pttrn <- "^group summary statistics \\([0-9]+\\)$"
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
            # number of summary stats
            pttrn <- "(?<=^group summary statistics \\()[0-9]+"
            tmp_n_sumstat <- as.integer(str_extract(strng, pttrn))
            # check
            valid <- (n_sumstat == tmp_n_sumstat)
            # TODO: parse end of file
        }
    }
    ## output
    return(lst(data_file, loci_description, 
               n_loci_des, n_param, n_sumstat, 
               raw_cond_list, raw_prior_list, raw_scenario_list, 
               simu_mode, valid))
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