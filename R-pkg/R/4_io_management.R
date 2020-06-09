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
#' @param data_file string, path to data file.
#' @param locus_type string, locus type `"mss"` or `"snp"`.
#' @param seq_mode string, `"indseq"` or `"poolseq"`.
#' @param expected_data_file string, default is NULL.
#' @importFrom tools file_ext
check_data_file <- function(data_file, data_dir, locus_type, seq_mode, 
                            expected_data_file = NULL) {
    header <- NULL
    info <- NULL
    msg <- list()
    spec <- NULL
    valid <- TRUE
    # ## debugging
    # logging("data_file = ", data_file)
    ## file exists?
    if(!file.exists(data_file)) {
        msg <- append(msg, "Input file does not exist")
        valid <- FALSE
    # snp locus
    } else if((locus_type == "snp") & (seq_mode == "indseq")) {
        # init output
        locus <- NULL
        n_loci <- NULL
        n_pop <- NULL
        n_indiv <- NULL
        # check file type
        if(tools::file_ext(data_file) != "snp") {
            # FIXME
            msg <- append(msg, "Only SNP files are managed at the moment")
            valid <- FALSE
        } else {
            # header
            info <- readLines(data_file, n = 1)
            msg <- append(
                msg,
                str_extract(info, pattern = "(?<=<)MAF=.*(?=>)")
            )
            # header
            header <- read.table(file = data_file, skip = 1, nrows = 1)
            # nb of locus
            n_loci <- length(header) - 3
            msg <- append(
                msg,
                str_c(n_loci, "loci", sep = " ")
            )
            # content
            content <- read.table(file = data_file, skip = 2)
            n_pop <- length(unique(content[,3]))
            n_indiv <- nrow(content)
            msg <- append(
                msg,
                str_c(n_indiv, "individuals from", n_pop, "populations", 
                      sep = " ")
            )
            # locus type
            candidate_locus <- c("A", "H", "X", "Y", "M")
            locus <- unlist(lapply(candidate_locus, function(type) {
                count <- str_count(str_c(header[-(1:3)], collapse = " "), 
                                   pattern = type)
                if(count > 0)
                    return(str_c(count,
                                 " <", type, ">"))
            }))
            msg <- append(
                msg,
                str_c("loci:", str_c(locus, collapse =  ", "), sep = " ")
            )
            # fix header
            header <- str_c(header, collapse = " ")
        }
        # output
        spec <- lst(locus, n_indiv, n_loci, n_pop)
        # mss locus
    } else if((locus_type == "snp") & (seq_mode == "poolseq")) {
        warning("Not implemented yet")
        # FIXME
    } else if(locus_type == "mss") {
        warning("Not implemented yet")
        # FIXME
    }
    # expected data file ?
    if(!is.null(expected_data_file)) {
        valid <- valid & (data_file == file.path(data_dir, expected_data_file))
    }
    ## output    
    out <- lst(header, info, msg, spec, valid)
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