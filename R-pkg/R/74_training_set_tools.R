#' Write training set simulation diyabc header file
#' @keywords internal
#' @author Ghislain Durif
write_header <- function(proj_dir, data_file, 
                         scenario_list, param_count_list, 
                         param_list, cond_list, 
                         locus_type, seq_mode, locus, 
                         mss_locus, mss_group_prior,
                         mss_rf_col_name) {
    
    # FIXME check input
    
    # # debugging
    # pprint("------ write_header input")
    # pprint("proj_dir =")
    # pprint(proj_dir)
    # pprint("param_list =")
    # pprint(param_list)
    # pprint("param_count_list =")
    # pprint(param_count_list)
    # pprint("scenario_list =")
    # pprint(scenario_list)
    # pprint("cond_list =")
    # pprint(cond_list)
    # pprint("data_file =")
    # pprint(data_file)
    # pprint("locus_type =")
    # pprint(locus_type)
    # pprint("seq_mode =")
    # pprint(seq_mode)
    # pprint("locus =")
    # pprint(locus)
    # pprint("mss_locus =")
    # pprint(mss_locus)
    # pprint("mss_group_prior =")
    # pprint(mss_group_prior)
    # pprint("mss_rf_col_name")
    # pprint(mss_rf_col_name)
    
    out <- NULL
    
    filename <- "header.txt"
    
    # pprint("log1")
    ## data filename and summary
    n_stat <- NULL
    if(locus_type == "snp") {
        n_stat <- 1
    } else if(locus_type == "mss") {
        n_stat <- sum(str_detect(
            mss_group_prior,
            "^group G[0-9]+ \\[[MS]\\]"
        ))
    } else {
        stop("Issue with 'locus_type' input argument")
    }
    sec1 <- str_c(basename(data_file),
                  str_c(length(param_list), "parameters and", 
                        n_stat, "summary statistics", 
                        sep = " "),
                  sep = "\n")
    
    # pprint("log2")
    ## scenario
    sec2 <- str_c(length(scenario_list), "scenarios:",
                  str_c(str_count(string = scenario_list, 
                                  pattern = "\n") + 1, 
                        collapse = " "),
                  sep = " ")
    format_scenario <- lapply(
        1:length(scenario_list),
        function(ind) {
            tmp_scenario <- scenario_list[[ind]]
            tmp_prior <- round(1/length(scenario_list), digits = 5)
            tmp_param_count <- param_count_list[[ind]]
            return(
                str_c(
                    str_c(
                        "scenario", ind, 
                        str_c("[", tmp_prior, "]"),
                        str_c("(", tmp_param_count, ")"),
                        sep = " "
                    ),
                    str_c(tmp_scenario, collapse = "\n"),
                    sep = "\n"
                )
            )
        }
    )
    sec2 <- str_c(sec2,
                  str_c(format_scenario, collapse = "\n"),
                  sep = "\n")
    
    # pprint("log3")
    ## historical parameters priors
    sec3 <- str_c(
        str_c("historical parameters priors ",
              "(", length(param_list), ",", length(cond_list), ")"),
        str_c(param_list, collapse = "\n"),
        sep = "\n"
    )
    if(!is.null(cond_list)) {
        if(length(cond_list) > 0) {
            sec3 <- str_c(
                sec3,
                str_c(cond_list, collapse = "\n"),
                "DRAW UNTIL",
                sep = "\n"
            )
        }
    }
    
    # pprint("log4")
    ## loci description
    sec4 <- NULL
    if(locus_type == "snp") {
        sec4 <- str_c(
            str_c(
                "loci description",
                str_c("(", length(locus), ")"),
                sep = " "
            ),
            str_c(locus, collapse = "\n"),
            sep = "\n"
        )
    } else if(locus_type == "mss") {
        if(is.null(mss_locus)) {
            stop("missing 'mss_locus' input argument")
        }
        sec4 <- str_c(
            str_c(
                "loci description",
                str_c("(", length(mss_locus), ")"),
                sep = " "
            ),
            str_c(mss_locus, collapse = "\n"),
            sep = "\n"
        )
    }
    
    ## group prior
    sec4b <- NULL
    if(locus_type == "mss") {
        if(is.null(mss_group_prior)) {
            stop("missing 'mss_group_prior' input argument")
        }
        sec4b <- str_c(
            str_c(
                "group priors",
                str_c("(", 
                      sum(str_detect(mss_group_prior, "group G")), 
                      ")"),
                sep = " "
            ),
            str_c(mss_group_prior, collapse = "\n"),
            sep = "\n"
        )
    }
    
    # pprint("log5")
    ## group summary statistics
    sec5 <- NULL
    microsat_group <- NULL
    seq_group <- NULL
    if(locus_type == "snp" & seq_mode == "indseq") {
        sec5 <- str_c(
            "group summary statistics (1)",
            "group G1 (1)",
            "HWm 1",
            sep = "\n"
        )
    } else if(locus_type == "snp" & seq_mode == "poolseq") {
        sec5 <- str_c(
            "group summary statistics (1)",
            "group G1 (1)",
            "HWm 1",
            sep = "\n"
        )
    } else if(locus_type == "mss") {
        sec5 <- str_c(
            "group summary statistics",
            str_c("(", 
                  sum(str_detect(mss_group_prior, "group G")), 
                  ")"),
            sep = " "
        )
        
        microsat_group <- str_extract(
            mss_group_prior,
            "^group G[0-9]+ \\[M\\]"
        )
        if(sum(!is.na(microsat_group)) > 0) {
            sec5 <- str_c(
                sec5,
                str_c(
                    unlist(lapply(
                        microsat_group[!is.na(microsat_group)],
                        function(item) return(
                            str_c(
                                str_c(item, "(1)", sep = " "),
                                "NAL 1", sep =  "\n"
                            )
                        )
                    )),
                    collapse = "\n"
                ),
                sep = "\n"
            )
        }
        
        seq_group <- str_extract(
            mss_group_prior,
            "^group G[0-9]+ \\[S\\]"
        )
        if(sum(!is.na(seq_group)) > 0) {
            sec5 <- str_c(
                sec5,
                str_c(
                    unlist(lapply(
                        seq_group[!is.na(seq_group)],
                        function(item) return(
                            str_c(
                                str_c(item, "(1)", sep = " "),
                                "NHA 1", sep =  "\n"
                            )
                        )
                    )),
                    collapse = "\n"
                ),
                sep = "\n"
            )
        }
    }
    
    # pprint("log6")
    ## final summary
    summary_stat <- NULL
    
    if(locus_type == "snp" & seq_mode == "indseq") {
        summary_stat <- "HWm_1"
    } else if(locus_type == "snp" & seq_mode == "poolseq") {
        summary_stat <- "HWm_1"
    } else if(locus_type == "mss") {
        microsat_summary <- NULL
        seq_summary <- NULL
        
        if(sum(!is.na(microsat_group)) > 0) {
            tmp_microsat_group <- str_extract(
                microsat_group[!is.na(microsat_group)],
                "(?<=^group G)[0-9]+(?= \\[M\\])"
            )
            microsat_summary <- str_c("NAL_", tmp_microsat_group, "_1")
        }
        if(sum(!is.na(seq_group)) > 0) {
            tmp_seq_group <- str_extract(
                seq_group[!is.na(seq_group)],
                "(?<=^group G)[0-9]+(?= \\[S\\])"
            )
            seq_summary <- str_c("NHA_", tmp_seq_group, "_1")
        }
        
        summary_stat <- c(mss_rf_col_name, microsat_summary, seq_summary)
    }
    
    sec6 <- str_c(
        str_c(
            str_pad(
                c(
                    "scenario",
                    str_extract(
                        string = param_list, 
                        pattern = str_c("^", single_param_regex(), "(?= )")
                    ),
                    summary_stat
                ),
                width = 14,
                side = "right"
            ),
            collapse = ""
        ),
        sep = ""
    )
    
    ## merge
    out <- NULL
    if(locus_type == "snp") {
        out <- str_c(sec1, sec2, sec3, sec4, sec5, sec6, sep = "\n\n")
    } else if(locus_type == "mss") {
        out <- str_c(sec1, sec2, sec3, sec4, sec4b, sec5, sec6, sep = "\n\n")
    }
    
    out <- str_c(out, "\n\n")

    ## write to file
    writeLines(out, file.path(proj_dir, filename))
}

#' Run training set simulation
#' @keywords internal
#' @author Ghislain Durif
diyabc_run_trainset_simu <- function(proj_dir, n_run = 100, 
                                     run_prior_check = FALSE) {
    # executable
    diyabc_bin <- find_bin("diyabc")
    
    # check project dir
    if(!dir.exists(proj_dir)) {
        stop("Input directory does not exist")
    }
    safe_proj_dir <- proj_dir
    if(!str_detect(string = proj_dir, 
                   pattern = str_c(.Platform$file.sep, "$"))) {
        safe_proj_dir <- str_c(proj_dir, .Platform$file.sep)
    }
    
    # check for header file (headerRF.txt copied to header.txt)
    if(!any(c("header.txt", "headerRF.txt") %in% list.files(proj_dir))) {
        stop("missing header input file")
    } else if(! "header.txt" %in% list.files(proj_dir)) {
        fs::file_copy(path = file.path(proj_dir, "headerRF.txt"),
                      new_path = file.path(proj_dir, "header.txt"),
                      overwrite = TRUE)
    }
    
    # remove seed file if existing
    if(file.exists(file.path(proj_dir, "RNG_state_0000.bin"))) {
        fs::file_delete(file.path(proj_dir, "RNG_state_0000.bin"))
    }
    
    ### init seeds
    logging("diyabc init")
    arguments <- c(
        "-p", safe_proj_dir,
        "-n", str_c("'t:", getOption("diyabcGUI")$ncore, "'")
    )
    init_proc <- processx::process$new(
        command = diyabc_bin, 
        args = arguments,
        stdin = NULL,
        stdout = file.path(proj_dir, "diyabc_seed_init_call.log"), 
        stderr = file.path(proj_dir, "diyabc_seed_init_call.log"),
        echo_cmd = TRUE
    )
    init_proc$wait()
    
    # exit check
    init_check <- init_proc$get_exit_status()
    # logging("diyabc init", init_check)
    if(init_check != 0) {
        warning("Issue with seed initialization")
    }
    
    ### run
    logging("diyabc run")
    arguments <- c(
        "-p", safe_proj_dir, 
        "-R", "", "-m",
        "-g", as.character(getOption("diyabcGUI")$simu_loop_size), 
        "-r", as.character(n_run),
        "-t", as.character(getOption("diyabcGUI")$ncore)
    )
    if(run_prior_check) {
        arguments <- c(
            arguments,
            "-d", "a:pl"
        )
    }
    
    run_proc <- processx::process$new(
        command = diyabc_bin, 
        args = arguments,
        stdin = NULL,
        stdout = file.path(proj_dir, "diyabc_run_call.log"), 
        stderr = file.path(proj_dir, "diyabc_run_call.log"),
        echo_cmd = TRUE
    )
    
    ## output process
    return(run_proc)
}

#' Clean up after diyabc
#' @keywords internal
#' @author Ghislain Durif
cleanup_diyabc_run <- function(project_dir) {
    ## file list
    files <- file.path(
        project_dir,
        # c("diyabc_seed_init_call.log", "diyabc_run_call.log",
        c("header.txt", "RNG_state_0000.bin")
    )
    # remove files
    lapply(files, function(filename) {
        if(file.exists(filename)) {
            fs::file_delete(filename)
        }
    })
}

#' Check condition provided by users
#' @keywords internal
#' @author Ghislain Durif
check_cond <- function(cond_list, scen_list) {
    
    # init output
    out <- list(
        msg = list(), valid = TRUE
    )
    
    # check if input is not NULL
    if(length(cond_list) > 0 && length(scen_list) > 0) {
        
        # unlist
        cond_list <- unlist(cond_list)
        scen_list <- unlist(scen_list)
        
        # check condition formatting
        format_check <- str_detect(
            string = cond_list,
            pattern = str_c("^", single_param_regex(), "(<|=<|>|>=)",
                            single_param_regex(),  "$")
        )
        if(!all(format_check)) {
            out$valid <- FALSE
            msg <- tagList(
                "Syntax issue with following conditions:", 
                do.call(
                    tags$ul,
                    lapply(
                        cond_list[!format_check], 
                        function(item) tags$li(tags$code(item))
                    )
                )
            )
            out$msg <- append(out$msg, list(msg))
            return(out)
        }
        # concerned parameters
        input_param <- str_extract_all(
            string = cond_list,
            pattern = single_param_regex()
        )
        # check for duplicate
        if(length(unique(input_param)) < length(input_param)) {
            out$valid <- FALSE
            msg <- tagList("Possible duplicated conditions.")
            out$msg <- append(out$msg, list(msg))
            return(out)
        }
        # parse scenario list to get details about parameters
        scen_check <- lapply(
            scen_list, 
            function(item) return(parse_scenario(item))
        )
        scen_valid <- unlist(lapply(
            scen_check, function(item) return(item$valid)
        ))
        # check scenario
        if(!all(scen_valid)) {
            out$valid <- FALSE
            msg <- tagList("Issue with input scenario list.")
            out$msg <- append(out$msg, list(msg))
            return(out)
        }
        # check for parameter validity
        param_check <- Reduce("rbind", lapply(
            input_param, 
            function(item) {
                tmp_check <- unlist(lapply(
                    scen_check, 
                    function(subitem) {
                        return(c(
                            (length(subitem$Ne_param) > 0) && 
                                all(item %in% subitem$Ne_param),
                            (length(subitem$time_param) > 0) && 
                                all(item %in% subitem$time_param),
                            (length(subitem$rate_param) > 0) && 
                                all(item %in% subitem$rate_param)
                        ))
                    }
                ))
            }
        ))
        param_valid <- (apply(param_check, 1, sum) > 0)
        if(!all(param_valid)) {
            out$valid <- FALSE
            msg <- tagList(
                "Issue with parameter(s) in the following condition(s)",
                "(parameter(s) of different types,",
                "parameters from different models,",
                "or wrong parameters, i.e. not in any model):",
                do.call(
                    tags$ul,
                    lapply(
                        cond_list[!param_valid], 
                        function(item) tags$li(tags$code(item))
                    )
                )
            )
            out$msg <- append(out$msg, list(msg))
            return(out)
        }
    }
    
    # output
    return(out)
}

#' Check locus description provided by users
#' @keywords internal
#' @author Ghislain Durif
check_locus_desc <- function(locus_desc, data_check, locus_type) {
    # init out
    out <- list(
        valid = TRUE, msg = list(), locus_count = NULL, from = NULL
    )
    # SNP locus
    if(locus_type == "snp") {
        return(check_snp_locus_desc(locus_desc, data_check$locus_count))
        
        ## MSS locus
    } else if(locus_type == "mss") {
        valid <- check_header_locus_desc(locus_desc, locus_type)
        if(!valid) {
            out$valid <- FALSE
            msg <- tagList(
                "Bad format."
            )
            out$msg <- append(out$msg, list(msg))
            return(out)
        }
    }
    
    ## output
    return(out)
}

#' Check locus description for SNP data provided by users
#' @keywords internal
#' @author Ghislain Durif
#' @param locus_desc character string, locus description.
#' @param locus_count data.frame with columns `type` for the locus type 
#' and `available` for the number of available loci.
check_snp_locus_desc <- function(locus_desc, locus_count) {
    # init out
    out <- list(
        valid = TRUE, msg = list(), locus_count = NULL, from = NULL
    )
    
    # check one group locus desc
    if(length(locus_desc) > 1) {
        out$valid <- FALSE
        msg <- tagList(
            "The locus description should be", 
            "a one-line description for SNP data."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # check locus description content
    desc_check <- check_header_locus_desc(locus_desc, type = "snp")
    if(!desc_check) {
        out$valid <- FALSE
        msg <- tagList(
            "Bad format."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # extract locus count
    pttrn <- str_c("(?<=(^| ))", int_regex(), "(?= <)")
    count_detail <- as.numeric(unlist(str_extract_all(locus_desc, pttrn)))
    # extract locus type
    pttrn <- str_c("(?<=<)[AHXYM](?=>)")
    type_detail <- unlist(str_extract_all(locus_desc, pttrn))
    
    # check length (just in case)
    if(length(count_detail) != length(type_detail)) {
        out$valid <- FALSE
        msg <- tagList(
            "Bad format."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # check each type
    type_check <- (type_detail %in% locus_count$type[locus_count$count > 0])
    if(!all(type_check)) {
        out$valid <- FALSE
        msg <- tagList(
            "The following locus types", 
            tags$code(str_c(type_detail[!type_check], collapse = ", ")),
            "are not present in the data."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # create data frame from extracted types and count
    extracted_locus_count <- data.frame(
        type = type_detail,
        requested = count_detail,
        stringsAsFactors = FALSE
    )
    
    # check count
    tmp_locus_count <- merge(locus_count, extracted_locus_count)
    count_check <- (tmp_locus_count$requested <= tmp_locus_count$available)
    if(!all(count_check)) {
        out$valid <- FALSE
        msg <- tagList(
            "For the following locus types", 
            tags$code(
                str_c(tmp_locus_count$type[!count_check], collapse = ", ")
            ),
            ", the required number of loci to simulate is higher than", 
            "the number of available loci in the data (after filtering)."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # starting locus
    pttrn <- "(?<=from )[0-9]+$"
    start_detail <- as.integer(str_extract(locus_desc, pttrn))
    n_locus <- sum(locus_count$available)
    if(start_detail - 1 + sum(count_detail) > n_locus) {
        out$valid <- FALSE
        msg <- tagList(
            "The required starting locus and number of loci to simulate", 
            "are not compatible with",
            "the number of loci available",
            "in the data (after filtering)."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    ## format output
    out$from <- start_detail
    out$locus_count <- merge(
        data.frame(
            count = count_detail, type = type_detail,
            stringsAsFactors = FALSE
        ),
        locus_count[,c("available", "type")]
    )
    
    ## output
    return(out)
}

#' Format locus description for SNP data
#' @keywords internal
#' @author Ghislain Durif
#' @details `count` and `type` input vectors should have the same length.
#' @param count integer vector of locus count.
#' @param type integer vector of locus type 
#' (among `"A"`, `"H"`, `"X"`, `"Y"`, `"M"`).
#' @param from integer, starting locus.
format_snp_locus_desc <- function(count, type, from = 1) {
    # check input
    if(length(count) != length(type))
        stop("`count` and `type` input vectors should have the same length.")
    count <- as.integer(count)
    from <- as.integer(from)
    if(!all(type %in% c("A", "H", "X", "Y", "M")))
        stop("`type` should contain only valid locus types.")
    # formating each locus type
    type_format <- unlist(lapply(
        1:length(count),
        function(ind) return(str_c(
            as.integer(count[ind]), " <", type[ind], ">", sep = ""
        ))
    ))
    # global description
    out <- str_c(
        str_c(type_format, collapse = " "),
        "from", from, sep = " "
    )
    # output
    return(out)
}

#' Default locus description for SNP data
#' @keywords internal
#' @author Ghislain Durif
#' @param locus_count data.frame with columns `type` for the locus type 
#' and `available` for the number of available loci.
#' @param from integer, starting locus.
default_snp_locus_desc <- function(locus_count, from = 1) {
    if(!is.data.frame(locus_count) || (nrow(locus_count) == 0) ||
       !all(c("type", "available") %in% colnames(locus_count)))
        stop("Input not valid")
    # get only locus types with available loci
    locus_count <- subset(locus_count, locus_count$available > 0)
    # format
    out <- format_snp_locus_desc(locus_count$available, locus_count$type, from)
    # output
    return(out)
}

#' Clean locus description for SNP data
#' @keywords internal
#' @author Ghislain Durif
#' @param locus_count data.frame with columns `type` for the locus type 
#' and `available` for the number of available loci.
#' @param from integer, starting locus.
clean_snp_locus_desc <- function(locus_desc, locus_count) {
    
    # default
    out <- default_snp_locus_desc(locus_count, from = 1)
    
    # no input
    if(isTruthy(locus_desc)) {
        check <- check_snp_locus_desc(locus_desc, locus_count)
        if(check$valid) {
            out <- locus_desc
        }
    }
    # output
    return(out)
}

#' Default locus description for Microsat data
#' @keywords internal
#' @author Ghislain Durif
default_microsat_locus_desc <- function(locus_name, locus_type, group_id = 1) {
    return(format_microsat_locus_desc(
        locus_name, locus_type, group_id, motif = 2, range = 40
    ))
}

#' Format locus description for Microsat data
#' @keywords internal
#' @author Ghislain Durif
format_microsat_locus_desc <- function(
    locus_name, locus_type, group_id = 1, motif = 2, range = 40
) {
    # check input
    if(!(length(locus_type) %in% c(1, length(locus_name)))) {
        stop("Issue with `locus_type` input.")
    }
    if(!(length(group_id) %in% c(1, length(locus_name)))) {
        stop("Issue with `group_id` input.")
    }
    if(!(length(motif) %in% c(1, length(locus_name)))) {
        stop("Issue with `motif` input.")
    }
    if(!(length(range) %in% c(1, length(locus_name)))) {
        stop("Issue with `range` input.")
    }
    # format
    out <- str_c(
        locus_name, 
        str_c("<", locus_type, ">"), 
        "[M]",
        str_c("G", group_id),
        motif, range, sep = " "
    )
    # output
    return(out)
}

#' Format locus description for Sequence data
#' @keywords internal
#' @author Ghislain Durif
format_sequence_locus_desc <- function(
    locus_name, locus_type, group_id = 1, seq_length = 1000
) {
    # check input
    if(!(length(locus_type) %in% c(1, length(locus_name)))) {
        stop("Issue with `locus_type` input.")
    }
    if(!(length(group_id) %in% c(1, length(locus_name)))) {
        stop("Issue with `group_id` input.")
    }
    if(!(length(seq_length) %in% c(1, length(locus_name)))) {
        stop("Issue with `seq_length` input.")
    }
    # format
    out <- str_c(
        locus_name, 
        str_c("<", locus_type, ">"), 
        "[S]",
        str_c("G", group_id),
        seq_length, sep = " "
    )
    # output
    return(out)
}

#' Default locus description for MSS data
#' @keywords internal
#' @author Ghislain Durif
default_mss_locus_desc <- function(
    locus_name, locus_type, locus_mode, seq_length
) {
    # check input
    if((length(locus_name) != length(locus_type)) || 
       (length(locus_name) != length(locus_mode)) ||
       (length(locus_name) != length(seq_length))) {
        stop("All input should have the same length")
    }
    if(!is.character(locus_name) || any(str_length(locus_name) == 0)) {
        stop("Issue with 'locus_name' input")
    }
    if(!all(locus_type %in% c("A", "H", "X", "Y", "M"))) {
        stop("Issue with 'locus_type' input")
    }
    if(!all(locus_mode %in% c("M", "S"))) {
        stop("Issue with 'locus_mode' input")
    }
    if(any(locus_mode == "S") && 
       !is.numeric(seq_length) &&
       (sum(!is.na(seq_length)) != sum(locus_mode == "S"))) {
        stop("Issue with 'seq_length' input")
    }
    
    # init output
    locus_desc <- character(length(locus_name))
    
    # mask
    microsat_mask <- (locus_mode == "M")
    sequence_mask <- (locus_mode == "S")
    
    # group id
    microsat_group_id <- which(unique(locus_mode) == "M")
    sequence_group_id <- which(unique(locus_mode) == "S")
    
    # microsat default locus desc
    if(any(microsat_mask)) {
        locus_desc[microsat_mask] <- default_microsat_locus_desc(
            locus_name[microsat_mask], locus_type[microsat_mask], 
            microsat_group_id
        )
    }
    
    # sequence default locus desc
    if(any(sequence_mask)) {
        locus_desc[sequence_mask] <- format_sequence_locus_desc(
            locus_name[sequence_mask], locus_type[sequence_mask], 
            sequence_group_id, seq_length[sequence_mask]
        )
    }
    
    # output
    return(locus_desc)
}

#' Correct group in in MSS locus desc
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Renumber groups starting from a given id (to avoid empty groups)
correct_mss_locus_desc_group_id <- function(locus_desc, start_id) {
    # get current groups
    old_group_id <- unique(str_extract(locus_desc, "G[0-9]+"))
    # number of groups
    n_group <- length(old_group_id)
    # new group id encoding
    new_groupd_id <- str_c("G", (1:n_group) + (start_id - 1))
    # replace group id
    out <- locus_desc
    for(ind in 1:n_group) {
        mask <- str_detect(locus_desc, old_group_id[ind])
        out[mask] <- str_replace(
            locus_desc[mask], old_group_id[ind], new_groupd_id[ind]
        )
    }
    # output
    return(out)
}
