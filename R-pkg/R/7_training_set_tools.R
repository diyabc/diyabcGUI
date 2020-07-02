#' Write training set simulation diyabc header file
#' @keywords internal
#' @author Ghislain Durif
write_header <- function(proj_dir, data_file, 
                         scenario_list, param_count_list, 
                         param_list, cond_list, 
                         locus_type, seq_mode, locus, 
                         mss_locus, mss_group_prior) {
    
    # FIXME check input
    
    # # debugging
    # print("------ write_header input")
    # print("proj_dir =")
    # print(proj_dir)
    # print("param_list =")
    # print(param_list)
    # print("param_count_list =")
    # print(param_count_list)
    # print("scenario_list =")
    # print(scenario_list)
    # print("cond_list =")
    # print(cond_list)
    # print("data_file =")
    # print(data_file)
    # print("locus_type =")
    # print(locus_type)
    # print("seq_mode =")
    # print(seq_mode)
    # print("locus =")
    # print(locus)
    # print("mss_locus =")
    # print(mss_locus)
    # print("mss_group_prior =")
    # print(mss_group_prior)
    
    out <- NULL
    
    filename <- "header.txt"
    
    # print("log1")
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
    
    # print("log2")
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
    
    # print("log3")
    ## historical parameters priors
    sec3 <- str_c(
        str_c("historical parameters priors ",
              "(", length(param_list), ",", length(cond_list), ")"),
        str_c(param_list, collapse = "\n"),
        sep = "\n"
    )
    if(!is.null(cond_list)) {
        sec3 <- str_c(
            sec3,
            str_c(cond_list, collapse = "\n"),
            sep = "\n"
        )
    }
    sec3 <- str_c(
        sec3,
        "DRAW UNTIL",
        sep = "\n"
    )
    
    # print("log4")
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
    
    # print("log5")
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
    
    # print("log6")
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
        
        summary_stat <- c(microsat_summary, seq_summary)
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
        "-R \"\"", "-m",
        "-g", as.character(getOption("diyabcGUI")$simu_loop_size), 
        "-r", n_run,
        "-t", as.character(getOption("diyabcGUI")$ncore)
    )
    if(run_prior_check) {
        arguments <- c(
            arguments,
            "-d a:pl"
        )
    }
    
    run_proc <- processx::process$new(
        command = diyabc_bin, 
        args = arguments,
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
        c("diyabc_seed_init_call.log", "diyabc_run_call.log",
          "header.txt", "RNG_state_0000.bin")
    )
    # remove files
    lapply(files, function(filename) {
        if(file.exists(filename)) {
            fs::file_delete(filename)
        }
    })
}