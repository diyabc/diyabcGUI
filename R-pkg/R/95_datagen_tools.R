#' Write simulation diyabc header file
#' @keywords internal
#' @author Ghislain Durif
#' @param raw_scenario string, historical scenario.
#' @param raw_param string, parameter value setting.
#' @param locus_description list of locus description.
#' @param nrep integer, number of repetitions.
#' @param sex_ratio float, between 0 and 1.
#' @param sample_sizes list of integer sample sizes.
write_headersim <- function(project_name, project_dir, seq_mode, locus_type,
                            raw_scenario, raw_param, locus_description, 
                            mss_group_prior, n_group,
                            sample_sizes,
                            n_rep = 1, sex_ratio = 0.5) {
    
    # FIXME check input
    
    out <- NULL
    
    filename <- "headersim.txt"
    
    ## line 1 and samples
    sec1 <- str_c(project_name, n_rep, sex_ratio, sep = " ")
    # FIXME
    if(seq_mode == "poolseq") {
        sec1 <- str_c("poolseq", sec1, sep = "_")
    } else if(seq_mode == "indseq" & locus_type == "snp") {
        sec1 <- str_c("SNP", sec1, sep = "_")
    }
    sec1 <- str_c(sec1, 
                  str_c(length(sample_sizes), "samples", sep = " "), 
                  sep = "\n")
    
    sample_string <- str_c(sample_sizes, collapse = "\n")
    sec1 <- str_c(sec1, sample_string, sep = "\n")
    
    ## scenario
    # FIXME
    sec2 <- str_c("scenario ", 
                  "(", str_count(string = raw_scenario, pattern = "\n") + 1,
                  ")\n",
                  raw_scenario)
    ## historical parameters
    sec3 <- str_c("historical parameters ", 
                  "(", length(raw_param), ")\n",
                  str_c(raw_param, collapse = "\n"))
    ## loci description
    total_locus_count <- NULL
    
    if(locus_type == "mss") {
        total_locus_count <- length(locus_description)
    } else if(locus_type == "snp") {
        total_locus_count <- sum(as.numeric(str_extract(
            string = locus_description, pattern = "^[0-9]+"
        )))
    } else {
        stop("locus_type not supported")
    }
    
    sec4 <- str_c("loci description ",
                  "(", total_locus_count, ")\n",
                  str_c(locus_description, collapse = "\n"))
    
    ## mss group prior (if relevant)
    sec5 <- NULL
    if(locus_type == "mss") {
        sec5 <- str_c(
            str_c("groups (", n_group, ")"),
            str_c(mss_group_prior, collapse = "\n"),
            sep = "\n"
        )
    }
    
    ## merge
    out <- str_c(sec1, sec2, sec3, sec4, sec5, sep = "\n\n")
    out <- str_c(out, "\n\n")
    
    ## write to file
    writeLines(out, file.path(project_dir, "headersim.txt"))
}

#' Run simulation
#' @keywords internal
#' @author Ghislain Durif
diyabc_run_datagen <- function(proj_dir) {
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
    
    # check for headersim file
    if(! "headersim.txt" %in% list.files(proj_dir)) {
        stop("missing headersim input file")
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
    logging("diyabc simu run")
    arguments <- c(
        "-p", safe_proj_dir, 
        "-k",
        "-t", as.character(getOption("diyabcGUI")$ncore)
    )
    
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
