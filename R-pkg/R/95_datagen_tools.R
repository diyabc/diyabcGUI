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
                  str_c(nrow(sample_sizes), "samples", sep = " "), 
                  sep = "\n")
    
    sample_string <- str_c(apply(sample_sizes, 1, str_c, collapse = " "),
                           collapse = "\n")
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
diyabc_run_simu <- function(project_dir, n_core = 1) {
    diyabc_bin <- find_bin("diyabc")
    # check project dir
    if(!dir.exists(project_dir)) {
        stop("Input directory does not exist")
    }
    if(!str_detect(string = project_dir, pattern = "/$")) {
        project_dir <- str_c(project_dir, "/")
    }
    # init seeds
    cmd <- str_c(diyabc_bin, 
                 "-p", project_dir, "-n",
                 str_c("'t:", n_core, "'"),
                 sep = " ")
    check <- system(cmd)
    if(check != 0) {
        warning("Issue with seed initialization")
    }
    # run
    cmd <- str_c(diyabc_bin, "-p", project_dir, "-k", sep = " ")
    check <- system(cmd)
    if(check != 0) {
        stop("Issue with simulation run")
    }
    # output
    return(NULL)
}
