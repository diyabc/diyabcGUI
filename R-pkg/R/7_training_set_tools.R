#' Write training set simulation diyabc header file
#' @keywords internal
#' @author Ghislain Durif
#' @param scenario string, historical scenario.
#' @param param string, parameter value setting.
#' @param locus_type list of locus type.
write_header <- function(project_name, project_dir, data_file, 
                         scenario_list, param_count_list, 
                         param_list, cond_list, 
                         locus_type) {
    
    # FIXME check input
    
    # print("project_dir =")
    # print(project_dir)
    # print("project_name =")
    # print(project_name)
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
    
    out <- NULL
    
    filename <- "header.txt"
    
    # print("log1")
    ## data filename and summary
    sec1 <- str_c(basename(data_file),
                  str_c(length(param_list), 
                        "parameters and 2 summary statistics", 
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
        str_c(cond_list, collapse = "\n"),
        "DRAW UNTIL",
        sep = "\n"
    )
    
    # print("log4")
    ## loci description
    # FIXME
    format_locus_type <- lapply(
        1:length(locus_type),
        function(ind) {
            return(
                str_c(
                    locus_type[[ind]],
                    str_c("G", ind),
                    "from 1", # FIXME
                    sep = " "
                )
            )
        }
    )
    
    sec4 <- str_c(
        str_c(
            "loci description",
            str_c("(", length(locus_type), ")"),
            sep = " "
        ),
        str_c(format_locus_type, collapse = "\n"),
        sep = "\n"
    )
    
    # print("log5")
    ## group summary statistics
    sec5 <- str_c(
        "group summary statistics (2)",
        "group G1 (2)",
        "HP0 1 2",
        sep = "\n"
    )
    
    # print("log6")
    ## final summary
    sec6 <- str_c(
        "scenario",
        str_c(
            str_extract(
                string = param_list, 
                pattern = str_c("^", single_param_regex(), "(?= )")
            ),
            collapse = "    "
        ), 
        "HP0_1_1", "HP0_1_2",
        sep = "    "
    )
    ## merge
    # out <- str_split(str_c(sec1, sec2, sec3, sec4, sep = "\n\n"), "\n")
    out <- str_c(sec1, sec2, sec3, sec4, sec5, sec6, sep = "\n\n")
    out <- str_c(out, "\n\n")

    ## write to file
    writeLines(out, file.path(project_dir, filename))
}

#' Run Write training set simulation
#' @keywords internal
#' @author Ghislain Durif
diyabc_run_trainset_simu <- function(project_dir, data_file, n_core = 1) {
    diyabc_bin <- find_bin("diyabc")
    # copy data file into project dir
    # FIXME
    # check project dir
    if(!dir.exists(project_dir)) {
        stop("Input directory does not exist")
    }
    if(!str_detect(string = project_dir, pattern = "/$")) {
        project_dir <- str_c(project_dir, "/")
    }
    # init seeds
    cmd <- str_c(diyabc_bin, 
                 "-p", project_dir, "-f", "-n",
                 str_c("'t:", n_core, "'"),
                 sep = " ")
    check <- system(cmd)
    if(check != 0) {
        stop("Issue with seed initialization")
    }
    # # run
    # cmd <- str_c(diyabc_bin, "-p", project_dir, "-k", sep = " ")
    # check <- system(cmd)
    # if(check != 0) {
    #     stop("Issue with simulation run")
    # }
    # output
    return(NULL)
}
