context("training_set_tools")

test_that("write_header", {
    
    test_dir <- mk_proj_dir("test_training_set_tools")
    param_list <- list(
        "N1 N UN[100,10000,0.0,0.0]",
        "N2 N UN[100,10000,0.0,0.0]",
        "N3 N UN[100,10000,0.0,0.0]",
        "N4 N UN[100,10000,0.0,0.0]",
        "ra A UN[0.05,0.95,0.0,0.0]",
        "t32 T UN[10,1000,0.0,0.0]",
        "t21 T UN[10,1000,0.0,0.0]",
        "t431 T UN[10,1000,0.0,0.0]"
    )
    param_count_list <- list(8)
    scenario_list <- as.list(str_c(
        "N1 N2 N3 N4",
        "0 sample 1",
        "0 sample 2",
        "0 sample 3",
        "0 sample 4",
        "t431 split 4 1 3 ra",
        "t32 merge 2 3",
        "t21 merge 1 2",
        sep = "\n"
    ))
    cond_list <- list("t21>t32", "t431<t32")
    data_file <- "indseq_SNP_sim_dataset_4POP_001.snp"
    locus_type <- "snp"
    seq_mode <- "indseq"
    locus <- "5000 <A> G1 from 1"
    
    write_header(test_dir, data_file, 
                 scenario_list, param_count_list, 
                 param_list, cond_list, 
                 locus_type, seq_mode, locus)
    
    file_name <- file.path(test_dir, "header.txt")
    file_type = "text/plain"
    data_type = "snp"
    
    header_check <- read_header(file_name, file_type, data_type)
    expect_true(header_check$valid)
    expect_equal(length(header_check$msg), 0)
    expect_equal(header_check$data_file, "indseq_SNP_sim_dataset_4POP_001.snp")
    expect_equal(header_check$locus_desc, "5000 <A> G1 from 1")
    expect_equal(header_check$n_param, 8)
    expect_equal(header_check$n_prior, 8)
    expect_equal(header_check$n_stat, 1)
    expect_equal(header_check$cond_list, c("t21>t32", "t431<t32"))
    expect_equal(
        header_check$prior_list,
        c("N1 N UN[100,10000,0.0,0.0]", 
          "N2 N UN[100,10000,0.0,0.0]",
          "N3 N UN[100,10000,0.0,0.0]",
          "N4 N UN[100,10000,0.0,0.0]",
          "ra A UN[0.05,0.95,0.0,0.0]",
          "t32 T UN[10,1000,0.0,0.0]",
          "t21 T UN[10,1000,0.0,0.0]",
          "t431 T UN[10,1000,0.0,0.0]")
    )
    expect_null(header_check$n_group)
    expect_null(header_check$group_prior_list)
    expect_equal(
        header_check$scenario_list,
        str_c(
            "N1 N2 N3 N4\n0 sample 1\n0 sample 2\n0 sample 3\n0 sample 4",
            "\nt431 split 4 1 3 ra\nt32 merge 2 3\nt21 merge 1 2"
        )
    )
    expect_equal(header_check$simu_mode, "DRAW UNTIL")
})

test_that("diyabc_run_trainset_simu", {
    
    # proj dir
    proj_dir = mk_proj_dir()
    on.exit(tryCatch(fs::dir_delete(proj_dir)))
    logging("tmp dir:", proj_dir)
    
    # copy header and data file from example
    lapply(
        c("headerRF.txt", "indseq_SNP_sim_dataset_4POP_001.snp"),
        function(filename) {
            fs::file_copy(
                path = file.path(example_dir(), 
                                 "IndSeq_SNP_estim_param", filename),
                new_path = file.path(proj_dir, filename)
            )
        }
    )
    
    # try run
    run_proc <- diyabc_run_trainset_simu(proj_dir, n_run = 100, 
                                         run_prior_check = FALSE)
    
    run_proc$is_alive()
    # run_proc$kill()
    
    ## check project directory
    list.files(proj_dir)
    
    ## clean up
    cleanup_diyabc_run(proj_dir)

})

test_that("check_cond", {
    
    cond_list <- c("t1>t2", "t2<t3")
    param_list <- c("N1", "N2", "t1", "t2", "t3")
    res <- check_cond(cond_list, param_list)
    expect_true(res$valid)
    
    cond_list <- c("t1>t2", "t2<")
    param_list <- c("N1", "N2", "t1", "t2", "t3")
    res <- check_cond(cond_list, param_list)
    expect_false(res$valid)
    expect_equal(length(res$msg), 1)
    
    cond_list <- c("t1>t2", "t2<t4")
    param_list <- c("N1", "N2", "t1", "t2", "t3")
    res <- check_cond(cond_list, param_list)
    expect_false(res$valid)
    expect_equal(length(res$msg), 1)
})
