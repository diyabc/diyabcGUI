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
    
    # ok
    cond_list <- c("t1>t2", "t3<t4")
    scen_list <- c("N N N\n0 sample 1\n0 sample 2\nt1 merge 2 3\nt2 merge 1 2",
                   "N N N\n0 sample 1\n0 sample 2\nt3 split 3 1 2 ra\nt4 merge 1 2")
    res <- check_cond(cond_list, scen_list)
    expect_true(res$valid)
    
    # ok: shared parameter
    cond_list <- c("t1>t2", "t1<t4")
    scen_list <- c("N N N\n0 sample 1\n0 sample 2\nt1 merge 2 3\nt2 merge 1 2",
                   "N N N\n0 sample 1\n0 sample 2\nt1 split 3 1 2 ra\nt4 merge 1 2")
    res <- check_cond(cond_list, scen_list)
    expect_true(res$valid)
    
    # nok: parameter mixing
    cond_list <- c("t1>t2", "t2<t4")
    scen_list <- c("N N N\n0 sample 1\n0 sample 2\nt1 merge 2 3\nt2 merge 1 2",
                   "N N N\n0 sample 1\n0 sample 2\nt1 split 3 1 2 ra\nt4 merge 1 2")
    res <- check_cond(cond_list, scen_list)
    expect_false(res$valid)
    expect_equal(length(res$msg), 1)
    
    # nok: formating issue
    cond_list <- c("t1>t2", "t3<")
    scen_list <- c("N N N\n0 sample 1\n0 sample 2\nt1 merge 2 3\nt2 merge 1 2",
                   "N N N\n0 sample 1\n0 sample 2\nt3 split 3 1 2 ra\nt4 merge 1 2")
    res <- check_cond(cond_list, scen_list)
    expect_false(res$valid)
    expect_equal(length(res$msg), 1)
    
    # nok: parameters from different models
    cond_list <- c("t1>t2", "t2<t4")
    scen_list <- c("N N N\n0 sample 1\n0 sample 2\nt1 merge 2 3\nt2 merge 1 2",
                   "N N N\n0 sample 1\n0 sample 2\nt3 split 3 1 2 ra\nt4 merge 1 2")
    res <- check_cond(cond_list, scen_list)
    expect_false(res$valid)
    expect_equal(length(res$msg), 1)
    
    # nok: wrong parameter
    cond_list <- c("t1>t2", "t3<t5")
    scen_list <- c("N N N\n0 sample 1\n0 sample 2\nt1 merge 2 3\nt2 merge 1 2",
                   "N N N\n0 sample 1\n0 sample 2\nt3 split 3 1 2 ra\nt4 merge 1 2")
    res <- check_cond(cond_list, scen_list)
    expect_false(res$valid)
    expect_equal(length(res$msg), 1)
    
    # nok: issue with scenario
    cond_list <- c("t1>t2", "t3<t4")
    scen_list <- c("N N N\n0 sample 1\n0 sample 2\nt1 merge 2 3\nt2 merge 1 2",
                   "N N N\n0 sample 1\n0 sample 2\nt3 split 3 1 2 ra\nt4 merge")
    res <- check_cond(cond_list, scen_list)
    expect_false(res$valid)
    expect_equal(length(res$msg), 1)
})

test_that("check_locus_desc", {
    
    ## SNP IndSeq
    locus_type <- "snp"
    seq_mode <- "indseq"
    
    # estim param
    test_proj <- "IndSeq_SNP_estim_param"
    test_dir <- file.path(data4test_dir(), test_proj)
    
    data_file <- "indseq_SNP_sim_dataset_4POP_001.snp"
    data_dir <- test_dir
    data_check <- check_data_file(data_file, data_dir, locus_type, seq_mode)
    expect_true(data_check$valid)
    
    header_file <- file.path(test_dir, "headerRF.txt")
    file_type <- "text/plain"
    header_check <- read_header(header_file, file_type, locus_type)
    expect_true(header_check$valid)
    locus_desc <- header_check$locus_desc
    
    res <- check_locus_desc(locus_desc, data_check, locus_type)
    expect_true(res$valid)
    
    locus_desc <- "70 <A> 10 <X> 10 <M> 10 <Y> G1 from 1"
    res <- check_locus_desc(locus_desc, data_check, locus_type)
    expect_false(res$valid)
    
    # model choice
    test_proj <- "IndSeq_SNP_model_choice"
    test_dir <- file.path(data4test_dir(), test_proj)
    
    data_file <- "indseq_SNP_sim_dataset_4POP_001.snp"
    data_dir <- test_dir
    data_check <- check_data_file(data_file, data_dir, locus_type, seq_mode)
    expect_true(data_check$valid)
    
    header_file <- file.path(test_dir, "headerRF.txt")
    file_type <- "text/plain"
    header_check <- read_header(header_file, file_type, locus_type)
    expect_true(header_check$valid)
    locus_desc <- header_check$locus_desc
    
    res <- check_locus_desc(locus_desc, data_check, locus_type)
    expect_true(res$valid)
    
    ## SNP PoolSeq
    locus_type <- "snp"
    seq_mode <- "poolseq"
    # estim param
    test_proj <- "PoolSeq_SNP_estim_param"
    test_dir <- file.path(data4test_dir(), test_proj)
    
    data_file <- "poolseq_SNP_sim_dataset_4POP_cov100_001.snp"
    data_dir <- test_dir
    data_check <- check_data_file(data_file, data_dir, locus_type, seq_mode)
    expect_true(data_check$valid)
    
    header_file <- file.path(test_dir, "headerRF.txt")
    file_type <- "text/plain"
    header_check <- read_header(header_file, file_type, locus_type)
    expect_true(header_check$valid)
    locus_desc <- header_check$locus_desc
    
    res <- check_locus_desc(locus_desc, data_check, locus_type)
    expect_true(res$valid)
    
    
    # model choice
    test_proj <- "PoolSeq_SNP_model_choice"
    test_dir <- file.path(data4test_dir(), test_proj)
    
    data_file <- "poolseq_SNP_sim_dataset_4POP_cov100_001.snp"
    data_dir <- test_dir
    data_check <- check_data_file(data_file, data_dir, locus_type, seq_mode)
    expect_true(data_check$valid)
    
    header_file <- file.path(test_dir, "headerRF.txt")
    file_type <- "text/plain"
    header_check <- read_header(header_file, file_type, locus_type)
    expect_true(header_check$valid)
    locus_desc <- header_check$locus_desc
    
    res <- check_locus_desc(locus_desc, data_check, locus_type)
    expect_true(res$valid)
    
    ## MSS
    locus_type <- "mss"
    seq_mode <- "indseq"
    
    # microsat
    test_proj <- "Microsat"
    test_dir <- file.path(data4test_dir(), test_proj)
    
    data_file <- "simu_dataset_microsat_one_pop_bottleneck_multisamples_001.mss"
    data_dir <- test_dir
    data_check <- check_data_file(data_file, data_dir, locus_type, seq_mode)
    expect_true(data_check$valid)
    
    header_file <- file.path(test_dir, "header.txt")
    file_type <- "text/plain"
    header_check <- read_header(header_file, file_type, locus_type)
    expect_true(header_check$valid)
    locus_desc <- header_check$locus_desc
    
    res <- check_locus_desc(locus_desc, data_check, locus_type)
    expect_true(res$valid)
    
    # microsat sequence 1
    test_proj <- "Microsat_Sequences"
    test_dir <- file.path(data4test_dir(), test_proj)
    
    data_file <- "mss_example_001.mss"
    data_dir <- test_dir
    data_check <- check_data_file(data_file, data_dir, locus_type, seq_mode)
    expect_true(data_check$valid)
    
    header_file <- file.path(test_dir, "header.txt")
    file_type <- "text/plain"
    header_check <- read_header(header_file, file_type, locus_type)
    expect_true(header_check$valid)
    locus_desc <- header_check$locus_desc
    
    res <- check_locus_desc(locus_desc, data_check, locus_type)
    expect_true(res$valid)
    
    # microsat sequence 2
    test_proj <- "Microsat_Sequences2"
    test_dir <- file.path(data4test_dir(), test_proj)
    
    data_file <- "toytest2_micro_seq_complexe_001.mss"
    data_dir <- test_dir
    data_check <- check_data_file(data_file, data_dir, locus_type, seq_mode)
    expect_true(data_check$valid)
    
    header_file <- file.path(test_dir, "header.txt")
    file_type <- "text/plain"
    header_check <- read_header(header_file, file_type, locus_type)
    expect_true(header_check$valid)
    locus_desc <- header_check$locus_desc
    
    res <- check_locus_desc(locus_desc, data_check, locus_type)
    expect_true(res$valid)
})

test_that("check_snp_locus_desc", {
    
    ## SNP PoolSeq
    locus_type <- "snp"
    seq_mode <- "poolseq"
    # estim param
    test_proj <- "PoolSeq_SNP_estim_param"
    test_dir <- file.path(data4test_dir(), test_proj)
    
    data_file <- "poolseq_SNP_sim_dataset_4POP_cov100_001.snp"
    data_dir <- test_dir
    data_check <- check_data_file(data_file, data_dir, locus_type, seq_mode)
    expect_true(data_check$valid)
    
    header_file <- file.path(test_dir, "headerRF.txt")
    file_type <- "text/plain"
    header_check <- read_header(header_file, file_type, locus_type)
    expect_true(header_check$valid)
    locus_desc <- header_check$locus_desc
    
    res <- check_snp_locus_desc(locus_desc, data_check)
    expect_true(res$valid)
    
    
    # model choice
    test_proj <- "PoolSeq_SNP_model_choice"
    test_dir <- file.path(data4test_dir(), test_proj)
    
    data_file <- "poolseq_SNP_sim_dataset_4POP_cov100_001.snp"
    data_dir <- test_dir
    data_check <- check_data_file(data_file, data_dir, locus_type, seq_mode)
    expect_true(data_check$valid)
    
    header_file <- file.path(test_dir, "headerRF.txt")
    file_type <- "text/plain"
    header_check <- read_header(header_file, file_type, locus_type)
    expect_true(header_check$valid)
    locus_desc <- header_check$locus_desc
    
    res <- check_snp_locus_desc(locus_desc, data_check)
    expect_true(res$valid)
})

test_that("format_snp_locus_desc", {
    
    # ok, multiple loci
    count <- c(10, 10, 10, 10, 10)
    type <- c("A", "H", "X", "Y", "M")
    expect_equal(
        format_snp_locus_desc(count, type, from = 1),
        "10 <A> 10 <H> 10 <X> 10 <Y> 10 <M> from 1"
    )
    
    # ok, single locus
    count <- c(10)
    type <- c("A")
    expect_equal(
        format_snp_locus_desc(count, type, from = 1),
        "10 <A> from 1"
    )
    
    # nok, different length input
    count <- c(10, 10)
    type <- c("A")
    expect_error(format_snp_locus_desc(count, type, from = 1))
    
    # nok, bad locus type
    count <- c(10, 10)
    type <- c("A", "Z")
    expect_error(format_snp_locus_desc(count, type, from = 1))
    
})

test_that("default_snp_locus_desc", {
    
    # ok, multiple loci
    locus_count <- data.frame(
        type = c("A", "H", "X", "Y", "M"), available = c(10, 10, 10, 10, 10),
        stringsAsFactors = FALSE
    )
    expect_equal(
        default_snp_locus_desc(locus_count, from = 1),
        "10 <A> 10 <H> 10 <X> 10 <Y> 10 <M> from 1"
    )
    
    # nok, bad column names
    locus_count <- data.frame(
        type = c("A", "H", "X", "Y", "M"), count = c(10, 10, 10, 10, 10),
        stringsAsFactors = FALSE
    )
    expect_error(default_snp_locus_desc(locus_count, from = 1))
    
    # nok, bad input formating
    locus_count <- list(
        type = c("A", "H", "X", "Y", "M"), count = c(10, 10, 10, 10, 10)
    )
    expect_error(default_snp_locus_desc(locus_count, from = 1))
    
    ## SNP PoolSeq
    locus_type <- "snp"
    seq_mode <- "poolseq"
    # estim param
    test_proj <- "PoolSeq_SNP_estim_param"
    test_dir <- file.path(data4test_dir(), test_proj)
    
    data_file <- "poolseq_SNP_sim_dataset_4POP_cov100_001.snp"
    data_dir <- test_dir
    data_check <- check_data_file(data_file, data_dir, locus_type, seq_mode)
    
    expect_equal(
        default_snp_locus_desc(data_check$locus_count, from = 1),
        "14388 <A> from 1"
    )
})

test_that("clean_snp_locus_desc", {
    
    ## SNP PoolSeq
    locus_type <- "snp"
    seq_mode <- "poolseq"
    # estim param
    test_proj <- "PoolSeq_SNP_estim_param"
    test_dir <- file.path(data4test_dir(), test_proj)
    
    data_file <- "poolseq_SNP_sim_dataset_4POP_cov100_001.snp"
    data_dir <- test_dir
    data_check <- check_data_file(data_file, data_dir, locus_type, seq_mode)
    expect_true(data_check$valid)
    
    header_file <- file.path(test_dir, "headerRF.txt")
    file_type <- "text/plain"
    header_check <- read_header(header_file, file_type, locus_type)
    expect_true(header_check$valid)
    locus_desc <- header_check$locus_desc
    
    expect_equal(
        clean_snp_locus_desc(locus_desc, data_check),
        "5000 <A> G1 from 1"
    )
    
    ## bad locus desc
    expect_equal(
        clean_snp_locus_desc(NULL, data_check),
        "14388 <A> from 1"
    )
    
    expect_equal(
        clean_snp_locus_desc("5000000 <A> G1 from 1", data_check),
        "14388 <A> from 1"
    )
})




