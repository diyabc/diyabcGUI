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
    
    # test
    locus_desc <- "1000 <A> 100 <M> G1 from 1"
    locus_count <- data.frame(
        type = c("A", "M"),
        count = c(1000, 100),
        available = c(1000, 100),
        stringsAsFactors = FALSE
    )
    res <- check_snp_locus_desc(locus_desc, locus_count)
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
    locus_count <- data_check$locus_count
    
    res <- check_snp_locus_desc(locus_desc, locus_count)
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
    locus_count <- data_check$locus_count
    
    res <- check_snp_locus_desc(locus_desc, locus_count)
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
        clean_snp_locus_desc(locus_desc, data_check$locus_count),
        "5000 <A> G1 from 1"
    )
    
    ## bad locus desc
    expect_equal(
        clean_snp_locus_desc(NULL, data_check$locus_count),
        "14388 <A> from 1"
    )
    
    expect_equal(
        clean_snp_locus_desc("5000000 <A> G1 from 1", data_check$locus_count),
        "14388 <A> from 1"
    )
})

test_that("default_microsat_locus_desc", {
    # MWE single locus
    locus_name <- str_c("locus", 1)
    locus_type <- "A"
    res <- default_microsat_locus_desc(locus_name, locus_type, group_id = 1)
    expect_equal(res, "locus1 <A> [M] G1 2 40")
    
    # MWE multiple locus
    locus_name <- str_c("locus", 1:20)
    locus_type <- rep(c("A", "M"), each = 10)
    res <- default_microsat_locus_desc(locus_name, locus_type, group_id = 1)
    expect_equal(length(res), length(locus_name))
    expect_equal(res[1], "locus1 <A> [M] G1 2 40")
})

test_that("format_microsat_locus_desc", {
    # MWE single locus/single param
    locus_name <- str_c("locus", 1)
    locus_type <- "A"
    res <- format_microsat_locus_desc(
        locus_name, locus_type, group_id = 1, motif = 2, range = 40
    )
    expect_equal(res, "locus1 <A> [M] G1 2 40")
    
    # MWE multiple locus/multiple param (case 1)
    locus_name <- str_c("locus", 1:20)
    locus_type <- rep(c("A", "M"), each = 10)
    res <- format_microsat_locus_desc(
        locus_name, locus_type, group_id = 1, motif = 2, range = 40
    )
    expect_equal(length(res), length(locus_name))
    expect_equal(res[1], "locus1 <A> [M] G1 2 40")
    
    # MWE multiple locus/multiple param (case 2)
    locus_name <- str_c("locus", 1:20)
    locus_type <- rep(c("A", "M"), times = 10)
    group_id <- sort(rep(1:2, each = 10))
    motif <- 2
    range <- c(rep(40, 15), rep(50, 5))
    res <- format_microsat_locus_desc(
        locus_name, locus_type, group_id, motif, range
    )
    expect_equal(length(res), length(locus_name))
    expect_equal(res[1], "locus1 <A> [M] G1 2 40")
})

test_that("format_sequence_locus_desc", {
    # MWE single locus/single param
    locus_name <- str_c("locus", 1)
    locus_type <- "A"
    res <- format_sequence_locus_desc(
        locus_name, locus_type, group_id = 1, seq_length = 1000
    )
    expect_equal(res, "locus1 <A> [S] G1 1000")
    
    # MWE multiple locus/multiple param (case 1)
    locus_name <- str_c("locus", 1:20)
    locus_type <- rep(c("A", "M"), each = 10)
    res <- format_sequence_locus_desc(
        locus_name, locus_type, group_id = 1, seq_length = 1000
    )
    expect_equal(length(res), length(locus_name))
    expect_equal(res[1], "locus1 <A> [S] G1 1000")
    
    # MWE multiple locus/multiple param (case 1)
    locus_name <- str_c("locus", 1:20)
    locus_type <- rep(c("A", "M"), times = 10)
    group_id <- sort(rep(1:2, each = 10))
    seq_length <- c(rep(1000, 15), rep(1200, 5))
    res <- format_sequence_locus_desc(
        locus_name, locus_type, group_id, seq_length
    )
    expect_equal(length(res), length(locus_name))
    expect_equal(res[1], "locus1 <A> [S] G1 1000")
})

test_that("default_mss_locus_desc", {
    
    # MWE 1
    locus_name <- str_c("locus", 1:20)
    locus_type <- rep(c("A", "M"), each = 10)
    locus_mode <- rep(c("M", "S"), times = 10)
    seq_length <- numeric(length(locus_type))
    seq_length[locus_mode == "S"] <- 1000
    seq_length[locus_mode != "S"] <- NA
    res <- default_mss_locus_desc(
        locus_name, locus_type, locus_mode, seq_length
    )
    expect_equal(length(res), length(locus_name))
    expect_equal(res[1], "locus1 <A> [M] G1 2 40")
    expect_equal(res[2], "locus2 <A> [S] G2 1000")
    
    # MWE 2
    locus_name <- str_c("locus", 1:20)
    locus_type <- rep(c("A", "M"), times = 10)
    locus_mode <- rep(c("M", "S"), each = 10)
    seq_length <- numeric(length(locus_type))
    seq_length[locus_mode == "S"] <- 1000
    seq_length[locus_mode != "S"] <- NA
    res <- default_mss_locus_desc(
        locus_name, locus_type, locus_mode, seq_length
    )
    expect_equal(length(res), length(locus_name))
    expect_equal(res[1], "locus1 <A> [M] G1 2 40")
    expect_equal(res[11], "locus11 <A> [S] G2 1000")
})

test_that("correct_mss_locus_desc_group_id", {
    
    # MWE
    locus_name <- str_c("locus", 1:10)
    locus_type <- rep(c("A", "M"), times = 5)
    locus_mode <- rep("M", each = 10)
    seq_length <- numeric(length(locus_type))
    seq_length[locus_mode == "S"] <- 1000
    seq_length[locus_mode != "S"] <- NA
    locus_desc <- default_mss_locus_desc(
        locus_name, locus_type, locus_mode, seq_length
    )
    locus_desc[6:10] <- str_replace(locus_desc[6:10], "G1", "G3")
    
    start_id <- 5
    
    res <- correct_mss_locus_desc_group_id(locus_desc, start_id)
    expect_true(is.character(res))
    expect_equal(length(res), length(locus_desc))
    expect_identical(
        str_extract(res, "G[0-9]+"), rep(c("G5", "G6"), each = 5)
    )
    
})

test_that("check_mean_group_prior", {
    
    strng <- "MEANMU UN[1.00E-004,1.00E-3,0.0005,2]"
    locus_mode <- "M"
    expect_true(check_mean_group_prior(strng, locus_mode))
    
    strng <- "MEANMU UN[1.00E-9,1.00E-7,5E-9,2]"
    locus_mode <- "S"
    expect_true(check_mean_group_prior(strng, locus_mode))
    
    # Microsat
    mean_prior_list = c(
        "MEANMU UN[1.00E-004,1.00E-3,0.0005,2]",
        "MEANP UN[1.00E-001,3.00E-001,0.22,2]",
        "MEANSNI LU[1.00E-008,1.00E-005,1.00E-007,2]"
    )
    locus_mode <- "M"
    
    expect_true(all(
        unlist(lapply(
            mean_prior_list, 
            function(strng) {
                check_mean_group_prior(strng, locus_mode)
            }
        ))
    ))
    
    # Sequence
    mean_prior_list = c(
        "MEANMU UN[1.00E-9,1.00E-7,5E-9,2]",
        "MEANK1 UN[0.050,20,10,2]",
        "MEANK2 UN[0.050,20,10,2]"
    )
    locus_mode <- "S"
    
    expect_true(all(
        unlist(lapply(
            mean_prior_list, 
            function(row) {
                check_mean_group_prior(strng, locus_mode)
            }
        ))
    ))
})

test_that("check_indiv_group_prior", {
    
    strng <- "GAMMU GA[1.00E-005,1.00E-002,Mean_u,2]"
    locus_mode <- "M"
    expect_true(check_indiv_group_prior(strng, locus_mode))
    
    strng <- "GAMK1 GA[0.050,20,Mean_k1,2]"
    locus_mode <- "S"
    expect_true(check_indiv_group_prior(strng, locus_mode))
    
    # Microsat
    indiv_prior_list <- c(
        "GAMMU GA[1.00E-005,1.00E-002,Mean_u,2]",
        "GAMP GA[1.00e-002,9.00E-001,Mean_P,2]",
        "GAMSNI GA[1.00E-009,1.00E-004,Mean_u_SNI,2]"
    )
    locus_mode <- "M"
    
    expect_true(all(
        unlist(lapply(
            indiv_prior_list, 
            function(strng) {
                check_indiv_group_prior(strng, locus_mode)
            }
        ))
    ))
    
    # Sequence
    indiv_prior_list <- c(
        "GAMMU GA[1.00E-9,1.00E-6,Mean_u,2]",
        "GAMK1 GA[0.050,20,Mean_k1,2]",
        "GAMK2 GA[0.050,20,Mean_k2,2]"
    )
    locus_mode <- "S"
    
    expect_true(all(
        unlist(lapply(
            indiv_prior_list, 
            function(strng) {
                check_indiv_group_prior(strng, locus_mode)
            }
        ))
    ))
})

test_that("get_group_prior_param", {
    
    prior <- "MEANMU UN[1e-4,1e-3,5e-4,2]"
    expect_equal(get_group_prior_param(prior, locus_mode = "M"), "MEANMU")
    prior <- "GAMMU GA[1e-5,1e-2,Mean_u,2]"
    expect_equal(get_group_prior_param(prior, locus_mode = "M"), "GAMMU")
    prior <- "MEANP UN[1e-1,3e-1,2.2e-1,2]"
    expect_equal(get_group_prior_param(prior, locus_mode = "M"), "MEANP")
    prior <- "GAMP GA[1e-2,9e-1,Mean_P,2]"
    expect_equal(get_group_prior_param(prior, locus_mode = "M"), "GAMP")
    prior <- "MEANSNI UN[1e-8,1e-5,1e-7,2]"
    expect_equal(get_group_prior_param(prior, locus_mode = "M"), "MEANSNI")
    prior <- "GAMSNI GA[1e-9,1e-4,Mean_u_SNI,2]"
    expect_equal(get_group_prior_param(prior, locus_mode = "M"), "GAMSNI")
    
    prior <- "MEANMU UN[1e-9,1e-7,5e-9,2]"
    expect_equal(get_group_prior_param(prior, locus_mode = "S"), "MEANMU")
    prior <- "GAMMU GA[1e-9,1e-6,Mean_u,2]"
    expect_equal(get_group_prior_param(prior, locus_mode = "S"), "GAMMU")
    prior <- "MEANK1 UN[0.05,20,10,2]"
    expect_equal(get_group_prior_param(prior, locus_mode = "S"), "MEANK1")
    prior <- "GAMK1 GA[0.05,20,Mean_k1,2]"
    expect_equal(get_group_prior_param(prior, locus_mode = "S"), "GAMK1")
    prior <- "MEANK2 UN[0.05,20,10,2]"
    expect_equal(get_group_prior_param(prior, locus_mode = "S"), "MEANK2")
    prior <- "GAMK2 GA[0.05,20,Mean_k2,2]"
    expect_equal(get_group_prior_param(prior, locus_mode = "S"), "GAMK2")
})

test_that("get_group_prior_distrib", {
    prior <- "MEANMU UN[1e-4,1e-3,5e-4,2]"
    expect_equal(get_group_prior_distrib(prior), "UN")
    prior <- "GAMMU GA[1e-5,1e-2,Mean_u,2]"
    expect_equal(get_group_prior_distrib(prior), "GA")
})

test_that("get_group_prior_val", {
    prior <- "MEANMU UN[1e-4,1e-3,5e-4,2]"
    expect_identical(get_group_prior_val(prior), c("1e-4", "1e-3", "5e-4", "2"))
    prior <- "GAMMU GA[1e-5,1e-2,Mean_u,2]"
    expect_identical(get_group_prior_val(prior), c("1e-5", "1e-2", "Mean_u", "2"))
    prior <- "GAMP GA[1e-2,9e-1,Mean_P,2]"
    expect_identical(get_group_prior_val(prior), c("1e-2", "9e-1", "Mean_P", "2"))
    prior <- "GAMSNI GA[1e-9,1e-4,Mean_u_SNI,2]"
    expect_identical(get_group_prior_val(prior), c("1e-9", "1e-4", "Mean_u_SNI", "2"))
    
    prior <- "MEANK1 UN[0.05,20,10,2]"
    expect_identical(get_group_prior_val(prior), c("0.05", "20", "10", "2"))
    prior <- "GAMK1 GA[0.05,20,Mean_k1,2]"
    expect_identical(get_group_prior_val(prior), c("0.05", "20", "Mean_k1", "2"))
    prior <- "GAMK2 GA[0.05,20,Mean_k2,2]"
    expect_identical(get_group_prior_val(prior), c("0.05", "20", "Mean_k2", "2"))
})

test_that("group_prior_param_desc", {
    res <- group_prior_param_desc(locus_mode = "M")
    expect_true(is.data.frame(res))
    res <- group_prior_param_desc(locus_mode = "S")
    expect_true(is.data.frame(res))
})

test_that("mutation_model_desc", {
    res <- mutation_model_desc()
    expect_true(is.data.frame(res))
})

test_that("check_group_prior", {
    
    # MWE with single group
    prior_desc <- "group G1 [M]\nMEANMU UN[1e-04,0.01,0.001,2]\nGAMMU GA[1e-04,0.01,Mean_u,2]\nMEANP UN[1e-04,0.01,0.001,2]\nGAMP GA[1e-04,0.01,Mean_P,2]\nMEANSNI UN[1e-04,0.01,0.001,2]\nGAMSNI GA[1e-04,0.01,Mean_u_SNI,2]"
    
    locus_mode <- c("M")
    group_id <- c("G1")
    
    expect_true(check_group_prior(
        prior_desc, locus_mode = locus_mode, group_id = group_id
    ))
    
    # MWE with multiple groups
    prior_desc <- list(
        "group G1 [M]\nMEANMU UN[1e-04,0.01,0.001,2]\nGAMMU GA[1e-04,0.01,Mean_u,2]\nMEANP UN[1e-04,0.01,0.001,2]\nGAMP GA[1e-04,0.01,Mean_P,2]\nMEANSNI UN[1e-04,0.01,0.001,2]\nGAMSNI GA[1e-04,0.01,Mean_u_SNI,2]",
        "group G2 [S]\nMEANMU UN[1e-04,0.01,0.001,2]\nGAMMU GA[1e-04,0.01,Mean_u,2]\nMEANK1 UN[1e-04,0.01,0.001,2]\nGAMK1 GA[1e-04,0.01,Mean_k1,2]\nMEANK2 UN[1e-04,0.01,0.001,2]\nGAMK2 GA[1e-04,0.01,Mean_k2,2]\nMODEL K2P 10 2"
    )
    
    locus_mode <- c("M", "S")
    group_id <- c("G1", "G2")
    
    expect_true(check_group_prior(
        prior_desc, locus_mode = locus_mode, group_id = group_id
    ))
    
    # microsat
    test_proj <- "Microsat"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "header.txt")
    file_type <- "text/plain"
    locus_type = "mss"
    header_check <- read_header(file_name, file_type, locus_type)
    
    prior_desc <- header_check$group_prior_list
    locus_desc <- header_check$locus_desc
    
    expect_true(check_group_prior(prior_desc, locus_desc))
    
    # microsat sequence 1
    test_proj <- "Microsat_Sequences"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "header.txt")
    file_type <- "text/plain"
    locus_type = "mss"
    header_check <- read_header(file_name, file_type, locus_type)
    
    prior_desc <- header_check$group_prior_list
    locus_desc <- header_check$locus_desc
    
    expect_true(check_group_prior(prior_desc, locus_desc))
})

test_that("default_group_prior",{
    
    group_id <- "G1"
    locus_mode <- "M"
    res <- default_mss_group_prior(group_id, locus_mode)
    expected_res <- c(
        "group G1 [M]", 
        "MEANMU UN[1e-4,1e-3,5e-4,2]", "GAMMU GA[1e-5,1e-2,Mean_u,2]", 
        "MEANP UN[1e-1,3e-1,2.2e-1,2]", "GAMP GA[1e-2,9e-1,Mean_P,2]", 
        "MEANSNI UN[1e-8,1e-5,1e-7,2]", "GAMSNI GA[1e-9,1e-4,Mean_u_SNI,2]"
    )
    expect_identical(res, expected_res)
    
    group_id <- "G1"
    locus_mode <- "S"
    res <- default_mss_group_prior(group_id, locus_mode)
    expected_res <- c(
        "group G1 [S]", 
        "MEANMU UN[1e-9,1e-7,5e-9,2]", "GAMMU GA[1e-9,1e-6,Mean_u,2]", 
        "MEANK1 UN[0.05,20,10,2]", "GAMK1 GA[0.05,20,Mean_k1,2]", 
        "MEANK2 UN[0.05,20,10,2]", "GAMK2 GA[0.05,20,Mean_k2,2]", 
        "MODEL K2P 10 2"
    )
    expect_identical(res, expected_res)
})

test_that("get_group_desc", {
    
    # microsat
    test_proj <- "Microsat"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "header.txt")
    file_type <- "text/plain"
    locus_type = "mss"
    header_check <- read_header(file_name, file_type, locus_type)
    locus_desc <- header_check$locus_desc
    
    res <- get_group_desc(locus_desc)
    expected_res <- data.frame(
        locus_mode = "M", group_id = "G1", stringsAsFactors = FALSE
    )
    expect_identical(res, expected_res)
    
    # microsat sequence 1
    test_proj <- "Microsat_Sequences"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "header.txt")
    file_type <- "text/plain"
    locus_type = "mss"
    header_check <- read_header(file_name, file_type, locus_type)
    locus_desc <- header_check$locus_desc
    
    res <- get_group_desc(locus_desc)
    expected_res <- data.frame(
        locus_mode = c("M", "S"), group_id = c("G1", "G2"), 
        stringsAsFactors = FALSE
    )
    expect_identical(res, expected_res)
})

test_that("default_mss_group_prior", {
    
    # MWE with single group
    locus_mode <- c("M")
    group_id <- c("G1")
    
    res <- default_mss_group_prior(locus_mode = locus_mode, group_id = group_id)
    expected_res <- list(
        "group G1 [M]\nMEANMU UN[1e-4,1e-3,5e-4,2]\nGAMMU GA[1e-5,1e-2,Mean_u,2]\nMEANP UN[1e-1,3e-1,2.2e-1,2]\nGAMP GA[1e-2,9e-1,Mean_P,2]\nMEANSNI UN[1e-8,1e-5,1e-7,2]\nGAMSNI GA[1e-9,1e-4,Mean_u_SNI,2]"
    )
    expect_identical(res, expected_res)
    
    # MWE with multiple groups
    locus_mode <- c("M", "S")
    group_id <- c("G1", "G2")
    
    res <- default_mss_group_prior(locus_mode = locus_mode, group_id = group_id)
    expected_res <- list(
        "group G1 [M]\nMEANMU UN[1e-4,1e-3,5e-4,2]\nGAMMU GA[1e-5,1e-2,Mean_u,2]\nMEANP UN[1e-1,3e-1,2.2e-1,2]\nGAMP GA[1e-2,9e-1,Mean_P,2]\nMEANSNI UN[1e-8,1e-5,1e-7,2]\nGAMSNI GA[1e-9,1e-4,Mean_u_SNI,2]",
        "group G2 [S]\nMEANMU UN[1e-9,1e-7,5e-9,2]\nGAMMU GA[1e-9,1e-6,Mean_u,2]\nMEANK1 UN[0.05,20,10,2]\nGAMK1 GA[0.05,20,Mean_k1,2]\nMEANK2 UN[0.05,20,10,2]\nGAMK2 GA[0.05,20,Mean_k2,2]\nMODEL K2P 10 2"
    )
    expect_identical(res, expected_res)
    
    # microsat
    test_proj <- "Microsat"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "header.txt")
    file_type <- "text/plain"
    locus_type = "mss"
    header_check <- read_header(file_name, file_type, locus_type)
    locus_desc <- header_check$locus_desc
    
    res <- default_mss_group_prior(locus_desc)
    expected_res <- list(
        "group G1 [M]\nMEANMU UN[1e-4,1e-3,5e-4,2]\nGAMMU GA[1e-5,1e-2,Mean_u,2]\nMEANP UN[1e-1,3e-1,2.2e-1,2]\nGAMP GA[1e-2,9e-1,Mean_P,2]\nMEANSNI UN[1e-8,1e-5,1e-7,2]\nGAMSNI GA[1e-9,1e-4,Mean_u_SNI,2]"
    )
    expect_identical(res, expected_res)
    
    # microsat sequence 1
    test_proj <- "Microsat_Sequences"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "header.txt")
    file_type <- "text/plain"
    locus_type = "mss"
    header_check <- read_header(file_name, file_type, locus_type)
    locus_desc <- header_check$locus_desc
    
    res <- default_mss_group_prior(locus_desc)
    expected_res <- list(
        "group G1 [M]\nMEANMU UN[1e-4,1e-3,5e-4,2]\nGAMMU GA[1e-5,1e-2,Mean_u,2]\nMEANP UN[1e-1,3e-1,2.2e-1,2]\nGAMP GA[1e-2,9e-1,Mean_P,2]\nMEANSNI UN[1e-8,1e-5,1e-7,2]\nGAMSNI GA[1e-9,1e-4,Mean_u_SNI,2]",
        "group G2 [S]\nMEANMU UN[1e-9,1e-7,5e-9,2]\nGAMMU GA[1e-9,1e-6,Mean_u,2]\nMEANK1 UN[0.05,20,10,2]\nGAMK1 GA[0.05,20,Mean_k1,2]\nMEANK2 UN[0.05,20,10,2]\nGAMK2 GA[0.05,20,Mean_k2,2]\nMODEL K2P 10 2"
    )
    expect_identical(res, expected_res)
})

test_that("parse_seq_mut_model", {
    # ok
    mut_model <- "MODEL K2P 10 2"
    res <- parse_seq_mut_model(mut_model)
    expect_true(res$valid)
    expect_equal(res$mut_model, "K2P")
    expect_equal(res$invariant_perc, 10)
    expect_equal(res$gamma_shape, 2)
    
    # ok
    mut_model <- "MODEL K2P 10 1e-1"
    res <- parse_seq_mut_model(mut_model)
    expect_true(res$valid)
    expect_equal(res$mut_model, "K2P")
    expect_equal(res$invariant_perc, 10)
    expect_equal(res$gamma_shape, 1E-1)
    
    # nok
    mut_model <- c("MODEL K2P 10 2", "MODEL K2P 10 2")
    res <- parse_seq_mut_model(mut_model)
    expect_false(res$valid)
    
    # nok
    mut_model <- "MODEL 2"
    res <- parse_seq_mut_model(mut_model)
    expect_false(res$valid)
    
    # nok
    mut_model <- "MODEL AA 10 2"
    res <- parse_seq_mut_model(mut_model)
    expect_false(res$valid)
})

