context("41_input_read")

# test_that("header_reader", {
#     
#     # existing file
#     test_proj <- "IndSeq_SNP_model_choice"
#     test_dir <- file.path(data4test_dir(), test_proj)
#     file_name <- file.path(test_dir, "headerRF.txt")
#     file_type <- "text/plain"
#     
#     tmp <- header_reader(file_name, file_type)
#     expect_true(tmp$valid)
# 
# })

test_that("read_header", {
    
    ## SNP IndSeq
    # estim param
    test_proj <- "IndSeq_SNP_estim_param"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "headerRF.txt")
    file_type <- "text/plain"
    locus_type = "snp"
    res <- read_header(file_name, file_type, locus_type)
    expect_true(res$valid)
    expect_equal(length(res$msg), 0)
    expect_equal(res$data_file, "indseq_SNP_sim_dataset_4POP_001.snp")
    expect_equal(res$n_param, 8)
    expect_equal(res$n_sumstat, 130)
    expect_equal(res$n_scenario, 1)
    expect_equal(res$n_scenario, length(res$scenario_list))
    expect_equal(res$n_cond, 2)
    expect_equal(res$n_cond, length(res$cond_list))
    expect_equal(res$simu_mode, "DRAW UNTIL")
    expect_equal(res$n_loci_desc, 1)
    expect_equal(res$n_loci_desc, length(res$loci_desc))
    expect_equal(res$n_prior, 8)
    expect_equal(res$n_prior, length(res$prior_list))
    
    # indeseq -> bad header file
    test_proj <- "bad_files"
    test_dir <- file.path(test_input_dir(), "bad_files")
    file_name <- file.path(test_dir, "headerRF.txt")
    file_type <- "text/plain"
    locus_type = "mss"
    res <- read_header(file_name, file_type, locus_type)
    expect_false(res$valid)
    expect_equal(length(res$msg), 1)
    
    # model choice
    test_proj <- "IndSeq_SNP_model_choice"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "headerRF.txt")
    file_type <- "text/plain"
    locus_type = "snp"
    res <- read_header(file_name, file_type, locus_type)
    expect_true(res$valid)
    expect_equal(length(res$msg), 0)
    expect_equal(res$data_file, "indseq_SNP_sim_dataset_4POP_001.snp")
    expect_equal(res$n_param, 13)
    expect_equal(res$n_sumstat, 130)
    expect_equal(res$n_scenario, 6)
    expect_equal(res$n_scenario, length(res$scenario_list))
    expect_equal(res$n_cond, 6)
    expect_equal(res$n_cond, length(res$cond_list))
    expect_equal(res$simu_mode, "DRAW UNTIL")
    expect_equal(res$n_loci_desc, 1)
    expect_equal(res$n_loci_desc, length(res$loci_desc))
    expect_equal(res$n_prior, 13)
    expect_equal(res$n_prior, length(res$prior_list))
    
    ## SNP PoolSeq
    # estim param
    test_proj <- "PoolSeq_SNP_estim_param"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "headerRF.txt")
    file_type <- "text/plain"
    locus_type = "snp"
    res <- read_header(file_name, file_type, locus_type)
    expect_true(res$valid)
    expect_equal(length(res$msg), 0)
    expect_equal(res$data_file, "poolseq_SNP_sim_dataset_4POP_cov100_001.snp")
    expect_equal(res$n_param, 8)
    expect_equal(res$n_sumstat, 130)
    expect_equal(res$n_scenario, 1)
    expect_equal(res$n_scenario, length(res$scenario_list))
    expect_equal(res$n_cond, 2)
    expect_equal(res$n_cond, length(res$cond_list))
    expect_equal(res$simu_mode, "DRAW UNTIL")
    expect_equal(res$n_loci_desc, 1)
    expect_equal(res$n_loci_desc, length(res$loci_desc))
    expect_equal(res$n_prior, 8)
    expect_equal(res$n_prior, length(res$prior_list))
    
    # model choice
    test_proj <- "PoolSeq_SNP_model_choice"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "headerRF.txt")
    file_type <- "text/plain"
    locus_type = "snp"
    res <- read_header(file_name, file_type, locus_type)
    expect_true(res$valid)
    expect_equal(length(res$msg), 0)
    expect_equal(res$data_file, "poolseq_SNP_sim_dataset_4POP_cov100_001.snp")
    expect_equal(res$n_param, 13)
    expect_equal(res$n_sumstat, 130)
    expect_equal(res$n_scenario, 6)
    expect_equal(res$n_scenario, length(res$scenario_list))
    expect_equal(res$n_cond, 6)
    expect_equal(res$n_cond, length(res$cond_list))
    expect_equal(res$simu_mode, "DRAW UNTIL")
    expect_equal(res$n_loci_desc, 1)
    expect_equal(res$n_loci_desc, length(res$loci_desc))
    expect_equal(res$n_prior, 13)
    expect_equal(res$n_prior, length(res$prior_list))
    
    ## MSS
    # microsat
    test_proj <- "Microsat"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "header.txt")
    file_type <- "text/plain"
    locus_type = "mss"
    res <- read_header(file_name, file_type, locus_type)
    expect_true(res$valid)
    expect_equal(length(res$msg), 0)
    expect_equal(
        res$data_file, 
        "simu_dataset_microsat_one_pop_bottleneck_multisamples_001.mss"
    )
    expect_equal(res$n_param, 6)
    expect_equal(res$n_sumstat, 40)
    expect_equal(res$n_scenario, 2)
    expect_equal(res$n_scenario, length(res$scenario_list))
    expect_equal(res$n_cond, 0)
    expect_equal(res$n_cond, length(res$cond_list))
    expect_null(res$simu_mode)
    expect_equal(res$n_loci_desc, 50)
    expect_equal(res$n_loci_desc, length(res$loci_desc))
    expect_equal(res$n_prior, 3)
    expect_equal(res$n_prior, length(res$prior_list))
    expect_equal(res$n_group, 1)
    expect_equal(res$n_group, length(res$group_prior_list))
    
    # microsat sequence 1
    test_proj <- "Microsat_Sequences"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "header.txt")
    file_type <- "text/plain"
    locus_type = "mss"
    res <- read_header(file_name, file_type, locus_type)
    expect_true(res$valid)
    expect_equal(length(res$msg), 0)
    expect_equal(res$data_file, "mss_example_001.mss")
    expect_equal(res$n_param, 9)
    expect_equal(res$n_sumstat, 2)
    expect_equal(res$n_scenario, 1)
    expect_equal(res$n_scenario, length(res$scenario_list))
    expect_equal(res$n_cond, 2)
    expect_equal(res$n_cond, length(res$cond_list))
    expect_equal(res$simu_mode, "DRAW UNTIL")
    expect_equal(res$n_loci_desc, 28)
    expect_equal(res$n_loci_desc, length(res$loci_desc))
    expect_equal(res$n_prior, 9)
    expect_equal(res$n_prior, length(res$prior_list))
    expect_equal(res$n_group, 2)
    expect_equal(res$n_group, length(res$group_prior_list))
    
    # microsat sequence 1 -> bad header file
    test_proj <- "bad_files"
    test_dir <- file.path(test_input_dir(), "bad_files")
    file_name <- file.path(test_dir, "header.txt")
    file_type <- "text/plain"
    locus_type = "mss"
    res <- read_header(file_name, file_type, locus_type)
    expect_false(res$valid)
    expect_equal(length(res$msg), 1)
    
    # microsat sequence 2
    test_proj <- "Microsat_Sequences2"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "header.txt")
    file_type <- "text/plain"
    locus_type = "mss"
    res <- read_header(file_name, file_type, locus_type)
    expect_true(res$valid)
    expect_equal(length(res$msg), 0)
    expect_equal(res$data_file, "toytest2_micro_seq_complexe_001.mss")
    expect_equal(res$n_param, 22)
    expect_equal(res$n_sumstat, 85)
    expect_equal(res$n_scenario, 2)
    expect_equal(res$n_scenario, length(res$scenario_list))
    expect_equal(res$n_cond, 2)
    expect_equal(res$n_cond, length(res$cond_list))
    expect_equal(res$simu_mode, "DRAW UNTIL")
    expect_equal(res$n_loci_desc, 28)
    expect_equal(res$n_loci_desc, length(res$loci_desc))
    expect_equal(res$n_prior, 9)
    expect_equal(res$n_prior, length(res$prior_list))
    expect_equal(res$n_group, 5)
    expect_equal(res$n_group, length(res$group_prior_list))
})


test_that("parse_header_scenario", {
    # model choice
    test_proj <- "IndSeq_SNP_model_choice"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "headerRF.txt")
    header <- str_split(read_file(file_name), "\n", simplify = TRUE)
    
    content <- header[5:13]
    res <- parse_header_scenario(content)
    
    expect_equal(
        res,
        list(
            valid=TRUE, id=1, n_param=8, prior=0.16667,
            scenario=str_c(content[6:13], collapse = "\n")
        )
    )
    
    content <- header[6:13]
    res <- parse_header_scenario(content)
    
    expect_false(res$valid)
})


test_that("parse_header_group_prior", {
    # microsat
    test_proj <- "Microsat_Sequences"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "header.txt")
    header <- str_split(read_file(file_name), "\n", simplify = TRUE)
    
    content <- header[60:74]
    n_group <- 2
    current_line <- 59
    res <- parse_header_group_prior(content, n_group, current_line)
    
    expect_true(res$valid)
    expect_equal(length(res$group_prior), 2)
    expect_equal(length(res$mss_type), 2)
    expect_equal(res$current_line, 74)
})


test_that("read_statobs", {
    
    # existing file
    test_proj <- "IndSeq_SNP_estim_param"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "statobsRF.txt")
    file_type <- "text/plain"
    tmp <- read_statobs(file_name, file_type, n_stat = 130)
    expect_true(tmp$valid)
    
    # bad number of summary stats
    test_proj <- "IndSeq_SNP_estim_param"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "statobsRF.txt")
    file_type <- "text/plain"
    res <- read_statobs(file_name, file_type, n_stat = 120)
    expect_false(res$valid)
    expect_true(length(res$msg) == 1)
    
    # unexisting file
    file_name <- file.path(test_dir, "toto.txt")
    tmp <- read_statobs(file_name, file_type, n_stat = 130)
    expect_false(tmp$valid)
    expect_true(length(tmp$msg) == 1)
    
    # unexisting file and wrong file type
    file_name <- file.path(test_dir, "toto.txt")
    file_type <- "toto"
    tmp <- read_statobs(file_name, file_type, n_stat = 130)
    expect_false(tmp$valid)
    expect_true(length(tmp$msg) == 2)
})
