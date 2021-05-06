context("44_data_check")

test_that("check_data_file", {
    # snp indseq
    data_file <- "indseq_SNP_sim_dataset_4POP_001.snp"
    data_dir <- file.path(example_dir(), 
                          "IndSeq_SNP_estim_param")
    locus_type <- "snp"
    seq_mode <- "indseq"
    out <- check_data_file(data_file, data_dir, locus_type, seq_mode)
    expect_true(out$valid)
    
    # snp poolseq
    data_file <- "poolseq_SNP_sim_dataset_4POP_cov100_001.snp"
    data_dir <- file.path(example_dir(), 
                          "PoolSeq_SNP_estim_param")
    locus_type <- "snp"
    seq_mode <- "poolseq"
    out <- check_data_file(data_file, data_dir, locus_type, seq_mode)
    expect_true(out$valid)
    
    # mss
    data_file <- "mss_example_001.mss"
    data_dir <- data4test_dir("mss")
    locus_type <- "mss"
    seq_mode <- NULL
    out <- check_data_file(data_file, data_dir, locus_type, seq_mode)
    expect_true(out$valid)
})
