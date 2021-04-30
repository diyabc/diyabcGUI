context("io_management")

test_that("check_data_file", {
    # snp indseq
    data_file <- "indseq_SNP_sim_dataset_4POP_001.snp"
    data_dir <- file.path(example_dir(), 
                          "IndSeq_SNP_estim_param")
    locus_type <- "snp"
    seq_mode <- "indseq"
    out <- check_data_file(data_file, data_dir, locus_type, seq_mode, 
                           expected_data_file = data_file)
    expect_true(out$valid)
    
    # snp poolseq
    data_file <- "poolseq_SNP_sim_dataset_4POP_cov100_001.snp"
    data_dir <- file.path(example_dir(), 
                          "PoolSeq_SNP_estim_param")
    locus_type <- "snp"
    seq_mode <- "poolseq"
    out <- check_data_file(data_file, data_dir, locus_type, seq_mode, 
                           expected_data_file = data_file)
    expect_true(out$valid)
    
    # mss
    data_file <- "mss_example_001.mss"
    data_dir <- data4test_dir("mss")
    locus_type <- "mss"
    seq_mode <- NULL
    out <- check_data_file(data_file, data_dir, locus_type, seq_mode, 
                           expected_data_file = data_file)
    expect_true(out$valid)
})

test_that("check_indseq_snp_data_file", {
    
    data_file <- "indseq_SNP_sim_dataset_4POP_001.snp"
    data_dir <- file.path(example_dir(), 
                          "IndSeq_SNP_estim_param")
    out <- check_indseq_snp_data_file(
        data_file, data_dir, expected_data_file = NULL
    )
    expect_true(out$valid)
    
    data_file <- "poolseq_SNP_sim_dataset_4POP_cov100_001.snp"
    data_dir <- file.path(example_dir(), 
                          "PoolSeq_SNP_estim_param")
    out <- check_indseq_snp_data_file(
        data_file, data_dir, expected_data_file = NULL
    )
    expect_false(out$valid)
})

test_that("indseq_locus_filter", {
    # generate test data
    n_indiv <- 100
    n_pop <- 5
    indiv_id <- str_c("ind", 1:n_indiv)
    indiv_sex <- sample(c("F", "M"), size = n_indiv, replace = TRUE)
    indiv_pop <- sample(1:n_pop, size = n_indiv, replace = TRUE)
    content <- data.frame(
        IND = str_c("ind", 1:n_indiv),
        SEX = indiv_sex,
        POP = indiv_pop,
        A = sample(0:2, size = n_indiv, replace = TRUE),
        H = sample(0:1, size = n_indiv, replace = TRUE),
        X = ifelse(
            indiv_sex == "F",
            sample(0:2, size = n_indiv, replace = TRUE),
            sample(0:1, size = n_indiv, replace = TRUE)
        ),
        Y = ifelse(
            indiv_sex == "F",
            rep(9, n_indiv),
            sample(0:1, size = n_indiv, replace = TRUE)
        ),
        M = sample(0:1, size = n_indiv, replace = TRUE),
        stringsAsFactors = FALSE
    )
    
    col_type <- colnames(content)
    
    locus_details <- data.frame(
        count = rep(1, 5),
        type = tail(col_type, 5),
        stringsAsFactors = FALSE
    )
    
    maf <- 0.05
    
    # test without missing values at random
    snp_filter <- lapply(
        1:(ncol(content) - 3) + 3,
        function(ind) {
            out <- indseq_locus_filter(
                snp_data = content[,ind], 
                sex_data = content[,2], 
                locus_type = col_type[ind], 
                maf = maf
            )
        }
    )
    snp_filter <- Reduce("rbind", snp_filter)
    
    expect_true(is.data.frame(snp_filter))
    expect_identical(colnames(snp_filter), c("filter", "mono", "issue"))
 })

test_that("filter_snp_indseq", {
    ## generate simulated test data
    n_indiv <- 100
    n_pop <- 5
    indiv_id <- str_c("ind", 1:n_indiv)
    indiv_sex <- sample(c("F", "M"), size = n_indiv, replace = TRUE)
    indiv_pop <- sample(1:n_pop, size = n_indiv, replace = TRUE)
    content <- data.frame(
        IND = str_c("ind", 1:n_indiv),
        SEX = indiv_sex,
        POP = indiv_pop,
        A = sample(0:2, size = n_indiv, replace = TRUE),
        H = sample(0:1, size = n_indiv, replace = TRUE),
        X = ifelse(
            indiv_sex == "F",
            sample(0:2, size = n_indiv, replace = TRUE),
            sample(0:1, size = n_indiv, replace = TRUE)
        ),
        Y = ifelse(
            indiv_sex == "F",
            rep(9, n_indiv),
            sample(0:1, size = n_indiv, replace = TRUE)
        ),
        M = sample(0:1, size = n_indiv, replace = TRUE),
        stringsAsFactors = FALSE
    )
    
    col_type <- colnames(content)
    
    locus_details <- data.frame(
        count = rep(1, 5),
        type = tail(col_type, 5),
        stringsAsFactors = FALSE
    )
    
    maf <- 0.05
    
    # test on simulated data
    out <- filter_snp_indseq(content, col_type, locus_details, maf)
    
    expect_true(is.data.frame(out))
    expect_true(all(colnames(out) %in% c("type", "mono", "count", "filter", "issue")))
    
    ## test on SNP data file
    data_file <- "indseq_SNP_sim_dataset_4POP_001.snp"
    data_dir <- file.path(example_dir(), 
                          "IndSeq_SNP_estim_param")
    data_path <- file.path(data_dir, data_file)
    
    # header
    col_type <- unname(unlist(
        read.table(file = data_path, skip = 1, nrows = 1)
    ))
    
    # locus type
    candidate_locus <- c("A", "H", "X", "Y", "M")
    locus_encoding <- str_c(header[-(1:3)], collapse = " ")
    locus_details <- Reduce("rbind", lapply(
        candidate_locus, 
        function(pttrn) {
            count <- str_count(locus_encoding, pttrn)
            return(data.frame(
                count = count,
                type = pttrn,
                stringsAsFactors = FALSE
            ))
        }
    ))
    
    # data
    content <- read.table(file = data_path, skip = 2)
    
    # test on data
    out <- filter_snp_indseq(content, col_type, locus_details, maf)
    
    expect_true(is.data.frame(out))
    expect_true(all(colnames(out) %in% c("type", "mono", "count", "filter", "issue")))
    
    
})

test_that("check_poolseq_snp_data_file", {
    
    data_file <- "poolseq_SNP_sim_dataset_4POP_cov100_001.snp"
    data_dir <- file.path(example_dir(), 
                          "PoolSeq_SNP_estim_param")
    out <- check_poolseq_snp_data_file(
        data_file, data_dir, expected_data_file = NULL
    )
    expect_true(out$valid)
    
    data_file <- "indseq_SNP_sim_dataset_4POP_001.snp"
    data_dir <- file.path(example_dir(), 
                          "IndSeq_SNP_estim_param")
    out <- check_poolseq_snp_data_file(
        data_file, data_dir, expected_data_file = NULL
    )
    expect_false(out$valid)
})

test_that("check_mss_data_file", {
    
    data_file <- "mss_example_001.mss"
    data_dir <- data4test_dir("mss")
    out <- check_mss_data_file(
        data_file, data_dir, expected_data_file = NULL
    )
    expect_true(out$valid)
    
    data_file <- "indseq_SNP_sim_dataset_4POP_001.snp"
    data_dir <- file.path(example_dir(), 
                          "IndSeq_SNP_estim_param")
    out <- check_mss_data_file(
        data_file, data_dir, expected_data_file = NULL
    )
    expect_false(out$valid)
})

test_that("check_file_name", {
    expect_true(check_file_name(system.file("DESCRIPTION", 
                                            package = "diyabcGUI")))
    expect_false(check_file_name(5))
    expect_false(check_file_name(file.path(fs::path_home(), 
                                           "not_existing_file")))
})
