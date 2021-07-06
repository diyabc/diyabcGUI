context("43_data_read")

test_that("read_indseq_snp_data", {
    
    # good file
    data_file <- "indseq_SNP_sim_dataset_4POP_001.snp"
    data_dir <- file.path(data4test_dir(), "IndSeq_SNP_estim_param")
    res <- read_indseq_snp_data(data_file, data_dir)
    expect_true(res$valid)
    expect_equal(length(res$msg), 0)
    expect_equal(res$data_file, data_file)
    expect_equal(res$n_loci, 30000)
    expect_true(is.data.frame(res$locus_count))
    expect_equal(res$locus_count$count, 30000)
    expect_equal(res$locus_count$filter, 13046)
    expect_equal(res$locus_count$mono, 0)
    expect_equal(res$locus_count$available, 16954)
    expect_equal(res$n_pop, 4)
    expect_equal(res$n_indiv, 40)
    expect_equal(res$sex_ratio, "NM=1NF")
    expect_equal(res$maf, 0.05)
    
    # bad file
    data_file <- "poolseq_SNP_sim_dataset_4POP_cov100_001.snp"
    data_dir <- file.path(data4test_dir(), "PoolSeq_SNP_estim_param")
    res <- read_indseq_snp_data(data_file, data_dir)
    expect_false(res$valid)
    expect_true(length(res$msg) > 0)
})

test_that("process_indseq_locus", {
    # generate test data
    n_indiv <- 100
    n_pop <- 5
    indiv_id <- str_c("ind", 1:n_indiv)
    indiv_sex <- sample(c("F", "M"), size = n_indiv, replace = TRUE)
    indiv_pop <- sample(1:n_pop, size = n_indiv, replace = TRUE)
    content <- data.frame(
        IND = str_c("ind", 1:n_indiv),
        SEX = indiv_sex,
        POP = as.character(indiv_pop),
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
    
    locus_count <- data.frame(
        count = rep(1, 5),
        type = tail(col_type, 5),
        stringsAsFactors = FALSE
    )
    
    maf <- 0.05
    
    # test on a single SNP
    ind <- 4
    snp_data <- content[,ind]
    sex_data <- content[,2]
    pop_data <- content[,3]
    snp_type <- col_type[ind]
    
    res <- process_indseq_locus(snp_data, sex_data, pop_data, snp_type, maf)
    expect_true(res$valid)
    expect_false(res$filter)
    expect_false(res$mono)
    expect_true(is.na(res$missing_pop))
    expect_true(is.na(res$issue_X))
    expect_true(is.na(res$issue_Y))
    expect_true(is.numeric(res$af))
    expect_true(is.numeric(res$maf))
    expect_false(res$hudson)
    
    # test on multiple SNP without missing values at random
    res <- Reduce("rbind", lapply(
        1:(ncol(content) - 3) + 3,
        function(ind) {
            out <- process_indseq_locus(
                snp_data = content[,ind],
                sex_data = content[,2],
                pop_data = content[,3],
                snp_type = col_type[ind],
                maf = maf
            )
        }
    ))
    
    expect_true(is.data.frame(res))
    
    ## test missing pop and missing values for X and Y chromosome
    # generate test data
    n_indiv <- 100
    n_pop <- 5
    indiv_id <- str_c("ind", 1:n_indiv)
    indiv_sex <- sort(rep(c("F", "M"), length.out = n_indiv))
    indiv_pop <- sort(rep(str_c("pop", 1:n_pop), length.out = n_indiv))
    content <- data.frame(
        IND = str_c("ind", 1:n_indiv),
        SEX = indiv_sex,
        POP = as.character(indiv_pop),
        A = c(rep(9, sum(indiv_pop == "pop1")), 
              sample(0:2, size = n_indiv - sum(indiv_pop == "pop1"), 
                     replace = TRUE)),
        H = sample(0:1, size = n_indiv, replace = TRUE),
        X = sample(0:2, size = n_indiv, replace = TRUE),
        Y = sample(0:1, size = n_indiv, replace = TRUE),
        M = sample(0:1, size = n_indiv, replace = TRUE),
        stringsAsFactors = FALSE
    )
    col_type <- colnames(content)
    # run
    res <- Reduce("rbind", lapply(
        1:(ncol(content) - 3) + 3,
        function(ind) {
            out <- process_indseq_locus(
                snp_data = content[,ind],
                sex_data = content[,2],
                pop_data = content[,3],
                snp_type = col_type[ind],
                maf = maf
            )
        }
    ))
    
    expect_equal(res$valid, c(FALSE, TRUE, FALSE, FALSE, TRUE))
})

test_that("check_snp_indseq", {
    ## generate simulated test data
    n_indiv <- 100
    n_pop <- 5
    indiv_id <- str_c("ind", 1:n_indiv)
    indiv_sex <- sample(c("F", "M"), size = n_indiv, replace = TRUE)
    indiv_pop <- sample(1:n_pop, size = n_indiv, replace = TRUE)
    data_tab <- data.frame(
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
    
    indiv_info <- data_tab[,1:3]
    content <- t(data_tab[,-(1:3)])
    snp_type <- rownames(content)
    
    locus_count <- data.frame(
        count = rep(1, 5), type = snp_type,
        stringsAsFactors = FALSE
    )
    
    maf <- 0.05
    
    # test on simulated data
    res <- check_snp_indseq(content, indiv_info, snp_type, locus_count, maf)
    expect_true(res$valid)
    expect_equal(length(res$msg), 0)
    expect_true(is.data.frame(res$locus_count))
    
    ## test missing pop and missing values for X and Y chromosome
    # generate test data
    n_indiv <- 100
    n_pop <- 5
    indiv_id <- str_c("ind", 1:n_indiv)
    indiv_sex <- sort(rep(c("F", "M"), length.out = n_indiv))
    indiv_pop <- sort(rep(str_c("pop", 1:n_pop), length.out = n_indiv))
    data_tab <- data.frame(
        IND = str_c("ind", 1:n_indiv),
        SEX = indiv_sex,
        POP = as.character(indiv_pop),
        A = c(rep(9, sum(indiv_pop == "pop1")), 
              sample(0:2, size = n_indiv - sum(indiv_pop == "pop1"), 
                     replace = TRUE)),
        H = sample(0:1, size = n_indiv, replace = TRUE),
        X = sample(0:2, size = n_indiv, replace = TRUE),
        Y = sample(0:1, size = n_indiv, replace = TRUE),
        M = sample(0:1, size = n_indiv, replace = TRUE),
        stringsAsFactors = FALSE
    )
    
    indiv_info <- data_tab[,1:3]
    content <- t(data_tab[,-(1:3)])
    snp_type <- rownames(content)
    
    locus_count <- data.frame(
        count = rep(1, 5), type = snp_type,
        stringsAsFactors = FALSE
    )
    
    maf <- 0.05
    
    # test on simulated data
    res <- check_snp_indseq(content, indiv_info, snp_type, locus_count, maf)
    expect_false(res$valid)
    expect_equal(length(res$msg), 3)
    
    ## test on SNP data file
    data_file <- "indseq_SNP_sim_dataset_4POP_001.snp"
    data_dir <- file.path(data4test_dir(), "IndSeq_SNP_estim_param")
    data_path <- file.path(data_dir, data_file)
    
    # header
    header <- unname(unlist(
        read.table(file = data_path, skip = 1, nrows = 1)
    ))
    
    # locus type
    candidate_locus <- c("A", "H", "X", "Y", "M")
    locus_encoding <- str_c(header[-(1:3)], collapse = " ")
    locus_count <- Reduce("rbind", lapply(
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
    
    snp_type <- header[-(1:3)]
    indiv_info <- content[,1:3]
    colnames(indiv_info) <- header[1:3]
    content <- t(content[,-(1:3)])
    
    # test on data
    res <- check_snp_indseq(content, indiv_info, snp_type, locus_count, maf)
    expect_true(res$valid)
    expect_true(is.data.frame(res$locus_count))
    expect_equal(res$locus_count$count, 30000)
    expect_equal(res$locus_count$filter, 13046)
    expect_equal(res$locus_count$mono, 0)
    expect_equal(res$locus_count$available, 16954)
})


test_that("read_poolseq_snp_data", {
    
    # good file
    data_file <- "poolseq_SNP_sim_dataset_4POP_cov100_001.snp"
    data_dir <- file.path(data4test_dir(), "PoolSeq_SNP_estim_param")
    res <- read_poolseq_snp_data(data_file, data_dir)
    expect_true(res$valid)
    expect_equal(length(res$msg), 0)
    expect_equal(res$data_file, data_file)
    expect_equal(res$n_loci, 30000)
    expect_true(is.data.frame(res$locus_count))
    expect_equal(res$locus_count$count, 30000)
    expect_equal(res$locus_count$filter, 15612)
    expect_equal(res$locus_count$mono, 5918)
    expect_equal(res$locus_count$available, 14388)
    expect_equal(res$n_pop, 4)
    expect_equal(res$sex_ratio, "NM=1NF")
    expect_equal(res$mrc, 5)
    
    # bad file
    data_file <- "indseq_SNP_sim_dataset_4POP_001.snp"
    data_dir <- file.path(data4test_dir(), "IndSeq_SNP_estim_param")
    res <- read_poolseq_snp_data(data_file, data_dir)
    expect_false(res$valid)
    expect_true(length(res$msg) > 0)
})


test_that("check_snp_poolseq", {
    
    ## test on SNP data file
    data_file <- "poolseq_SNP_sim_dataset_4POP_cov100_001.snp"
    data_dir <- file.path(data4test_dir(), "PoolSeq_SNP_estim_param")
    data_path <- file.path(data_dir, data_file)
    
    # header
    header <- unname(unlist(
        read.table(file = data_path, skip = 1, nrows = 1)
    ))
    
    # data
    content <- read.table(file = data_path, skip = 2)
    
    # test on data
    mrc <- 5
    res <- check_snp_poolseq(content, mrc)
    expect_true(is.data.frame(res$locus_count))
    expect_equal(res$locus_count$count, 30000)
    expect_equal(res$locus_count$filter, 15612)
    expect_equal(res$locus_count$mono, 5918)
    expect_equal(res$locus_count$available, 14388)
})

test_that("read_mss_data", {
    
    ## good file microsat
    data_file <- "simu_dataset_microsat_one_pop_bottleneck_multisamples_001.mss"
    data_dir <- file.path(data4test_dir(), "Microsat")
    res <- read_mss_data(data_file, data_dir)
    expect_true(res$valid)
    expect_equal(length(res$msg), 0)
    expect_equal(res$data_file, data_file)
    expect_equal(res$n_loci, 50)
    expect_true(is.data.frame(res$locus_count))
    expect_equal(sum(res$locus_count$count), res$n_loci)
    expect_equal(res$n_pop, 4)
    expect_equal(res$n_indiv, 80)
    expect_equal(res$pop_size, c(20,20,20,20))
    expect_equal(res$sex_ratio, "NM=1.0NF")
    expect_equal(length(res$locus_type), res$n_loci)
    expect_equal(length(res$locus_name), res$n_loci)
    expect_equal(length(res$locus_mode), res$n_loci)
    expect_equal(length(res$seq_length), res$n_loci)
    expect_equal(sum(!is.na(res$seq_length)), sum(res$locus_mode == "S"))
    
    ## good file microsat/sequence
    data_file <- "mss_example_001.mss"
    data_dir <- data4test_dir("mss")
    res <- read_mss_data(data_file, data_dir)
    expect_true(res$valid)
    expect_equal(length(res$msg), 0)
    expect_equal(res$data_file, data_file)
    expect_equal(res$n_loci, 28)
    expect_true(is.data.frame(res$locus_count))
    expect_equal(sum(res$locus_count$count), res$n_loci)
    expect_equal(res$n_pop, 3)
    expect_equal(res$n_indiv, 60)
    expect_equal(res$pop_size, c(20,20,20))
    expect_equal(res$sex_ratio, "NM=2.33333NF")
    expect_equal(length(res$locus_type), res$n_loci)
    expect_equal(length(res$locus_name), res$n_loci)
    expect_equal(length(res$locus_mode), res$n_loci)
    expect_equal(length(res$seq_length), res$n_loci)
    expect_equal(sum(!is.na(res$seq_length)), sum(res$locus_mode == "S"))
    
    ## bad file
    data_file <- "indseq_SNP_sim_dataset_4POP_001.snp"
    data_dir <- file.path(example_dir(), 
                          "IndSeq_SNP_estim_param")
    res <- read_mss_data(data_file, data_dir)
    expect_false(res$valid)
})

