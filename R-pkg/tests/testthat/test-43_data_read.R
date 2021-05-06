context("43_data_read")

test_that("read_indseq_snp_data", {
    
    data_file <- "indseq_SNP_sim_dataset_4POP_001.snp"
    data_dir <- file.path(data4test_dir(), "IndSeq_SNP_estim_param")
    out <- read_indseq_snp_data(
        data_file, data_dir, expected_data_file = NULL
    )
    expect_true(out$valid)
    
    data_file <- "poolseq_SNP_sim_dataset_4POP_cov100_001.snp"
    data_dir <- file.path(example_dir(), 
                          "PoolSeq_SNP_estim_param")
    out <- read_indseq_snp_data(
        data_file, data_dir, expected_data_file = NULL
    )
    expect_false(out$valid)
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
    expect_false(res$filt)
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


