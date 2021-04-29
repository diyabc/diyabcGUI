context("io_management")

test_that("check_file_name", {
    
    # existing file
    test_proj <- "IndSeq_SNP_estim_param"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "statobsRF.txt")
    expect_true(check_file_name(file_name))
    
    # unexisting file
    file_name <- file.path(test_dir, "toto.txt")
    expect_false(check_file_name(file_name))
    
    # two input
    file_name <- c("1", "2")
    expect_false(check_file_name(file_name))
    
    # non character input
    file_name <- 19
    expect_false(check_file_name(file_name))
})

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


test_that("parse_diyabc_header", {
    # model choice
    file_name <- file.path(example_dir(), 
                           "IndSeq_SNP_model_choice",
                           "headerRF.txt")
    file_type = "text/plain"
    locus_type = "snp"
    expect_equal(
        parse_diyabc_header(file_name, file_type, locus_type),
        list(
            data_file="indseq_SNP_sim_dataset_4POP_001.snp", 
            loci_description="5000 <A> G1 from 1", 
            n_loci_des=1, n_param=13, n_prior=13, n_sumstat=2, 
            raw_cond_list=c("t21>t32", "t42<t21", "t43<t32", "t32>t423", 
                            "t431<t32", "t421<t21"), 
            raw_prior_list=c("N1 N UN[100,10000,0.0,0.0]", 
                             "N2 N UN[100,10000,0.0,0.0]",
                             "N3 N UN[100,10000,0.0,0.0]",
                             "N4 N UN[100,10000,0.0,0.0]",
                             "t423 T UN[10,1000,0.0,0.0]",
                             "ra A UN[0.05,0.95,0.0,0.0]",
                             "t32 T UN[10,1000,0.0,0.0]",
                             "t21 T UN[10,1000,0.0,0.0]",
                             "t421 T UN[10,1000,0.0,0.0]",
                             "t431 T UN[10,1000,0.0,0.0]",
                             "t41 T UN[10,1000,0.0,0.0]",
                             "t42 T UN[10,1000,0.0,0.0]",
                             "t43 T UN[10,1000,0.0,0.0]"), 
            raw_group_prior_list = NULL,
            raw_scenario_list=c(
                "N1 N2 N3 N4\n0 sample 1\n0 sample 2\n0 sample 3\n0 sample 4\nt423 split 4 2 3 ra\nt32 merge 2 3\nt21 merge 1 2",
                "N1 N2 N3 N4\n0 sample 1\n0 sample 2\n0 sample 3\n0 sample 4\nt421 split 4 2 1 ra\nt32 merge 2 3\nt21 merge 1 2",
                "N1 N2 N3 N4\n0 sample 1\n0 sample 2\n0 sample 3\n0 sample 4\nt431 split 4 1 3 ra\nt32 merge 2 3\nt21 merge 1 2",
                "N1 N2 N3 N4\n0 sample 1\n0 sample 2\n0 sample 3\n0 sample 4\nt41 merge 1 4\nt32 merge 2 3\nt21 merge 1 2",
                "N1 N2 N3 N4\n0 sample 1\n0 sample 2\n0 sample 3\n0 sample 4\nt42 merge 2 4\nt32 merge 2 3\nt21 merge 1 2",
                "N1 N2 N3 N4\n0 sample 1\n0 sample 2\n0 sample 3\n0 sample 4\nt43 merge 3 4\nt32 merge 2 3\nt21 merge 1 2"), 
            simu_mode="DRAW UNTIL", 
            valid=TRUE
        )
    )
    
    # parameter estimation
    file_name <- file.path(example_dir(), 
                           "IndSeq_SNP_estim_param",
                           "headerRF.txt")
    file_type = "text/plain"
    locus_type = "snp"
    expect_equal(
        parse_diyabc_header(file_name, file_type, locus_type),
        list(
            data_file="indseq_SNP_sim_dataset_4POP_001.snp", 
            loci_description="5000 <A> G1 from 1", 
            n_loci_des=1, n_param=8, n_prior=8, n_sumstat=2, 
            raw_cond_list=c("t21>t32", "t431<t32"), 
            raw_prior_list=c("N1 N UN[100,10000,0.0,0.0]", 
                             "N2 N UN[100,10000,0.0,0.0]",
                             "N3 N UN[100,10000,0.0,0.0]",
                             "N4 N UN[100,10000,0.0,0.0]",
                             "ra A UN[0.05,0.95,0.0,0.0]",
                             "t32 T UN[10,1000,0.0,0.0]",
                             "t21 T UN[10,1000,0.0,0.0]",
                             "t431 T UN[10,1000,0.0,0.0]"), 
            raw_group_prior_list = NULL,
            raw_scenario_list="N1 N2 N3 N4\n0 sample 1\n0 sample 2\n0 sample 3\n0 sample 4\nt431 split 4 1 3 ra\nt32 merge 2 3\nt21 merge 1 2", 
            simu_mode="DRAW UNTIL", 
            valid=TRUE
        )
    )
    
    # bad input
    file_name <- file.path(test_input_dir(),
                           "bad_files",
                           "header.txt")
    file_type = "text/plain"
    expect_equal(
        parse_diyabc_header(file_name, file_type, locus_type = "snp"),
        list(data_file="indseq_SNP_sim_dataset_4POP_001.snp", 
             loci_description=NULL, 
             n_loci_des=NULL, n_param=13, n_prior=NULL, n_sumstat=2, 
             raw_cond_list=NULL,
             raw_prior_list=NULL, 
             raw_group_prior_list = NULL,
             raw_scenario_list=NULL, 
             simu_mode=NULL, valid=FALSE)
    )
    
    expect_equal(
        parse_diyabc_header(file.path(fs::path_home(), "not_existing_file"), 
                            file_type, locus_type),
        list(data_file=NULL, 
             loci_description=NULL, 
             n_loci_des=NULL, n_param=NULL, n_prior=NULL, n_sumstat=NULL, 
             raw_cond_list=NULL,
             raw_prior_list=NULL, 
             raw_group_prior_list = NULL,
             raw_scenario_list=NULL, 
             simu_mode=NULL, valid=FALSE)
    )
})

test_that("parse_diyabc_header_scenarii", {
    # model choice
    file_name <- file.path(example_dir(), 
                           "IndSeq_SNP_model_choice",
                           "header.txt")
    raw_content <- readLines(file_name)
    raw_content <- raw_content[raw_content != ""]
    
    expect_equal(parse_diyabc_header_scenarii(raw_content[4:12]),
                 list(id=1, n_param=8, prior=0.16667,
                      raw_scenario=str_c(raw_content[5:12], collapse = "\n"),
                      valid=TRUE))
    
    expect_equal(parse_diyabc_header_scenarii(raw_content[5:12]),
                 list(id=NULL, n_param=NULL, prior=NULL, raw_scenario=NULL,
                      valid=FALSE))
})

test_that("parse_diyabc_statobs", {
    
    # existing file
    test_proj <- "IndSeq_SNP_estim_param"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "statobsRF.txt")
    file_type <- "text/plain"
    tmp <- parse_diyabc_statobs(file_name, file_type, n_stat = 130)
    expect_true(tmp$valid)
    
    # bad number of summary stats
    test_proj <- "IndSeq_SNP_estim_param"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "statobsRF.txt")
    file_type <- "text/plain"
    res <- parse_diyabc_statobs(file_name, file_type, n_stat = 120)
    expect_false(res$valid)
    expect_true(length(res$msg) == 1)
    
    # unexisting file
    file_name <- file.path(test_dir, "toto.txt")
    tmp <- parse_diyabc_statobs(file_name, file_type, n_stat = 130)
    expect_false(tmp$valid)
    expect_true(length(tmp$msg) == 1)
    
    # unexisting file and wrong file type
    file_name <- file.path(test_dir, "toto.txt")
    file_type <- "toto"
    tmp <- parse_diyabc_statobs(file_name, file_type, n_stat = 130)
    expect_false(tmp$valid)
    expect_true(length(tmp$msg) == 2)
})
