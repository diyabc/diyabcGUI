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
    
    # existing file
    test_proj <- "IndSeq_SNP_estim_param"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "headerRF.txt")
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

test_that("parse_scenario_header", {
    # model choice
    test_proj <- "IndSeq_SNP_model_choice"
    test_dir <- file.path(data4test_dir(), test_proj)
    file_name <- file.path(test_dir, "headerRF.txt")
    content <- str_split(read_file(file_name), "\n", simplify = TRUE)
    
    res <- parse_scenario_header(content[5:13])
    
    expect_equal(
        res,
        list(
            valid=TRUE, id=1, n_param=8, prior=0.16667,
            scenario=str_c(content[6:13], collapse = "\n")
        )
    )
    
    res <- parse_scenario_header(content[6:13])
    
    expect_false(res$valid)
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
