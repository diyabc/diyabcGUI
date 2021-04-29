context("42_input_check")

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