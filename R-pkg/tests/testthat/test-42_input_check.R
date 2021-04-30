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


test_that("check_header_prior", {
    expect_true(check_header_prior("N N UN[100,10000,0.0,0.0]"))
    expect_true(check_header_prior("ra A UN[0.05,0.95,0.0,0.0]"))
    expect_true(check_header_prior("t T UN[10,1000,0.0,0.0]"))
    
    expect_false(check_header_prior("N N UN[100,10000,0.0]"))
    expect_false(check_header_prior("ra A [0.05,0.95,0.0,0.0]"))
    expect_false(check_header_prior("T UN[10,1000,0.0,0.0]"))
})

test_that("check_header_cond", {
    expect_true(check_header_cond("t1>t2"))
    expect_true(check_header_cond("t1>=t2"))
    expect_true(check_header_cond("t1<t2"))
    expect_true(check_header_cond("t1=<t2"))
    
    expect_false(check_header_cond("t1>"))
    expect_false(check_header_cond("t1>"))
    expect_false(check_header_cond("<t2"))
})

