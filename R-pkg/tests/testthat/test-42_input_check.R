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

test_that("check_header_loci_desc", {
    ## MSS
    expect_true(check_header_loci_desc(
        "Locus_xxx <A> [M] G1 2 40", type = "mss"
    ))
    expect_true(check_header_loci_desc(
        "Locus_yyy <M> [S] G2 1000", type = "mss"
    ))
    
    expect_false(check_header_loci_desc(
        "Locus_xxx <A> [M G1 2 40", type = "mss"
    ))
    expect_false(check_header_loci_desc(
        "<A> [M] G1 2 40", type = "mss"
    ))
    expect_false(check_header_loci_desc(
        "Locus_xxx <A> [M] G1 2", type = "mss"
    ))
    expect_false(check_header_loci_desc(
        "Locus_yyy <M [S] G2 1000", type = "mss"
    ))
    expect_false(check_header_loci_desc(
        "Locus_yyy <M [S] G2 1000 3000", type = "mss"
    ))
    
    ## SNP
    expect_true(check_header_loci_desc(
        "5000 <A> G1 from 1", type = "snp"
    ))
    expect_true(check_header_loci_desc(
        "5000 <X> G2 from 10", type = "snp"
    ))
    
    expect_false(check_header_loci_desc(
        "<X> G2 from 10", type = "snp"
    ))
    expect_false(check_header_loci_desc(
        "5000 <X> G2 from ", type = "snp"
    ))
    expect_false(check_header_loci_desc(
        "5000 <X> G from 10", type = "snp"
    ))
})

