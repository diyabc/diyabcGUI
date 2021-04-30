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


test_that("check_header_group_prior", {
    ## MSS
    expect_true(check_header_group_prior(
        "Locus_xxx <A> [M] G1 2 40", type = "mss"
    ))
    expect_true(check_header_group_prior(
        "Locus_yyy <M> [S] G2 1000", type = "mss"
    ))
    
    expect_false(check_header_group_prior(
        "Locus_xxx <A> [M G1 2 40", type = "mss"
    ))
    expect_false(check_header_group_prior(
        "<A> [M] G1 2 40", type = "mss"
    ))
    expect_false(check_header_group_prior(
        "Locus_xxx <A> [M] G1 2", type = "mss"
    ))
    expect_false(check_header_group_prior(
        "Locus_yyy <M [S] G2 1000", type = "mss"
    ))
    expect_false(check_header_group_prior(
        "Locus_yyy <M [S] G2 1000 3000", type = "mss"
    ))
    
    ## SNP
    expect_true(check_header_group_prior(
        "5000 <A> G1 from 1", type = "snp"
    ))
    expect_true(check_header_group_prior(
        "5000 <X> G2 from 10", type = "snp"
    ))
    
    expect_false(check_header_group_prior(
        "<X> G2 from 10", type = "snp"
    ))
    expect_false(check_header_group_prior(
        "5000 <X> G2 from ", type = "snp"
    ))
    expect_false(check_header_group_prior(
        "5000 <X> G from 10", type = "snp"
    ))
})


test_that("check_header_group_prior", {
    ## MSS
    content <- c(
        "MEANMU UN[1.00E-004,1.00E-3,0.0005,2]",
        "GAMMU GA[1.00E-005,1.00E-002,Mean_u,2]",
        "MEANP UN[1.00E-001,3.00E-001,0.22,2]",
        "GAMP GA[1.00E-002,9.00E-001,Mean_P,2]",
        "MEANSNI LU[1.00E-008,1.00E-005,1.00E-007,2]",
        "GAMSNI GA[1.00E-009,1.00E-004,Mean_u_SNI,2]"
    )
    expect_true(check_header_group_prior(content, type = "M"))
    
    content <- c(
        "MEANMU UN[1.00E-004,1.00E-3,0.0005,2]",
        "GAMMU GA[1.00E-005,1.00E-002,Mean_u,2]",
        "MEANP UN[1.00E-001,3.00E-001,0.22,2]",
        "GAMP GA[1.00E-002,9.00E-001,Mean_P,2]"
    )
    expect_false(check_header_group_prior(content, type = "M"))
    
    content <- c(
        "MEANMU UN[1.00E-004,1.00E-3,0.0005,2",
        "GAMMU GA[1.00E-005,1.00E-002,Mean_u,2]",
        "MEANP UN[1.00E-001,3.00E-001,0.22,2]",
        "GAMP GA[1.00E-002,9.00E-001,Mean_P,2]",
        "MEANSNI LU[1.00E-008,1.00E-005,1.00E-007,2]",
        "GAMSNI GA[1.00E-009,1.00E-004,Mean_u_SNI,2]"
    )
    expect_false(check_header_group_prior(content, type = "M"))
    
    content <- c(
        "MEANMU UN[1.00E-9,1.00E-7,5E-9,2]",
        "GAMMU GA[1.00E-9,1.00E-6,Mean_u,2]",
        "MEANK1 UN[0.050,20,10,2]",
        "GAMK1 GA[0.050,20,Mean_k1,2]",
        "MEANK2 UN[0.050,20,10,2]",
        "GAMK2 GA[0.050,20,Mean_k2,2]",
        "MODEL TN 10 2.00"
    )
    expect_false(check_header_group_prior(content, type = "M"))
    
    
    ## SNP
    content <- c(
        "MEANMU UN[1.00E-9,1.00E-7,5E-9,2]",
        "GAMMU GA[1.00E-9,1.00E-6,Mean_u,2]",
        "MEANK1 UN[0.050,20,10,2]",
        "GAMK1 GA[0.050,20,Mean_k1,2]",
        "MEANK2 UN[0.050,20,10,2]",
        "GAMK2 GA[0.050,20,Mean_k2,2]",
        "MODEL TN 10 2.00"
    )
    expect_true(check_header_group_prior(content, type = "S"))
    
    content <- c(
        "MEANMU UN[1.00E-9,1.00E-7,5E-9,2]",
        "GAMMU GA[1.00E-9,1.00E-6,Mean_u,2]",
        "MEANK1 UN[0.050,20,10,2]",
        "GAMK1 GA[0.050,20,Mean_k1,2]",
        "MEANK2 UN[0.050,20,10,2]",
        "GAMK2 GA[0.050,20,Mean_k2,2]",
        "MODEL TN 10.99 2.00"
    )
    expect_false(check_header_group_prior(content, type = "S"))
    
    content <- c(
        "MEANMU UN[1.00E-9,1.00E-7,5E-9,2]",
        "GAMMU GA[1.00E-9,1.00E-6,Mean_u,2]",
        "MEANK2 UN[0.050,20,10,2]",
        "GAMK2 GA[0.050,20,Mean_k2,2]",
        "MODEL TN 10 2.00"
    )
    expect_false(check_header_group_prior(content, type = "S"))
    
    content <- c(
        "MEANMU UN[1.00E-9,1.00E-7,5E-9,2]",
        "GAMMU GA[1.00E-9,1.00E-6,Mean_u,2]",
        "MEANK1 UN[0.050,20,10,2]",
        "GAMK1 GA[0.050,20,0,2]",
        "MEANK2 UN[0.050,20,10,2]",
        "GAMK2 GA[0.050,20,Mean_k2,2]",
        "MODEL TN 10 2.00"
    )
    expect_false(check_header_group_prior(content, type = "S"))
    
    
    content <- c(
        "MEANMU UN[1.00E-004,1.00E-3,0.0005,2]",
        "GAMMU GA[1.00E-005,1.00E-002,Mean_u,2]",
        "MEANP UN[1.00E-001,3.00E-001,0.22,2]",
        "GAMP GA[1.00E-002,9.00E-001,Mean_P,2]",
        "MEANSNI LU[1.00E-008,1.00E-005,1.00E-007,2]",
        "GAMSNI GA[1.00E-009,1.00E-004,Mean_u_SNI,2]"
    )
    expect_false(check_header_group_prior(content, type = "S"))
})

