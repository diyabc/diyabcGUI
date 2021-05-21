context("historic_model")

test_that("parse_merge", {
    event <- "0 merge 1 2"
    out <- parse_merge(event)
    expected_out <- list(event_type="merge", event_param=NULL, 
                         event_pop=c(1L,2L), event_check=TRUE)
    expect_equal(out, expected_out)
    
    event <- "t merge 1 2"
    out <- parse_merge(event)
    expected_out <- list(event_type="merge", event_param=NULL, 
                         event_pop=c(1L,2L), event_check=TRUE)
    expect_equal(out, expected_out)
    
    event <- "0 merge 1"
    out <- parse_merge(event)
    expect_false(out$event_check)
    
    event <- "0 merge 1 2 3"
    out <- parse_merge(event)
    expect_false(out$event_check)
    
    event <- "0 merge 1 x"
    out <- parse_merge(event)
    expect_false(out$event_check)
    
    event <- "0 merge"
    out <- parse_merge(event)
    expect_false(out$event_check)
    
    event <- "merge 1 2"
    out <- parse_merge(event)
    expect_false(out$event_check)
})

test_that("parse_sample", {
    event <- "0 sample 1"
    out <- parse_sample(event)
    expected_out <- list(event_type="sample", event_param=NULL, 
                         event_pop=1L, event_check=TRUE)
    expect_equal(out, expected_out)
    
    event <- "t sample 1"
    out <- parse_sample(event)
    expected_out <- list(event_type="sample", event_param=NULL, 
                         event_pop=1L, event_check=TRUE)
    expect_equal(out, expected_out)
    
    event <- "0 sample 1 2"
    out <- parse_sample(event)
    expect_false(out$event_check)
    
    event <- "0 sample x"
    out <- parse_sample(event)
    expect_false(out$event_check)
    
    event <- "sample 1"
    out <- parse_sample(event)
    expect_false(out$event_check)
    
    event <- "0 sample"
    out <- parse_sample(event)
    expect_false(out$event_check)
})

test_that("parse_split", {
    event <- "0 split 1 2 3 0.5"
    out <- parse_split(event)
    expected_out <- list(event_type="split", event_param=0.5, 
                         event_pop=c(1L,2L,3L), event_check=TRUE)
    expect_equal(out, expected_out)
    
    event <- "0 split 1 2 3 p"
    out <- parse_split(event)
    expected_out <- list(event_type="split", event_param="p", 
                         event_pop=c(1L,2L,3L), event_check=TRUE)
    expect_equal(out, expected_out)
    
    event <- "t split 1 2 3 p"
    out <- parse_split(event)
    expected_out <- list(event_type="split", event_param="p", 
                         event_pop=c(1L,2L,3L), event_check=TRUE)
    expect_equal(out, expected_out)
    
    event <- "0 split 1 2 0.5"
    out <- parse_split(event)
    expect_false(out$event_check)
    
    event <- "0 split 1 2 3"
    out <- parse_split(event)
    expect_false(out$event_check)
    
    event <- "0 split 1 2 3 10"
    out <- parse_split(event)
    expect_false(out$event_check)
    
    event <- "0 split 1 x 3 p"
    out <- parse_split(event)
    expect_false(out$event_check)
    
    event <- "0 split"
    out <- parse_split(event)
    expect_false(out$event_check)
    
    event <- "split 1 2 3 p"
    out <- parse_split(event)
    expect_false(out$event_check)
})

test_that("parse_varNe", {
    event <- "0 varNe 1 10"
    out <- parse_varNe(event)
    expected_out <- list(event_type="varNe", event_param=10L, 
                         event_pop=1L, event_check=TRUE)
    expect_equal(out, expected_out)
    
    event <- "t varNe 1 10"
    out <- parse_varNe(event)
    expected_out <- list(event_type="varNe", event_param=10L, 
                         event_pop=1L, event_check=TRUE)
    expect_equal(out, expected_out)
    
    event <- "t varNe 1 Ne"
    out <- parse_varNe(event)
    expected_out <- list(event_type="varNe", event_param="Ne", 
                         event_pop=1L, event_check=TRUE)
    expect_equal(out, expected_out)
    
    event <- "0 varNe 1"
    out <- parse_varNe(event)
    expect_false(out$event_check)
    
    event <- "0 varNe 1 2 3"
    out <- parse_varNe(event)
    expect_false(out$event_check)
    
    event <- "0 varNe"
    out <- parse_varNe(event)
    expect_false(out$event_check)
    
    event <- "varNe 1 10"
    out <- parse_varNe(event)
    expect_false(out$event_check)
})

test_that("parse_event", {
    ## sample
    event <- "0 sample 1"
    out <- parse_event(event)
    expected_out <- list(event_type="sample", event_time=0, event_param=NULL, 
                         event_pop=1L, event_valid=TRUE)
    expect_equal(out, expected_out)
    
    event <- "t sample 1"
    out <- parse_event(event)
    expected_out <- list(event_type="sample", event_time="t", event_param=NULL, 
                         event_pop=1L, event_valid=TRUE)
    expect_equal(out, expected_out)
    
    event <- "sample 1"
    out <- parse_event(event)
    expect_false(out$event_valid)
    
    event <- "0 sample"
    out <- parse_event(event)
    expect_false(out$event_valid)
    
    event <- "0 sampl 1"
    out <- parse_event(event)
    expect_false(out$event_valid)
    
    event <- "test"
    out <- parse_event(event)
    expect_false(out$event_valid)
})

test_that("parse_scenario", {
    text <- "N1 N2\n0 sample 1\n0 sample 2\nt sample 1\nt2 merge 1 2"
    out <- parse_scenario(text)
    expect_true(out$valid)
    expect_identical(
        out,
        list(
            npop = 2L, nevent = 4, 
            event_type = c("sample", "sample", "sample", "merge"), 
            event_time = list(0, 0, "t", "t2"), 
            event_pop = list(1L, 2L, 1L, 1:2), 
            event_param = list(NULL, NULL, NULL, NULL), 
            valid = TRUE, Ne_param = c("N1", "N2"), 
            Ne_list_0 = c("N1", "N2"), rate_param = NULL, 
            time_param = c("t", "t2"), msg_list = list()
        )
    )
    
    text <- "N1 N2 N3\n0 sample 1\n0 sample 2\n0 sample 3\nt merge 2 3"
    out <- parse_scenario(text)
    expect_true(out$valid)
    expect_identical(
        out,
        list(
            npop = 3L, nevent = 4, 
            event_type = c("sample", "sample", "sample", "merge"), 
            event_time = list(0, 0, 0, "t"), 
            event_pop = list(1L, 2L, 3L, 2:3), 
            event_param = list(NULL, NULL, NULL, NULL), 
            valid = TRUE, Ne_param = c("N1", "N2", "N3"), 
            Ne_list_0 = c("N1", "N2", "N3"), rate_param = NULL, 
            time_param = "t", msg_list = list()
        )
    )
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 merge 2 3",
        "t3 varNe 2 N2+N3",
        "t2 merge 1 2",
        "t2 varNe 1 N1+N2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    expect_true(parsed_scenario$valid)
    expect_identical(
        parsed_scenario,
        list(
            npop = 3L, nevent = 7, 
            event_type = c("sample", "sample", "sample", "merge", 
                           "varNe", "merge", "varNe"), 
            event_time = list(0, 0, 0, "t3", "t3", "t2", "t2"), 
            event_pop = list(1L, 2L, 3L, 2:3, 2L, 1:2, 1L), 
            event_param = list(NULL, NULL, NULL, NULL, "N2+N3", NULL, "N1+N2"), 
            valid = TRUE, Ne_param = c("N2", "N3", "N1"), 
            Ne_list_0 = c("N1", "N2", "N3"), rate_param = NULL, 
            time_param = c("t3", "t2"), msg_list = list()
        )
    )
    
    text <- str_c(
        "N1 N2", 
        "0 sample 1", 
        "0 sample 2", 
        "t sample 1", 
        "t2 merge 1 2", 
        "t3 merge 1 2",
        sep = "\n"
    )
    out <- parse_scenario(text)
    expect_false(out$valid)
    expect_identical(
        out$msg_list,
        list("Use of non-existing population in event at row 6")
    )
    
    text <- str_c(
        "N1 N2 N3 N4",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "0 sample 4", 
        "t3 split 3 1 2 r3",
        "t4 split 4 1 2 r4", 
        "t2 merge 1 2",
        sep = "\n")
    out <- parse_scenario(text)
    expect_true(out$valid)
    expect_identical(
        out,
        list(
            npop = 4L, nevent = 7, 
            event_type = c("sample", "sample", "sample", 
                           "sample", "split", "split", "merge"), 
            event_time = list(0, 0, 0, 0, "t3", "t4", "t2"), 
            event_pop = list(1L, 2L, 3L, 4L, c(3L, 1L, 2L), 
                             c(4L, 1L, 2L), 1:2), 
            event_param = list(NULL, NULL, NULL, NULL, "r3", "r4", NULL), 
            valid = TRUE, Ne_param = c("N1", "N2", "N3", "N4"), 
            Ne_list_0 = c("N1", "N2", "N3", "N4"), 
            rate_param = c("r3", "r4"), time_param = c("t3", "t4", "t2"), 
            msg_list = list()
        )
    )
    
    text <- str_c(
        "N1 N2 N3", 
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t sample 1",
        "t21 split 3 1 2 ra",
        "t2 merge 1 2",
        sep = "\n"
    )
    out <- parse_scenario(text)
    expect_true(out$valid)
    
    # scenario with ghost pop
    text <- str_c(
        "N1 N2 N3 Nbc3",
        "0 sample 1",
        "50 sample 2",
        "0 sample 3",
        "t3-DB3 VarNe 3 NF3",
        "t3 merge 4 3",
        "t3lb merge 1 4",
        "t2 merge 1 2",
        sep = "\n"
    )
    out <- parse_scenario(text)
    expect_true(out$valid)
    
    # more complicated scenario with ghost pop
    text <- str_c(
        "N1 NKo NKa NJ12 N2 N3 N4 Na",
        "0 sample 1",
        "0 sample 2",
        "0 sample 3",
        "0 sample 4",
        "0 sample 5",
        "0 sample 6",
        "0 sample 7",
        "tJ12-DBJ12 varNe 4 NJ12B",
        "tJ12 VarNe 4 NgJ12",
        "tgJ12 merge 8 4",
        "tKa-DBKa varNe 3 NKaB",
        "tKa VarNe 3 NgKa",
        "tgKa merge 8 3",
        "tKo-DBKo varNe 2 NKoB",
        "tKo VarNe 2 NgKo",
        "tgKo merge 8 2",
        "t1 merge 8 1",
        "t2 merge 8 5",
        "t3 merge 8 6",
        "t4 merge 8 7",
        "ta VarNe 8 Naold",
        sep = "\n"
    )
    out <- parse_scenario(text)
    expect_true(out$valid)
})

test_that("default_param_prior", {
    
    expect_null(default_param_prior(NULL))
    expect_null(default_param_prior(list()))
    
    # scenario with issue
    expect_null(default_param_prior(str_c(
        "N1 N2", 
        "0 sample", 
        "0 sample 2", 
        "t2 merge 1 2", 
        sep = "\n"
    )))
    
    # list of scenario
    scen_list <- c(
        str_c(
            "N1 N2", 
            "0 sample 1", 
            "0 sample 2", 
            "t sample 1", 
            "t2 merge 1 2", 
            sep = "\n"
        ),
        str_c(
            "N1 N2 N3", 
            "0 sample 1", 
            "0 sample 2", 
            "0 sample 3",
            "t sample 1",
            "t21 split 3 1 2 ra",
            "t2 merge 1 2",
            sep = "\n"
        ),
        str_c(
            "N1 N2 N3 Nbc3",
            "0 sample 1",
            "50 sample 2",
            "0 sample 3",
            "t3-DB3 VarNe 3 NF3",
            "t3 merge 4 3",
            "t3lb merge 1 4",
            "t2 merge 1 2",
            sep = "\n"
        ),
        str_c(
            "N1 N2 N3",
            "0 sample 1", 
            "0 sample 2", 
            "0 sample 3",
            "t3 merge 2 3",
            "t3 varNe 2 N2+N3",
            "t2 merge 1 2",
            "t2 varNe 1 N1+N2",
            sep = "\n"
        )
    )
    
    res <- default_param_prior(scen_list)
    expect_equal(length(res), 12)
})

test_that("default_prior_num_val", {
    expect_equal(default_prior_num_val("A", "UN"), c(0.01, 0.99, 0, 0))
    expect_equal(default_prior_num_val("A", "NO"), c(0.01, 0.99, 0.5, 0.1))
    expect_equal(default_prior_num_val("N", "UN"), c(10, 10000, 0, 0))
    expect_equal(default_prior_num_val("N", "NO"), c(10, 10000, 1000, 100))
})

test_that("clean_param_prior", {
    
    expect_null(clean_param_prior(NULL, NULL))
    expect_null(clean_param_prior(list(), list()))
    
    # scenario with issue
    expect_null(clean_param_prior(NULL, str_c(
        "N1 N2", 
        "0 sample", 
        "0 sample 2", 
        "t2 merge 1 2", 
        sep = "\n"
    )))
    
    # list of scenario
    scen_list <- c(
        str_c(
            "N1 N2", 
            "0 sample 1", 
            "0 sample 2", 
            "t sample 1", 
            "t2 merge 1 2", 
            sep = "\n"
        ),
        str_c(
            "N1 N2 N3", 
            "0 sample 1", 
            "0 sample 2", 
            "0 sample 3",
            "t sample 1",
            "t21 split 3 1 2 ra",
            "t2 merge 1 2", 
            "t3 merge 1 2",
            sep = "\n"
        ),
        str_c(
            "N1 N2 N3 Nbc3",
            "0 sample 1",
            "50 sample 2",
            "0 sample 3",
            "t3-DB3 VarNe 3 NF3",
            "t3 merge 4 3",
            "t3lb merge 1 4",
            "t2 merge 1 2",
            sep = "\n"
        ),
        str_c(
            "N1 N2 N3",
            "0 sample 1", 
            "0 sample 2", 
            "0 sample 3",
            "t3 merge 2 3",
            "t3 varNe 2 N2+N3",
            "t2 merge 1 2",
            "t2 varNe 1 N1+N2",
            sep = "\n"
        )
    )
    
    prior_list <- default_param_prior(scen_list)
    
    # test with prior list corresponding to scenario list
    res <- clean_param_prior(prior_list, scen_list)
    expect_equal(res, prior_list)
    
    # remove some scenario
    scen_list <- scen_list[1:2]
    res <- clean_param_prior(prior_list, scen_list)
    expect_equal(res, default_param_prior(scen_list))
})

test_that("get_param_name", {
    prior <- "N1 N UN[10,10000,0.0,0.0]"
    expect_equal(get_param_name(prior), "N1")
    
    prior <- " N [10,10000,0.0,0.0]"
    expect_null(get_param_name(prior))
})

test_that("get_prior_distrib", {
    prior <- "N1 N UN[10,10000,0.0,0.0]"
    expect_equal(get_prior_distrib(prior), "UN")
    
    prior <- "N1 N UNUN[10,10000,0.0,0.0]"
    expect_null(get_prior_distrib(prior))
    
    prior <- "N1 N [10,10000,0.0,0.0]"
    expect_null(get_prior_distrib(prior))
})

test_that("get_prior_num_val", {
    prior <- "N1 N UN[10,10000,0.0,0.0]"
    expect_equal(get_prior_num_val(prior), c(10, 10000, 0, 0))
    
    prior <- "N1 N UN[10,10000,0.0,]"
    expect_null(get_prior_num_val(prior))
})
