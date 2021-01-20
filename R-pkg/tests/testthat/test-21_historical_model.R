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
    
})
