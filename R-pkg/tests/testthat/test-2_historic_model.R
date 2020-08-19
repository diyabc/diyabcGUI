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
    expect_equal(out$npop, 2)
    expect_equal(out$nevent, 4)
    # FIXME
    # expect_equal()
    # expect_equal()
    # expect_equal()
    # expect_equal()
    # expect_equal()
    
    text <- "N1 N2\n0 sample 1\n0 sample 2\nt sample 1\nt2 merge 1 2\nt3 merge 1 2"
    out <- parse_scenario(text)
    expect_false(out$valid)
    
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
    
})
