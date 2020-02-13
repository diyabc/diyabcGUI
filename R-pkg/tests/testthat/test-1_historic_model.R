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
    expected_out <- list(event_type="merge", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "0 merge 1 2 3"
    out <- parse_merge(event)
    expected_out <- list(event_type="merge", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "0 merge 1 x"
    out <- parse_merge(event)
    expected_out <- list(event_type="merge", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "0 merge"
    out <- parse_merge(event)
    expected_out <- list(event_type="merge", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "merge 1 2"
    out <- parse_merge(event)
    expected_out <- list(event_type="merge", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
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
    expected_out <- list(event_type="sample", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "0 sample x"
    out <- parse_sample(event)
    expected_out <- list(event_type="sample", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "sample 1"
    out <- parse_sample(event)
    expected_out <- list(event_type="sample", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "0 sample"
    out <- parse_sample(event)
    expected_out <- list(event_type="sample", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
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
    expected_out <- list(event_type="split", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "0 split 1 2 3"
    out <- parse_split(event)
    expected_out <- list(event_type="split", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "0 split 1 2 3 10"
    out <- parse_split(event)
    expected_out <- list(event_type="split", event_param=10, 
                         event_pop=c(1L,2L,3L), event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "0 split 1 x 3 p"
    out <- parse_split(event)
    expected_out <- list(event_type="split", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "0 split"
    out <- parse_split(event)
    expected_out <- list(event_type="split", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "split 1 2 3 p"
    out <- parse_split(event)
    expected_out <- list(event_type="split", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
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
    expected_out <- list(event_type="varNe", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "0 varNe 1 2 3"
    out <- parse_varNe(event)
    expected_out <- list(event_type="varNe", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "0 varNe"
    out <- parse_varNe(event)
    expected_out <- list(event_type="varNe", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "varNe 1 10"
    out <- parse_varNe(event)
    expected_out <- list(event_type="varNe", event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
})

test_that("parse_event", {
    ## sample
    event <- "0 sample 1"
    out <- parse_event(event)
    expected_out <- list(event_type="sample", event_time=0, event_param=NULL, 
                         event_pop=1L, event_check=TRUE)
    expect_equal(out, expected_out)
    
    event <- "t sample 1"
    out <- parse_event(event)
    expected_out <- list(event_type="sample", event_time="t", event_param=NULL, 
                         event_pop=1L, event_check=TRUE)
    expect_equal(out, expected_out)
    
    event <- "sample 1"
    out <- parse_event(event)
    expected_out <- list(event_type=NULL, event_time=NULL, event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "0 sample"
    out <- parse_event(event)
    expected_out <- list(event_type="sample", event_time=0, event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "0 sampl 1"
    out <- parse_event(event)
    expected_out <- list(event_type=NULL, event_time=NULL, event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
    
    event <- "test"
    out <- parse_event(event)
    expected_out <- list(event_type=NULL, event_time=NULL, event_param=NULL, 
                         event_pop=NULL, event_check=FALSE)
    expect_equal(out, expected_out)
})

test_that("parse_scenario", {
    text = "N1 N2\n0 sample 1\n0 sample 2\nt sample 1\nt2 merge 1 2"
    out <- parse_scenario(text)
    expect_equal(out$npop, 2)
    expect_equal(out$parameters, c("N1", "N2", "t", "t2"))
})

# text = "N1 N2\n0 sample 1\n0 sample 2\nt sample 110sample 1\nt2 merge 1 2"
