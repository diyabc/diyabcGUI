context("historic_model")

test_that("parse_scenario", {
    text = "N1 N2\n0 sample 1\n0 sample 2\nt sample 1\nt2 merge 1 2"
    out <- parse_scenario(text)
    expect_equal(out$npop, 2)
    expect_equal(out$parameters, c("N1", "N2", "t", "t2"))
})
