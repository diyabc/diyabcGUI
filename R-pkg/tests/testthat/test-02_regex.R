context("02_regex")

test_that("int_regex", {
    expect_true(str_detect("12", int_regex()))
    expect_false(str_detect("xxx", int_regex()))
})

test_that("num_regex", {
    expect_true(str_detect("12", num_regex()))
    expect_true(str_detect("0.12", num_regex()))
    expect_true(str_detect("12.0", num_regex()))
    expect_true(str_detect("12.", num_regex()))
    expect_true(str_detect(".12", num_regex()))
    expect_false(str_detect("xxx", num_regex()))
})

test_that("numexp_regex", {
    expect_true(str_detect("12", numexp_regex()))
    expect_true(str_detect("0.12", numexp_regex()))
    expect_true(str_detect("12.0", numexp_regex()))
    expect_true(str_detect("12.", numexp_regex()))
    expect_true(str_detect(".12", numexp_regex()))
    expect_true(str_detect("12E7", numexp_regex()))
    expect_true(str_detect("0.12e-6", numexp_regex()))
    expect_true(str_detect("12.0E-8", numexp_regex()))
    expect_true(str_detect("12.E-001", numexp_regex()))
    expect_true(str_detect(".12E10", numexp_regex()))
    expect_false(str_detect("xxx", numexp_regex()))
})
