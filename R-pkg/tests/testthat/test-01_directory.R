context("01_directory")

test_that("bin_dir", {
    path <- bin_dir()
    expect_true(dir.exists(path))
})

test_that("data4test_dir", {
    path <- data4test_dir()
    expect_true(dir.exists(path))
    
    path <- data4test_dir("mss")
    expect_true(dir.exists(path))
    
    expect_error(data4test_dir("msss"))
})

test_that("example_dir", {
    path <- example_dir()
    expect_true(dir.exists(path))
})

test_that("help_dir", {
    path <- help_dir()
    expect_true(dir.exists(path))
})

test_that("test_input_dir", {
    path <- test_input_dir()
    expect_true(dir.exists(path))
})

test_that("mk_proj_dir", {
    path <- mk_proj_dir()
    expect_true(dir.exists(path))
    expect_error(fs::dir_delete(path), NA)
})