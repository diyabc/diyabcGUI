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

test_that("clean_proj_dir", {
    # test dir
    tmp_dir <- mk_proj_dir("test_clean_proj_dir")
    # setup test
    fs::file_create(file.path(tmp_dir, "file1"))
    fs::file_create(file.path(tmp_dir, "file2"))
    fs::dir_create(file.path(tmp_dir, "subdir1"))
    fs::file_create(file.path(tmp_dir, "subdir1", "file1"))
    fs::file_create(file.path(tmp_dir, "subdir1", "file1"))
    fs::dir_create(file.path(tmp_dir, "subdir1", "subdir2"))
    fs::file_create(file.path(tmp_dir, "subdir1", "subdir2", "file1"))
    fs::file_create(file.path(tmp_dir, "subdir1", "subdir2", "file1"))
    # run
    clean_proj_dir(tmp_dir)
    # check
    expect_equal(length(list.files(tmp_dir)), 0)
    
})

test_that("clean_bin_dir", {
    # test cleaning
    clean_bin_dir()
    expect_true(all(list.files(bin_dir()) %in% c("LICENSE", "README.md")))
    # dl latest bin
    dl_all_latest_bin()
})
