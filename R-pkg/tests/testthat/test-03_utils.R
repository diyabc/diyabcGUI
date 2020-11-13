context("03_utils")

test_that("prog_name", {
    expect_error(prog_name("diyabccc"))
    expect_true(is.character(prog_name("diyabc")))
    expect_true(is.character(prog_name("abcranger")))
})

test_that("find_bin", {
    # bad input
    expect_error(find_bin("diyabcc"))
    # no binary file to find
    clean_bin_dir()
    expect_error(find_bin("diyabc"))
    expect_error(find_bin("abcranger"))
    # dl latest bin
    dl_all_latest_bin()
    # test
    path <- find_bin("diyabc")
    expect_true(file.exists(path))
    path <- find_bin("abcranger")
    expect_true(file.exists(path))
})

test_that("clean_bin_dir", {
    # test cleaning
    clean_bin_dir()
    expect_true(all(list.files(bin_dir()) %in% c("LICENSE", "README.md")))
    # dl latest bin
    dl_all_latest_bin()
})

test_that("get_os", {
    expect_true(is.character(get_os("abcranger")))
})

test_that("dl_latest_bin", {
    # clean existing binary files
    clean_bin_dir()
    # dl diyabc
    prog = "diyabc"
    dl_latest_bin(prog)
    # dl abcranger
    prog = "abcranger"
    dl_latest_bin(prog)
    # check
    expect_error(find_bin("diyabc"), NA)
    expect_error(find_bin("abcranger"), NA)
})

test_that("dl_all_latest_bin", {
    # clean existing binary files
    clean_bin_dir()
    # dl all bins
    dl_all_latest_bin()
    # check
    expect_error(find_bin("diyabc"), NA)
    expect_error(find_bin("abcranger"), NA)
})

test_that("logging", {
    # enable logging
    enable_logging()
    # test
    expect_output(logging("test"), "test")
    # disable logging
    disable_logging()
})

test_that("enable_logging", {
    enable_logging()
    expect_true(getOption("diyabcGUI")$verbose)
})

test_that("disable_logging", {
    disable_logging()
    expect_false(getOption("diyabcGUI")$verbose)
})

test_that("set_diyabcGUI_options", {
    # setup options
    set_diyabcGUI_options()
    # option status
    diyabcGUI_options <- getOption("diyabcGUI")
    # check
    expect_false(is.null(diyabcGUI_options))
    expect_false(is.null(diyabcGUI_options$ncore))
    expect_false(is.null(diyabcGUI_options$simu_loop_size))
    expect_false(is.null(diyabcGUI_options$image_ext))
    expect_false(is.null(diyabcGUI_options$verbose))
    expect_true(is.integer(diyabcGUI_options$ncore))
    expect_true(is.integer(diyabcGUI_options$simu_loop_size))
    expect_true(is.character(diyabcGUI_options$image_ext))
    expect_true(
        diyabcGUI_options$image_ext %in% c("eps", "ps", "tex", "pdf", "jpeg", 
                                           "tiff", "png", "bmp", "svg")
    )
    expect_true(is.logical(diyabcGUI_options$verbose))
})
