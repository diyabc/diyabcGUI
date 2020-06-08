context("io_management")

test("check_filename", {
    expect_true(check_filename(system.file("DESCRIPTION", 
                                           package = "diyabcGUI")))
    expect_false(check_filename(5))
    expect_false(check_filename(file.path(fs::path_home(), 
                                          "not_existing_file")))
})

test("parse_diyabc_project", {
    filename <- file.path(example_dir(),
                          "IndSeq_SNP_estim_param",
                          "diyabcGUI_proj.txt")
    type = "text/plain"
    expect_equal(parse_diyabc_project(filename, type),
                 list(proj_name="IndSeq_SNP_estim_param",
                      valid=TRUE))
    expect_equal(parse_diyabc_project(filename, type = "bin"),
                 list(proj_name=NULL,
                      valid=FALSE))
    expect_equal(parse_diyabc_project(file.path(fs::path_home(), 
                                                "not_existing_file"), 
                                      type),
                 list(proj_name=NULL,
                      valid=FALSE))
})