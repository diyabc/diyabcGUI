context("test-62_analysis_project_io")

test_that("proj_file_input", {
    
    ## clean project files
    test_proj <- "IndSeq_SNP_estim_param"
    
    # setup test case
    tmp_dir <- file.path(mk_proj_dir("test-62_analysis_project_io"), test_proj)
    test_dir <- file.path(data4test_dir(), test_proj)
    fs::dir_copy(test_dir, tmp_dir, overwrite = TRUE)
    
    tmp_file_list <- list.files(tmp_dir)
    tmp_file_info <- file.info(
        file.path(tmp_dir, tmp_file_list)
    )
    
    # input parameters
    file_input <- data.frame(
        name = tmp_file_list,
        size = tmp_file_info$size,
        type = ifelse(
            tmp_file_info$isdir,
            "diyabc_dir",
            "diyabc_file"
        ),
        datapath = file.path(tmp_dir, tmp_file_list),
        stringsAsFactors = FALSE
    )
    
    # specific file type
    ind <- which(out$file_input$name == "headerRF.txt")
    file_input$type[ind] <- "text/plain"
    ind <- which(file_input$name == "header.txt")
    file_input$type[ind] <- "text/plain"
    ind <- which(file_input$name == "reftableRF.bin")
    file_input$type[ind] <- "application/octet-stream"
    ind <- which(file_input$name == "statobsRF.txt")
    file_input$type[ind] <- "text/plain"
    
    # tmp proj dir
    proj_dir <- mk_proj_dir("test-62_analysis_project_io")
    
    # run
    proj_file <- proj_file_input(file_input, proj_dir)
    
    expect_true(proj_file$valid)
    expect_true(length(list.files(proj_dir)) == length(list.files(test_dir)))
    
    
    ## no input
    proj_file <- proj_file_input(head(file_input, 0), proj_dir)
    
    expect_false(proj_file$valid)
    expect_true(length(proj_file$msg) == 1)
    
    
    ## clean project zip file with root directory
    test_proj <- "IndSeq_SNP_estim_param"
    
    # setup test case
    tmp_dir <- mk_proj_dir("test-62_analysis_project_io")
    test_dir <- file.path(data4test_dir(), test_proj)
    zip_file <- file.path(tmp_dir, paste0(test_proj, ".zip"))
    zip::zip(
        zipfile = zip_file, files = test_proj, root = data4test_dir(),
        recurse = TRUE, include_directories = TRUE
    )
    
    tmp_file <- list.files(tmp_dir)
    tmp_file_info <- file.info(
        file.path(tmp_dir, tmp_file)
    )
    
    # input parameters
    file_input <- data.frame(
        name = tmp_file,
        size = tmp_file_info$size,
        type = "application/zip",
        datapath = file.path(tmp_dir, tmp_file),
        stringsAsFactors = FALSE
    )
    
    # run
    proj_file <- proj_file_input(file_input, proj_dir)
    
    expect_true(proj_file$valid)
    expect_true(length(list.files(proj_dir)) == length(list.files(test_dir)))
})



test_that("proj_zip_file_input", {

    ## clean project zip file with root directory
    test_proj <- "IndSeq_SNP_estim_param"

    # setup test case
    tmp_dir <- mk_proj_dir("test-62_analysis_project_io")
    test_dir <- file.path(data4test_dir(), test_proj)
    zip_file <- file.path(tmp_dir, paste0(test_proj, ".zip"))
    zip::zip(
        zipfile = zip_file, files = test_proj, root = data4test_dir(),
        recurse = TRUE, include_directories = TRUE
    )

    tmp_file <- list.files(tmp_dir)
    tmp_file_info <- file.info(
        file.path(tmp_dir, tmp_file)
    )

    # input parameters
    file_input <- data.frame(
        name = tmp_file,
        size = tmp_file_info$size,
        type = "application/zip",
        datapath = file.path(tmp_dir, tmp_file),
        stringsAsFactors = FALSE
    )
    
    # run
    zip_proj <- proj_zip_file_input(file_input)
    
    expect_true(zip_proj$zip_file)
    expect_true(zip_proj$valid)
    expect_true(length(zip_proj$msg) == 0)
    expect_true(nrow(zip_proj$file_input) == length(list.files(test_dir)))
    
    ## providing two zip files
    file_input <- rbind(file_input, file_input)
    
    # run
    zip_proj <- proj_zip_file_input(file_input)
    
    expect_true(zip_proj$zip_file)
    expect_false(zip_proj$valid)
    expect_true(length(zip_proj$msg) == 1)
    
    
    ## clean project zip file without root directory
    test_proj <- "IndSeq_SNP_estim_param"
    
    # setup test case
    tmp_dir <- mk_proj_dir("test-62_analysis_project_io")
    test_dir <- file.path(data4test_dir(), test_proj)
    zip_file <- file.path(tmp_dir, paste0(test_proj, ".zip"))
    zip::zip(
        zipfile = zip_file, files = list.files(test_dir), root = test_dir,
        recurse = TRUE, include_directories = TRUE
    )
    
    tmp_file <- list.files(tmp_dir)
    tmp_file_info <- file.info(
        file.path(tmp_dir, tmp_file)
    )
    
    # input parameters
    file_input <- data.frame(
        name = tmp_file,
        size = tmp_file_info$size,
        type = "application/zip",
        datapath = file.path(tmp_dir, tmp_file),
        stringsAsFactors = FALSE
    )
    
    # run
    zip_proj <- proj_zip_file_input(file_input)
    
    expect_true(zip_proj$zip_file)
    expect_true(zip_proj$valid)
    expect_true(length(zip_proj$msg) == 0)
    expect_true(nrow(zip_proj$file_input) == length(list.files(test_dir)))
    
    
    ## providing empty zip file
    
    # setup test case
    tmp_dir <- mk_proj_dir("test-62_analysis_project_io")
    test_dir <- file.path(tmp_dir, "empty_project")
    fs::dir_create(test_dir)
    zip_file <- file.path(tmp_dir, "empty_project.zip")
    zip::zip(
        zipfile = zip_file, files = "empty_project", root = tmp_dir,
        recurse = TRUE, include_directories = TRUE
    )
    fs::dir_delete(test_dir)
    
    tmp_file <- list.files(tmp_dir)
    tmp_file_info <- file.info(
        file.path(tmp_dir, tmp_file)
    )
    
    # input parameters
    file_input <- data.frame(
        name = tmp_file,
        size = tmp_file_info$size,
        type = "application/zip",
        datapath = file.path(tmp_dir, tmp_file),
        stringsAsFactors = FALSE
    )
    
    # run
    zip_proj <- proj_zip_file_input(file_input)
    
    expect_true(zip_proj$zip_file)
    expect_false(zip_proj$valid)
    expect_true(length(zip_proj$msg) == 1)
})
