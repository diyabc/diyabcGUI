context("random_forest_tools")

test_that("abcranger_run and cleanup_abcranger_run", {
    
    test_proj <- "IndSeq_SNP_model_choice"
    test_dir <- file.path(data4test_dir(), test_proj)
    tmp_dir = mk_proj_dir("test_abcranger_run")
    
    # copy header and data file from example
    lapply(
        c("headerRF.txt", "statobsRF.txt", "reftableRF.bin", 
          "indseq_SNP_sim_dataset_4POP_001.snp"),
        function(filename) {
            fs::file_copy(
                path = file.path(test_dir, filename),
                new_path = file.path(tmp_dir, filename)
            )
        }
    )
    
    ## parameter estimation
    mode <- "param_estim"
    n_rec <- 500
    min_node_size <- 0
    n_tree <- 50
    noise_columns <- 5
    no_linear <- FALSE
    pls_max_var <- 0.9
    chosen_scenario <- 1
    noob <- 50
    parameter <- "N1"
    groups <- NULL
    
    # try run
    run_proc <- abcranger_run(
        tmp_dir, mode, n_rec, min_node_size, n_tree, noise_columns, no_linear, 
        pls_max_var, chosen_scenario, noob, parameter, groups
    )
    
    # expect_true(run_proc$is_alive())
    # run_proc$kill()
    run_proc$wait()
    
    expect_equal(run_proc$get_exit_status(), 0)
    
    # check project directory
    expected_files <- c(
        "estimparam_out.importance", "estimparam_out.oobstats", 
        "estimparam_out.plsweights", "estimparam_out.predweights", 
        "estimparam_out.ooberror", "estimparam_out.plsvar", 
        "estimparam_out.predictions", "estimparam_out.settings"
    )
    expect_true(all(expected_files %in% list.files(tmp_dir)))
    
    ## model choice
    mode <- "model_choice"
    n_rec <- 500
    min_node_size <- 0
    n_tree <- 50
    noise_columns <- 5
    no_linear <- FALSE
    pls_max_var <- 0.9
    chosen_scenario <- NULL
    noob <- NULL
    parameter <- NULL
    groups <- "1,2,3;4,5"
    
    # try run
    run_proc <- abcranger_run(
        tmp_dir, mode, n_rec, min_node_size, n_tree, noise_columns, no_linear, 
        pls_max_var, chosen_scenario, noob, parameter, groups
    )
    
    # expect_true(run_proc$is_alive())
    # run_proc$kill()
    run_proc$wait()
    
    expect_equal(run_proc$get_exit_status(), 0)
    
    # check project directory
    expected_files <- c(
        "modelchoice_out.confusion", "modelchoice_out.importance", 
        "modelchoice_out.lda", "modelchoice_out.ooberror", 
        "modelchoice_out.predictions", "modelchoice_out.settings"
    )
    expect_true(all(expected_files %in% list.files(tmp_dir)))
    
    ## clean up
    cleanup_abcranger_run(tmp_dir) # does nothing at the moment
})

test_that("parse_abcranger_group", {
    
    txt <- '1,2,3;4,5,6'
    n_scenario <- 6
    res <- parse_abcranger_group(txt, n_scenario)
    expect_true(res$valid)
    
    txt <- '1,2,3;4,5'
    n_scenario <- 6
    res <- parse_abcranger_group(txt, n_scenario)
    expect_true(res$valid)
    
    txt <- '1,2,3'
    n_scenario <- 6
    res <- parse_abcranger_group(txt, n_scenario)
    expect_false(res$valid)
    
    txt <- '1,2,3;4,5,6,7'
    n_scenario <- 6
    res <- parse_abcranger_group(txt, n_scenario)
    expect_false(res$valid)
    
    txt <- 'test'
    n_scenario <- 6
    res <- parse_abcranger_group(txt, n_scenario)
    expect_false(res$valid)
})


test_that("abcranger_postprocess", {
    # parameter estimation
    proj_dir <- file.path(data4test_dir(), "PoolSeq_SNP_estim_param")
    graph_dir <- mk_proj_dir("testing")
    param <- "N1"
    prefix <- "estimparam_out"
    run_mode <- "param_estim"
    
    abcranger_postprocess(proj_dir, graph_dir, run_mode, prefix, param)
    
    expect_identical(
        list.files(graph_dir), 
        c(
            str_c(prefix, "_graph_density_plot.", 
                  get_option("image_ext")),
            str_c(prefix, "_graph_error_versus_ntrees.", 
                  get_option("image_ext")),
            str_c(prefix, "_graph_variable_importance.", 
                  get_option("image_ext"))
        )
    )
    
    # model choice
    proj_dir <- file.path(data4test_dir(), "PoolSeq_SNP_model_choice")
    graph_dir <- mk_proj_dir("testing")
    param <- NULL
    prefix <- "modelchoice_out"
    run_mode <- "model_choice"
    
    abcranger_postprocess(proj_dir, graph_dir, run_mode, prefix, param)
    
    expect_identical(
        list.files(graph_dir), 
        c(
            str_c(prefix, "_graph_error_versus_ntrees.", 
                  get_option("image_ext")),
            str_c(prefix, "_graph_lda.", 
                  get_option("image_ext")),
            str_c(prefix, "_graph_variable_importance.", 
                  get_option("image_ext"))
        )
    )
})

test_that("abcranger_subdir", {
    
    ## parameter estimation
    
    # prepare data for test
    data_dir <- file.path(data4test_dir(), "PoolSeq_SNP_estim_param")
    tmp_dir <- mk_proj_dir("testing")
    fs::dir_copy(path = data_dir, new_path = tmp_dir)
    proj_dir <- file.path(tmp_dir, "PoolSeq_SNP_estim_param")
    # list.files(proj_dir)
    sub_proj_name <- "sub_proj_test"
    prefix <- "estimparam_out"
    
    # file setup
    proj_file_list <- list.files(proj_dir)
    n_file <- length(proj_file_list)
    n_file_to_move <- sum(str_detect(proj_file_list, pattern = prefix))
    
    # manage sub proj dir
    abcranger_subdir(proj_dir, sub_proj_name, prefix)
    
    # check
    subproj_file_list <- list.files(file.path(proj_dir, sub_proj_name))
    expect_true(length(subproj_file_list) == n_file_to_move)
    
    proj_file_list <- list.files(proj_dir)
    expect_true(length(proj_file_list) == n_file - n_file_to_move + 1)
        
    ## model choice
    
    # prepare data for test
    
    # prepare data for test
    data_dir <- file.path(data4test_dir(), "PoolSeq_SNP_model_choice")
    tmp_dir <- mk_proj_dir("testing")
    fs::dir_copy(path = data_dir, new_path = tmp_dir)
    proj_dir <- file.path(tmp_dir, "PoolSeq_SNP_estim_param")
    # list.files(proj_dir)
    sub_proj_name <- "sub_proj_test"
    prefix <- "modelchoice_out"
    
    # file setup
    proj_file_list <- list.files(proj_dir)
    n_file <- length(proj_file_list)
    n_file_to_move <- sum(str_detect(proj_file_list, pattern = prefix))
    
    # manage sub proj dir
    abcranger_subdir(proj_dir, sub_proj_name, prefix)
    
    # check
    subproj_file_list <- list.files(file.path(proj_dir, sub_proj_name))
    expect_true(length(subproj_file_list) == n_file_to_move)
    
    proj_file_list <- list.files(proj_dir)
    expect_true(length(proj_file_list) == n_file - n_file_to_move + 1)
})