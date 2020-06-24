context("random_forest_tools")

test_that("abcranger_run", {
    
    # proj dir
    proj_dir = mk_proj_dir()
    on.exit(tryCatch(fs::dir_detete(proj_dir)))
    logging("tmp dir:", proj_dir)
    
    # copy header and data file from example
    lapply(
        c("headerRF.txt", "statobsRF.txt", "reftableRF.bin", 
          "indseq_SNP_sim_dataset_4POP_001.snp"),
        function(filename) {
            fs::file_copy(
                path = file.path(example_dir(), "diyabc_rf_pipeline", 
                                 "IndSeq_SNP_estim_param", filename),
                new_path = file.path(proj_dir, filename)
            )
        }
    )
    
    mode <- "param_estim"
    n_ref <- 0
    min_node_size <- 0
    n_tree <- 500
    noise_columns <- 5
    no_linear <- FALSE
    pls_max_var <- 0.9
    chosen_scenario <- 1
    noob <- 50
    parameter <- "N1"
    groups <- NULL
    
    # try run
    run_proc <- abcranger_run(proj_dir, mode, n_ref, 
                              min_node_size, n_tree, noise_columns, no_linear, 
                              pls_max_var, chosen_scenario, noob, parameter, 
                              groups)
    
    run_proc$is_alive()
    # run_proc$kill()
    
    ## check project directory
    list.files(proj_dir)
    
    ## clean up
    cleanup_abcranger_run(proj_dir)
    
})

test_that("parse_abcranger_group", {
    
    txt <- '1,2,3;4,5,6'
    n_scenario <- 6
    expect_true(parse_abcranger_group(txt, n_scenario)$valid)
    
    txt <- '1,2,3;4,5'
    n_scenario <- 6
    expect_false(parse_abcranger_group(txt, n_scenario)$valid)
    
    txt <- '1,2,3;4,5,6,7'
    n_scenario <- 6
    expect_false(parse_abcranger_group(txt, n_scenario)$valid)
    
    txt <- 'test'
    n_scenario <- 6
    expect_false(parse_abcranger_group(txt, n_scenario)$valid)
})