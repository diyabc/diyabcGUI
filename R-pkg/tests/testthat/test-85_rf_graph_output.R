context("RF graph output")

test_that("param_estim_graph_ouptut", {
    
    # input argument
    test_proj <- "PoolSeq_SNP_estim_param"
    proj_dir <- file.path(data4test_dir(), test_proj)
    graph_dir <- mk_proj_dir("test_abcranger_postprocess")
    param <- "N1"
    prefix <- "estimparam_out"
    
    param_estim_graph_ouptut(proj_dir, graph_dir, param, prefix)
    
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
})

test_that("param_estim_density", {
    # input argument
    proj_dir <- file.path(data4test_dir(), "PoolSeq_SNP_estim_param")
    param <- "N1"
    prefix <- "estimparam_out"
    # run and test
    g1 <- param_estim_density(proj_dir, param, prefix)
    expect_true("ggplot" %in% class(g1))
})

test_that("oob_error_graph", {
    # parameter estimation test
    proj_dir <- file.path(data4test_dir(), "PoolSeq_SNP_estim_param")
    prefix <- "estimparam_out"
    g1 <- oob_error_graph(proj_dir, prefix)
    expect_true("ggplot" %in% class(g1))
    # model choice test
    proj_dir <- file.path(data4test_dir(), "PoolSeq_SNP_model_choice")
    prefix <- "modelchoice_out"
    g1 <- oob_error_graph(proj_dir, prefix)
    expect_true("ggplot" %in% class(g1))
})

test_that("var_imp_graph", {
    # parameter estimation test
    proj_dir <- file.path(data4test_dir(), "PoolSeq_SNP_estim_param")
    prefix <- "estimparam_out"
    g1 <- var_imp_graph(proj_dir, prefix)
    expect_true("ggplot" %in% class(g1))
    # model choice test
    proj_dir <- file.path(data4test_dir(), "PoolSeq_SNP_model_choice")
    prefix <- "modelchoice_out"
    g1 <- var_imp_graph(proj_dir, prefix)
    expect_true("ggplot" %in% class(g1))
})


test_that("model_choice_graph_ouptut", {
    
    # input argument
    test_proj <- "PoolSeq_SNP_model_choice"
    proj_dir <- file.path(data4test_dir(), test_proj)
    graph_dir <- mk_proj_dir("test_abcranger_postprocess")
    prefix <- "modelchoice_out"
    
    model_choice_graph_ouptut(proj_dir, graph_dir, prefix)
    
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

test_that("lda_coordinate_graph", {
    proj_dir <- file.path(data4test_dir(), "PoolSeq_SNP_model_choice")
    prefix <- "modelchoice_out"
    g1 <- lda_coordinate_graph(proj_dir, prefix)
    expect_true("ggplot" %in% class(g1))
})
