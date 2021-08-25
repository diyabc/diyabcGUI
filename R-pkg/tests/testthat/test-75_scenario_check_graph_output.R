test_that("scenario_check_graph_output", {
    
    ## Model choice
    
    # PoolSeq
    proj_dir <- file.path(data4test_dir(), "PoolSeq_SNP_model_choice")
    graph_dir <- mk_proj_dir("testing")
    prefix <- "pcaloc1_"
    scenario_check_graph_output(proj_dir, graph_dir, prefix)
    
    expect_identical(
        list.files(graph_dir), 
        c(
            str_c("scenario_check_pca_output_graph_", 1:3, ".", 
                  get_option("image_ext"))
        )
    )
    
    # IndSeq
    proj_dir <- file.path(data4test_dir(), "IndSeq_SNP_model_choice")
    graph_dir <- mk_proj_dir("testing")
    prefix <- "pcaloc1_"
    scenario_check_graph_output(proj_dir, graph_dir, prefix)
    
    expect_identical(
        list.files(graph_dir), 
        c(
            str_c("scenario_check_pca_output_graph_", 1:3, ".", 
                  get_option("image_ext"))
        )
    )
    
    ## Parameter estimation
    
    # PoolSeq
    proj_dir <- file.path(data4test_dir(), "PoolSeq_SNP_estim_param")
    graph_dir <- mk_proj_dir("testing")
    prefix <- "pcaloc1_"
    scenario_check_graph_output(proj_dir, graph_dir, prefix)
    
    expect_identical(
        list.files(graph_dir), 
        c(
            str_c("scenario_check_pca_output_graph_", 1:3, ".", 
                  get_option("image_ext"))
        )
    )
    
    # IndSeq
    proj_dir <- file.path(data4test_dir(), "IndSeq_SNP_estim_param")
    graph_dir <- mk_proj_dir("testing")
    prefix <- "pcaloc1_"
    scenario_check_graph_output(proj_dir, graph_dir, prefix)
    
    expect_identical(
        list.files(graph_dir), 
        c(
            str_c("scenario_check_pca_output_graph_", 1:3, ".", 
                  get_option("image_ext"))
        )
    )
})


test_that("pca_coordinate_graph", {
    
    ## Model choice
    
    # PoolSeq
    proj_dir <- file.path(data4test_dir(), "PoolSeq_SNP_model_choice")
    prefix <- "pcaloc1_"
    comp <- 1:2
    g1 <- pca_coordinate_graph(proj_dir, prefix, comp)
    expect_true("ggplot" %in% class(g1))
    
    # IndSeq
    proj_dir <- file.path(data4test_dir(), "IndSeq_SNP_model_choice")
    prefix <- "pcaloc1_"
    comp <- 1:2
    g1 <- pca_coordinate_graph(proj_dir, prefix, comp)
    expect_true("ggplot" %in% class(g1))
    
    ## Parameter estimation
    
    # PoolSeq
    proj_dir <- file.path(data4test_dir(), "PoolSeq_SNP_estim_param")
    prefix <- "pcaloc1_"
    comp <- 1:2
    g1 <- pca_coordinate_graph(proj_dir, prefix, comp)
    expect_true("ggplot" %in% class(g1))
    
    # IndSeq
    proj_dir <- file.path(data4test_dir(), "IndSeq_SNP_estim_param")
    prefix <- "pcaloc1_"
    comp <- 1:2
    g1 <- pca_coordinate_graph(proj_dir, prefix, comp)
    expect_true("ggplot" %in% class(g1))
})