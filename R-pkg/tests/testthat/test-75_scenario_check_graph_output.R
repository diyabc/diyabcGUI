test_that("scenario_check_graph_ouptut", {
    # PoolSeq
    proj_dir <- file.path(data4test_dir(), "PoolSeq_SNP_estim_param")
    graph_dir <- mk_proj_dir("testing")
    prefix <- "pcaloc1_"
    scenario_check_graph_ouptut(proj_dir, graph_dir, prefix)
    
    expect_identical(
        list.files(graph_dir), 
        c(
            str_c("scenario_check_pca_output_graph.", 
                  get_option("image_ext"))
        )
    )
    
    # IndSeq
    proj_dir <- file.path(data4test_dir(), "IndSeq_SNP_estim_param")
    graph_dir <- mk_proj_dir("testing")
    prefix <- "pcaloc1_"
    scenario_check_graph_ouptut(proj_dir, graph_dir, prefix)
    
    expect_identical(
        list.files(graph_dir), 
        c(
            str_c("scenario_check_pca_output_graph.", 
                  get_option("image_ext"))
        )
    )
})


test_that("pca_coordinate_graph", {
    # PoolSeq
    proj_dir <- file.path(data4test_dir(), "PoolSeq_SNP_estim_param")
    prefix <- "pcaloc1_"
    g1 <- pca_coordinate_graph(proj_dir, prefix)
    expect_true("ggplot" %in% class(g1))
    
    # IndSeq
    proj_dir <- file.path(data4test_dir(), "IndSeq_SNP_estim_param")
    prefix <- "pcaloc1_"
    g1 <- pca_coordinate_graph(proj_dir, prefix)
    expect_true("ggplot" %in% class(g1))
})