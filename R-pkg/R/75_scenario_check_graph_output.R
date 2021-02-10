#' Scneario check graphical output
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom ggplot2 ggsave
scenario_check_graph_ouptut <- function(proj_dir, graph_dir, 
                                        prefix = "pcaloc1_") {
    # PCA latent space coordinates
    g1 <- pca_coordinate_graph(proj_dir, prefix, comp = 1:2)
    # save graph
    ggsave(
        filename = str_c("scenario_check_pca_output_graph_1.", 
                         get_option("image_ext")),
        plot = g1, 
        path = graph_dir,
        units = "cm", width = 14, height = 10
    )
    # PCA latent space coordinates
    g1 <- pca_coordinate_graph(proj_dir, prefix, comp = c(1,3))
    # save graph
    ggsave(
        filename = str_c("scenario_check_pca_output_graph_2.", 
                         get_option("image_ext")),
        plot = g1, 
        path = graph_dir,
        units = "cm", width = 14, height = 10
    )
    # PCA latent space coordinates
    g1 <- pca_coordinate_graph(proj_dir, prefix, comp = 2:3)
    # save graph
    ggsave(
        filename = str_c("scenario_check_pca_output_graph_3.", 
                         get_option("image_ext")),
        plot = g1, 
        path = graph_dir,
        units = "cm", width = 14, height = 10
    )
}

#' PCA embedding representation
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom ggplot2 geom_density
pca_coordinate_graph <- function(proj_dir, prefix = "pcaloc1_", 
                                 comp = 1:2) {
    # PCA latent space coordinates
    file_name <- file.path(proj_dir, str_c(prefix, "ACP.txt"))
    # header
    header <- unlist(read.csv(file_name, header = FALSE, sep = " ", nrows = 1))
    # explained variance
    exp_var <- header[-(1:2)]
    # file content
    pca_coord <- read.csv(file_name, header = FALSE, sep = " ", skip = 1)
    # preprocessing
    colnames(pca_coord) <- c("id", str_c("comp", 1:(ncol(pca_coord)-1)))
    # tagging
    pca_coord$id <- as.factor(pca_coord$id)
    pca_coord$tag <- c("observations", 
                       rep("simulations", length = nrow(pca_coord) - 1))
    pca_coord$simulation <- c("observed data",
                              str_c("scenario", pca_coord$id[-1], sep = " "))
    # PCA latent space first two axes: observation vs simulation coordinates
    g1 <- ggplot(pca_coord) +
        geom_point(
            data = subset(pca_coord, pca_coord$tag == "simulations"),
            aes_string(x=str_c("comp", comp[1]), 
                       y=str_c("comp", comp[2]), 
                       col="simulation"), 
            alpha = 0.1, size = 1
        ) +
        geom_point(
            data = subset(pca_coord, pca_coord$tag == "observations"),
            aes_string(x=str_c("comp", comp[1]), 
                       y=str_c("comp", comp[2]), 
                       col="simulation"), 
            alpha = 1, size = 2
        ) +
        xlab(str_c("axis", comp[1], 
                   str_c("(", exp_var[comp[1]] * 100, "%)"), 
                   sep = " ")) +
        ylab(str_c("axis", comp[2], 
                   str_c("(", exp_var[comp[2]] * 100, "%)"), 
                   sep = " ")) +
        theme_bw(base_size = 12)
    # output
    return(g1)
}