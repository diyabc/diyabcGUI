#' Scneario check graphical output
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom ggplot2 ggsave
scenario_check_graph_ouptut <- function(proj_dir, graph_dir, 
                                        prefix = "pcaloc1_") {
    # PCA latent space coordinates
    g1 <- pca_coordinate_graph(proj_dir, prefix)
    # save graph
    ggsave(
        filename = str_c("scenario_check_pca_output_graph.", 
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
pca_coordinate_graph <- function(proj_dir, prefix = "pcaloc1_") {
    # PCA latent space coordinates
    file_name <- file.path(proj_dir, str_c(prefix, "ACP.txt"))
    pca_coord <- read.csv(file_name, header = FALSE, sep = " ")
    # preprocessing (remove first line and first two columns)
    pca_coord <- pca_coord[-1,]
    pca_coord <- pca_coord[,-c(1,2)]
    # tagging
    pca_coord$tag <- c("observations", 
                       rep("simulations", length = nrow(pca_coord) - 1))
    # LDA latent space first two axes: observation vs simulation coordinates
    g1 <- ggplot(pca_coord) +
        geom_point(
            data = subset(pca_coord, pca_coord$tag == "simulations"),
            aes(x=V3, y=V4, col=tag), alpha = 0.1
        ) +
        geom_point(
            data = subset(pca_coord, pca_coord$tag == "observations"),
            aes(x=V3, y=V4, col=tag), alpha = 1, size = 2
        ) +
        xlab("axis 1") +
        ylab("axis 2") +
        theme_bw(base_size = 12)
    # output
    return(g1)
}