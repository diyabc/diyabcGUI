#' Parameter estimation graphical output
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom ggplot2 ggsave
param_estim_graph_ouptut <- function(proj_dir, graph_dir, param, 
                                     prefix = "estimparam_out") {
    # kernel density estimate
    g1 <- param_estim_density(proj_dir, param, prefix)
    # graph oob error vs n tree
    g2 <- oob_error_graph(proj_dir, prefix)
    # graph of top 50 variable importance
    g3 <- var_imp_graph(proj_dir, prefix)
    # save graph
    ggsave(
        filename = str_c(prefix, "_graph_density_plot.", 
                         get_option("image_ext")),
        plot = g1, 
        path = graph_dir,
        units = "cm", width = 12, height = 10
    )
    ggsave(
        filename = str_c(prefix, "_graph_error_versus_ntrees.", 
                         get_option("image_ext")),
        plot = g2, 
        path = graph_dir,
        units = "cm", width = 12, height = 10
    )
    ggsave(
        filename = str_c(prefix, "_graph_variable_importance.", 
                         get_option("image_ext")),
        plot = g3, 
        path = graph_dir,
        units = "cm", width = 12, height = 22
    )
}

#' Parameter posterior estimated density
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom ggplot2 geom_density
param_estim_density <- function(proj_dir, param, prefix = "estimparam_out") {
    # prediction and weights
    file_name <- file.path(proj_dir, str_c(prefix, ".predweights"))
    predweights <- read.csv(file_name, header = TRUE, sep = ",")
    # kernel density estimate
    g1 <- ggplot(predweights) +
        geom_density(aes(x=value, weight=weight)) +
        theme_bw(base_size = 12) +
        xlab("value") +
        ylab("posterior density") +
        ggtitle(str_c("parameter ", param))
    # output
    return(g1)
}


#' OOB error graphical output
#' @keywords internal
#' @author Ghislain Durif
oob_error_graph <- function(proj_dir, prefix = "estimparam_out") {
    # oob error
    file_name <- file.path(proj_dir, str_c(prefix, ".ooberror"))
    ooberror <- read.csv(file_name, header = FALSE)
    colnames(ooberror) <- "error"
    ooberror$ntree <- 1:nrow(ooberror)
    # graph oob error vs n tree
    g1 <- ggplot(ooberror) +
        geom_point(aes(x=ntree, y=error), alpha = 1, size = 0.2) +
        theme_bw(base_size = 12) +
        xlab("number of tree") +
        ylab("OOB error")
    # output
    return(g1)
}

#' Variable importance graphical output
#' @keywords internal
#' @author Ghislain Durif
var_imp_graph <- function(proj_dir, prefix = "estimparam_out") {
    # variable importance
    file_name <- file.path(proj_dir, str_c(prefix, ".importance"))
    var_importance <- read.csv(file_name, header = FALSE, sep = ":")
    colnames(var_importance) <- c("statistics",  "importance")
    # format data
    data2plot <- head(
        var_importance[order(var_importance$importance, decreasing = TRUE),], 
        50
    )
    data2plot$statistics <- factor(data2plot$statistics, 
                                   levels = rev(data2plot$statistics))
    # graph of top 50 variable importance
    g1 <- ggplot(data2plot) +
        geom_point(aes(x=importance, y=statistics)) +
        theme_bw(base_size = 12)
    # output
    return(g1)
}


#' Model choice graphical output
#' @keywords internal
#' @author Ghislain Durif
model_choice_graph_ouptut <- function(proj_dir, graph_dir, 
                                      prefix = "modelchoice_out") {
    # lda output
    g1 <- lda_coordinate_graph(proj_dir, prefix)
    # graph oob error vs n tree
    g2 <- oob_error_graph(proj_dir, prefix)
    # graph of top 50 variable importance
    g3 <- var_imp_graph(proj_dir, prefix)
    # save graph
    ggsave(
        filename = str_c(prefix, "_graph_lda.",
                         get_option("image_ext")),
        plot = g1,
        path = graph_dir,
        units = "cm", width = 14, height = 10
    )
    ggsave(
        filename = str_c(prefix, "_graph_error_versus_ntrees.", 
                         get_option("image_ext")),
        plot = g2, 
        path = graph_dir,
        units = "cm", width = 12, height = 10
    )
    ggsave(
        filename = str_c(prefix, "_graph_variable_importance.", 
                         get_option("image_ext")),
        plot = g3, 
        path = graph_dir,
        units = "cm", width = 12, height = 22
    )
}


#' Model choice LDA latent coordinates graphical output
#' @keywords internal
#' @author Ghislain Durif
lda_coordinate_graph <- function(proj_dir, prefix = "modelchoice_out") {
    # LDA latent space coordinates
    file_name <- file.path(proj_dir, str_c(prefix, ".lda"))
    lda_out <- read.table(file_name, skip = 1)
    # tagging
    lda_out$tag <- c("observations", 
                     rep("simulations", length = nrow(lda_out) - 1))
    # LDA latent space first two axes: observation vs simulation coordinates
    g1 <- ggplot(lda_out) +
        geom_point(
            data = subset(lda_out, lda_out$tag == "simulations"),
            aes(x=V1, y=V2, col=tag), alpha = 0.1
        ) +
        geom_point(
            data = subset(lda_out, lda_out$tag == "observations"),
            aes(x=V1, y=V2, col=tag), alpha = 1, size = 2
        ) +
        xlab("axis 1") +
        ylab("axis 2") +
        theme_bw(base_size = 12)
    # output
    return(g1)
}