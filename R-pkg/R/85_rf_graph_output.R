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
    # init output
    g1 <- NULL
    
    # LDA latent space coordinates
    file_name <- file.path(proj_dir, str_c(prefix, ".lda"))
    lda_coord <- read.table(file_name, skip = 1)
    lda_dim <- ncol(lda_coord)
    
    # check for number of LDA components
    if(lda_dim < 2) {
        txt <- "No LDA axis,\n impossible to create\n a LDA plot"
        g1 <- ggplot() + 
            annotate("text", x = 4, y = 25, size=8, label = txt) + 
            theme_void()
        # output
        return(g1)
    }
    
    # colnames
    colnames(lda_coord) <- c(
        str_c("axis", 1:(ncol(lda_coord)-1)),
        "id"
    )
    # tagging
    lda_coord$tag <- c(
        "observations", 
        rep("simulations", length = nrow(lda_coord) - 1)
    )
    lda_coord$simulation <- c(
        "observed data",
        str_c("scenario", lda_coord$id[-1], sep = " ")
    )
    
    # single LDA axes (in case of two scenarii)
    if(lda_dim == 2) {
        g1 <- ggplot(lda_coord) +
            geom_density(
                data = subset(lda_coord, lda_coord$tag == "simulations"),
                aes_string(
                    x="axis1", col="simulation", fill="simulation"
                ), alpha = 0.2
            ) +
            suppressWarnings(geom_vline(
                data = subset(lda_coord, lda_coord$tag == "observations"),
                aes_string(
                    xintercept="axis1", col="simulation", fill="simulation"
                ), linetype="dashed"
            )) +
            xlab("axis 1") +
            ylab("Density") +
            theme_bw(base_size = 12) +
            theme(legend.title = element_blank())
    } else {
        # at least two LDA axis
        # LDA latent space first two axes: observation vs simulation coordinates
        g1 <- ggplot(lda_coord) +
            geom_point(
                data = subset(lda_coord, lda_coord$tag == "simulations"),
                aes_string(
                    x="axis1", y="axis2", col="simulation"
                ),
                alpha = 0.45, size = 1
            ) +
            geom_point(
                data = subset(lda_coord, lda_coord$tag == "observations"),
                aes_string(
                    x="axis1", y="axis2", col="simulation"
                ), 
                alpha = 1, size = 3
            ) +
            xlab("axis 1") +
            ylab("axis 2") +
            theme_bw(base_size = 12) +
            theme(legend.title = element_blank())
    }
    
    # output
    return(g1)
}