context("historic_model_display")

test_that("find_parent_event", {
    
    text <- "N1 N2\n0 sample 1\n0 sample 2\nt sample 1\nt2 merge 1 2"
    parsed_scenario <- parse_scenario(text)
    parent_event <- find_parent_event(
        event_type_list = parsed_scenario$event_type, 
        event_pop_list = parsed_scenario$event_pop, 
        event_id = 1, pop_id = 1
    )
    expect_equal(parent_event, 3)
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 merge 2 3",
        "t2 merge 1 2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    parent_event <- find_parent_event(
        event_type_list = parsed_scenario$event_type, 
        event_pop_list = parsed_scenario$event_pop, 
        event_id = 4, pop_id = 2
    )
    expect_equal(parent_event, 5)
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 split 3 1 2 r3",
        "t2 merge 1 2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    parent_event <- find_parent_event(
        event_type_list = parsed_scenario$event_type, 
        event_pop_list = parsed_scenario$event_pop, 
        event_id = 4, pop_id = 1
    )
    expect_equal(parent_event, 5)
})

test_that("scenario2tree", {
    
    text <- "N1 N2\n0 sample 1\n0 sample 2\nt sample 1\nt2 merge 1 2"
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    expect_identical(
        tree_df,
        structure(
            list(
                id = 1:4, 
                event = c("sample", "sample", "sample", "merge"), 
                pop = c(1L, 2L, 1L, 1L), 
                parent1_id = c(3L, 4L, 4L, NA), 
                parent2_id = c(NA, NA, NA, NA), 
                time = c("0", "0", "t", "t2"), 
                Ne = c("N1", "N2", "N1", "N1"), 
                param = c(NA, NA, NA, NA), 
                leaf = c(TRUE, TRUE, FALSE, FALSE)
            ), 
            row.names = c(NA, -4L), 
            class = "data.frame"
        )
    )
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 merge 2 3",
        "t2 merge 1 2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    expect_identical(
        tree_df,
        structure(
            list(
                id = 1:5, 
                event = c("sample", "sample", "sample", "merge", "merge"), 
                pop = c(1L, 2L, 3L, 2L, 1L), 
                parent1_id = c(5L, 4L, 4L, 5L, NA), 
                parent2_id = c(NA, NA, NA, NA, NA), 
                time = c("0", "0", "0", "t3", "t2"), 
                Ne = c("N1", "N2", "N3", "N2", "N1"), 
                param = c(NA, NA, NA, NA, NA), 
                leaf = c(TRUE, TRUE, TRUE, FALSE, FALSE)
            ), 
            row.names = c(NA, -5L), 
            class = "data.frame"
        )
    )
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 merge 2 3",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    expect_identical(
        tree_df,
        structure(
            list(
                id = 1:4, 
                event = c("sample", "sample", "sample", "merge"), 
                pop = c(1L, 2L, 3L, 2L), 
                parent1_id = c(NA, 4L, 4L, NA), 
                parent2_id = c(NA, NA, NA, NA), 
                time = c("0", "0", "0", "t3"), 
                Ne = c("N1", "N2", "N3", "N2"), 
                param = c(NA, NA, NA, NA), 
                leaf = c(TRUE, TRUE, TRUE, FALSE)
            ), 
            row.names = c(NA, -4L), 
            class = "data.frame"
        )
    )
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 merge 2 3",
        "t3 varNe 2 N2+N3",
        "t2 merge 1 2",
        "t2 varNe 1 N1+N2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    expect_identical(
        tree_df, 
        structure(
            list(
                id = 1:7, 
                event = c("sample", "sample", "sample", "merge", 
                          "varNe", "merge", "varNe"), 
                pop = c(1L, 2L, 3L, 2L, 2L, 1L, 1L), 
                parent1_id = c(6L, 4L, 4L, 5L, 6L, 7L, NA), 
                parent2_id = c(NA, NA, NA, NA, NA, NA, NA), 
                time = c("0", "0", "0", "t3", "t3", "t2", "t2"), 
                Ne = c("N1", "N2", "N3", "N2", "N2+N3", "N1", "N1+N2"), 
                param = c(NA, NA, NA, NA, "N2+N3", NA, "N1+N2"), 
                leaf = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
            ), 
            row.names = c(NA, -7L), 
            class = "data.frame"
        )
    )
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 split 3 1 2 r3",
        "t2 merge 1 2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    expect_identical(
        tree_df, 
        structure(
            list(
                id = c(1, 2, 3, 4, 4.1, 4.2, 5), 
                event = c("sample", "sample", "sample", "split", 
                          "split1", "split2", "merge"), 
                pop = c(1L, 2L, 3L, 3L, 1L, 2L, 1L), 
                parent1_id = c(4.1, 4.2, 4, 4.1, 5, 5, NA), 
                parent2_id = c(NA, NA, NA, 4.2, NA, NA, NA), 
                time = c("0", "0", "0", "t3", "t3", "t3", "t2"), 
                Ne = c("N1", "N2", "N3", "N3", "N1", "N2", "N1"), 
                param = c(NA, NA, NA, "r3", "r3", "r3", NA), 
                leaf = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
            ), 
            row.names = c(NA, -7L), 
            class = "data.frame"
        )
    )
})

test_that("reverse_tree", {
    
    text <- "N1 N2\n0 sample 1\n0 sample 2\nt sample 1\nt2 merge 1 2"
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    expect_true(rev_tree_out$valid)
    expect_length(rev_tree_out$msg_list, 0)
    expect_identical(
        rev_tree_out$rev_tree_df,
        structure(
            list(
                id = c(4L, 2L, 3L, 1L),
                event = c("merge", "sample", "sample", "sample"), 
                pop = c(1L, 2L, 1L, 1L), 
                time = c("t2", "0", "t", "0"), 
                Ne = c("N1", "N2", "N1", "N1"), 
                param = c(NA, NA, NA, NA), 
                leaf = c(FALSE, TRUE, FALSE, TRUE), 
                root = c(TRUE, FALSE, FALSE, FALSE), 
                child1_id = c(2L, NA, 1L, NA), 
                child2_id = c(3L, NA, NA, NA)
            ), 
            row.names = c(4L, 2L, 3L, 1L), 
            class = "data.frame"
        )
    )
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 merge 2 3",
        "t2 merge 1 2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    expect_true(rev_tree_out$valid)
    expect_length(rev_tree_out$msg_list, 0)
    expect_identical(
        rev_tree_out$rev_tree_df,
        structure(
            list(
                id = c(5L, 1L, 4L, 2L, 3L), 
                event = c("merge", "sample", "merge", "sample", "sample"), 
                pop = c(1L, 1L, 2L, 2L, 3L), 
                time = c("t2", "0", "t3", "0", "0"), 
                Ne = c("N1", "N1", "N2", "N2", "N3"), 
                param = c(NA, NA, NA, NA, NA), 
                leaf = c(FALSE, TRUE, FALSE, TRUE, TRUE), 
                root = c(TRUE, FALSE, FALSE, FALSE, FALSE), 
                child1_id = c(1L, NA, 2L, NA, NA), 
                child2_id = c(4L, NA, 3L, NA, NA)
            ), 
            row.names = c(5L, 1L, 4L, 2L, 3L), 
            class = "data.frame"
        )
    )
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 merge 2 3",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    expect_false(rev_tree_out$valid)
    expect_identical(
        rev_tree_out$msg_list, 
        list("Issue with scenario: populations 1, 2 do not coalesce")
    )
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 merge 2 3",
        "t3 varNe 2 N2+N3",
        "t2 merge 1 2",
        "t2 varNe 1 N1+N2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    expect_true(rev_tree_out$valid)
    expect_length(rev_tree_out$msg_list, 0)
    expect_identical(
        rev_tree_out$rev_tree_df,
        structure(
            list(
                id = c(7L, 6L, 1L, 5L, 4L, 2L, 3L), 
                event = c("varNe", "merge", "sample", "varNe", "merge", "sample", "sample"), 
                pop = c(1L, 1L, 1L, 2L, 2L, 2L, 3L), 
                time = c("t2", "t2", "0", "t3", "t3", "0", "0"), 
                Ne = c("N1+N2", "N1", "N1", "N2+N3", "N2", "N2", "N3"), 
                param = c("N1+N2", NA, NA, "N2+N3", NA, NA, NA), 
                leaf = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE), 
                root = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), 
                child1_id = c(6L, 1L, NA, 4L, 2L, NA, NA), 
                child2_id = c(NA, 5L, NA, NA, 3L, NA, NA)
            ), 
            row.names = c(7L, 6L, 1L, 5L, 4L, 2L, 3L), 
            class = "data.frame"
        )
    )
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 split 3 1 2 r3",
        "t2 merge 1 2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    expect_true(rev_tree_out$valid)
    expect_length(rev_tree_out$msg_list, 0)
    expect_identical(
        rev_tree_out$rev_tree_df,
        structure(
            list(
                id = c(5, 4.1, 4.2, 2, 1, 4, 3), 
                event = c("merge", "split1", "split2", "sample", 
                          "sample", "split", "sample"), 
                pop = c(1L, 1L, 2L, 2L, 1L, 3L, 3L), 
                time = c("t2", "t3", "t3", "0", "0", "t3", "0"), 
                Ne = c("N1", "N1", "N2", "N2", "N1", "N3", "N3"), 
                param = c(NA, "r3", "r3", NA, NA, "r3", NA), 
                leaf = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE), 
                root = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), 
                child1_id = c(4.1, 1, 2, NA, NA, 3, NA), 
                child2_id = c(4.2, 4, 4, NA, NA, NA, NA)
            ), 
            row.names = c(7L, 5L, 6L, 2L, 1L, 4L, 3L), 
            class = "data.frame"
        )
    )
})

test_that("tree_context", {
    
    grid_unit <- 2
    
    text <- "N1 N2\n0 sample 1\n0 sample 2\nt sample 1\nt2 merge 1 2"
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    rev_tree_df <- rev_tree_out$rev_tree_df
    info <- tree_context(parsed_scenario, rev_tree_df, grid_unit)
    expect_identical(
        info,
        list(
            timing_list = c("0", "t", "t2"), 
            time_coordinates = structure(
                list(
                    param = c("0", "t", "t2"), 
                    coord = c(0, 4, 8)
                ), 
                class = "data.frame", 
                row.names = c(NA, -3L)
            ), 
            event_nb = structure(
                list(
                    param = c("0", "t", "t2"), 
                    count = c(2L, 1L, 1L)
                ), 
                class = "data.frame", 
                row.names = c(NA, -3L)
            ), 
            existing_pop = list(`0` = 2L, t = 1L, t2 = 1L), 
            npop = 2L
        )
    )
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 merge 2 3",
        "t2 merge 1 2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    rev_tree_df <- rev_tree_out$rev_tree_df
    info <- tree_context(parsed_scenario, rev_tree_df, grid_unit)
    expect_identical(
        info,
        list(
            timing_list = c("0", "t3", "t2"), 
            time_coordinates = structure(
                list(
                    param = c("0", "t3", "t2"), 
                    coord = c(0, 6, 12)
                ), 
                class = "data.frame", 
                row.names = c(NA, -3L)
            ), 
            event_nb = structure(
                list(
                    param = c("0", "t3", "t2"), 
                    count = c(3L, 1L, 1L)
                ), 
                class = "data.frame", 
                row.names = c(NA, -3L)
            ), 
            existing_pop = list(`0` = 3L, t3 = 1L, t2 = 1L), 
            npop = 3L
        )
    )
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 split 3 1 2 r3",
        "t2 merge 1 2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    rev_tree_df <- rev_tree_out$rev_tree_df
    info <- tree_context(parsed_scenario, rev_tree_df, grid_unit)
    expect_identical(
        info,
        list(
            timing_list = c("0", "t3", "t2"), 
            time_coordinates = structure(
                list(
                    param = c("0", "t3", "t2"), 
                    coord = c(0, 6, 12)
                ), 
                class = "data.frame", 
                row.names = c(NA, -3L)
            ), 
            event_nb = structure(
                list(
                    param = c("0", "t3", "t2"), 
                    count = c(3L, 1L, 1L)
                ), 
                class = "data.frame", 
                row.names = c(NA, -3L)
            ), 
            existing_pop = list(`0` = 3L, t3 = 2L, t2 = 1L), 
            npop = 3L
        )
    )
})

test_that("adapt_grid_unit", {
    # FIXME
})

test_that("tree2node_coordinate", {
    
    grid_unit <- 2
    
    text <- "N1 N2\n0 sample 1\n0 sample 2\nt sample 1\nt2 merge 1 2"
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    rev_tree_df <- rev_tree_out$rev_tree_df
    rev_tree_coord_df <- tree2node_coordinate(tree_df, rev_tree_df, 
                                              parsed_scenario, grid_unit)
    
    g1 <- ggplot(rev_tree_coord_df, aes(x=x_coord, y=y_coord)) +
        geom_point(aes(col = Ne)) + 
        theme_void(base_size = 12) +
        ggtitle("(Warning! Time is not to scale)") +
        theme(legend.position = "left", 
              legend.justification = "top", 
              legend.direction = "vertical", 
              plot.margin = margin(10,10,10,10),
              plot.title = element_text(size = 12, hjust = 1))
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 merge 2 3",
        "t2 merge 1 2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    rev_tree_df <- rev_tree_out$rev_tree_df
    rev_tree_coord_df <- tree2node_coordinate(tree_df, rev_tree_df, 
                                              parsed_scenario, grid_unit)
    
    g1 <- ggplot(rev_tree_coord_df, aes(x=x_coord, y=y_coord)) +
        geom_point(aes(col = Ne)) + 
        theme_void(base_size = 12) +
        ggtitle("(Warning! Time is not to scale)") +
        theme(legend.position = "left", 
              legend.justification = "top", 
              legend.direction = "vertical", 
              plot.margin = margin(10,10,10,10),
              plot.title = element_text(size = 12, hjust = 1))
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 split 3 1 2 r3",
        "t2 merge 1 2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    rev_tree_df <- rev_tree_out$rev_tree_df
    rev_tree_coord_df <- tree2node_coordinate(tree_df, rev_tree_df, 
                                              parsed_scenario, grid_unit)
    
    g1 <- ggplot(rev_tree_coord_df, aes(x=x_coord, y=y_coord)) +
        geom_point(aes(col = Ne)) + 
        theme_void(base_size = 12) +
        ggtitle("(Warning! Time is not to scale)") +
        theme(legend.position = "left", 
              legend.justification = "top", 
              legend.direction = "vertical", 
              plot.margin = margin(10,10,10,10),
              plot.title = element_text(size = 12, hjust = 1))
})

test_that("extract_edge_coordinate", {
    
    grid_unit <- 2
    
    text <- "N1 N2\n0 sample 1\n0 sample 2\nt sample 1\nt2 merge 1 2"
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    rev_tree_df <- rev_tree_out$rev_tree_df
    rev_tree_coord_df <- tree2node_coordinate(tree_df, rev_tree_df, 
                                              parsed_scenario, grid_unit)
    
    current_node <- rev_tree_coord_df[2,]
    child_node <- rev_tree_coord_df[3,]
    ntime <- 3
    
    edge_coord <- extract_edge_coordinate(current_node, child_node, ntime, 
                                          grid_unit)
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 merge 2 3",
        "t2 merge 1 2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    rev_tree_df <- rev_tree_out$rev_tree_df
    rev_tree_coord_df <- tree2node_coordinate(tree_df, rev_tree_df, 
                                              parsed_scenario, grid_unit)
    
    current_node <- rev_tree_coord_df[2,]
    child_node <- rev_tree_coord_df[3,]
    ntime <- 3
    
    edge_coord <- extract_edge_coordinate(current_node, child_node, ntime, 
                                          grid_unit)
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 split 3 1 2 r3",
        "t2 merge 1 2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    rev_tree_df <- rev_tree_out$rev_tree_df
    rev_tree_coord_df <- tree2node_coordinate(tree_df, rev_tree_df, 
                                              parsed_scenario, grid_unit)
    rev_tree_coord_df <- tree2node_coordinate(tree_df, rev_tree_df, 
                                              parsed_scenario, grid_unit)
    
    current_node <- rev_tree_coord_df[2,]
    child_node <- rev_tree_coord_df[3,]
    ntime <- 3
    edge_coord <- extract_edge_coordinate(current_node, child_node, ntime, 
                                          grid_unit)
})

test_that("node2edge_coordinate", {
    
    grid_unit <- 2
    
    text <- "N1 N2\n0 sample 1\n0 sample 2\nt sample 1\nt2 merge 1 2"
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    rev_tree_df <- rev_tree_out$rev_tree_df
    rev_tree_coord_df <- tree2node_coordinate(tree_df, rev_tree_df, 
                                              parsed_scenario, grid_unit)
    
    edge_coordinates <- node2edge_coordinate(tree_df, rev_tree_df, 
                                             parsed_scenario, 
                                             rev_tree_coord_df, grid_unit)
    
    g1 <- ggplot(edge_coordinates) +
        geom_segment(aes(x=x_start, y=y_start, xend=x_end, yend=y_end, col = Ne)) + 
        geom_label(aes(x=x_end, y=y_end, label = text)) +
        theme_void(base_size = 12) +
        ggtitle("(Warning! Time is not to scale)") +
        theme(legend.position = "left", 
              legend.justification = "top", 
              legend.direction = "vertical", 
              plot.margin = margin(10,10,10,10),
              plot.title = element_text(size = 12, hjust = 1))
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 merge 2 3",
        "t2 merge 1 2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    rev_tree_df <- rev_tree_out$rev_tree_df
    rev_tree_coord_df <- tree2node_coordinate(tree_df, rev_tree_df, 
                                              parsed_scenario, grid_unit)
    
    edge_coordinates <- node2edge_coordinate(tree_df, rev_tree_df, 
                                             parsed_scenario, 
                                             rev_tree_coord_df, grid_unit)
    
    g1 <- ggplot(edge_coordinates) +
        geom_segment(aes(x=x_start, y=y_start, xend=x_end, yend=y_end, col = Ne)) + 
        geom_label(aes(x=x_end, y=y_end, label = text)) +
        theme_void(base_size = 12) +
        ggtitle("(Warning! Time is not to scale)") +
        theme(legend.position = "left", 
              legend.justification = "top", 
              legend.direction = "vertical", 
              plot.margin = margin(10,10,10,10),
              plot.title = element_text(size = 12, hjust = 1))
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 split 3 1 2 r3",
        "t2 merge 1 2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    rev_tree_df <- rev_tree_out$rev_tree_df
    rev_tree_coord_df <- tree2node_coordinate(tree_df, rev_tree_df, 
                                              parsed_scenario, grid_unit)
    
    edge_coordinates <- node2edge_coordinate(tree_df, rev_tree_df, 
                                             parsed_scenario, 
                                             rev_tree_coord_df, grid_unit)
    
    g1 <- ggplot(edge_coordinates) +
        geom_segment(aes(x=x_start, y=y_start, xend=x_end, yend=y_end, col = Ne)) + 
        geom_label(aes(x=x_end, y=y_end, label = text)) +
        theme_void(base_size = 12) +
        ggtitle("(Warning! Time is not to scale)") +
        theme(legend.position = "left", 
              legend.justification = "top", 
              legend.direction = "vertical", 
              plot.margin = margin(10,10,10,10),
              plot.title = element_text(size = 12, hjust = 1))
    
    text <- str_c(
        "N1 N2 N3 N4",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "0 sample 4",
        "t4 merge 3 4",
        "t2 merge 1 2",
        "t3 merge 1 3",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    rev_tree_df <- rev_tree_out$rev_tree_df
    rev_tree_coord_df <- tree2node_coordinate(tree_df, rev_tree_df, 
                                              parsed_scenario, grid_unit)
    
    edge_coordinates <- node2edge_coordinate(tree_df, rev_tree_df, 
                                             parsed_scenario, 
                                             rev_tree_coord_df, grid_unit)
    
    g1 <- ggplot(edge_coordinates) +
        geom_segment(aes(x=x_start, y=y_start, xend=x_end, yend=y_end, col = Ne)) + 
        geom_label(aes(x=x_end, y=y_end, label = text)) +
        theme_void(base_size = 12) +
        ggtitle("(Warning! Time is not to scale)") +
        theme(legend.position = "left", 
              legend.justification = "top", 
              legend.direction = "vertical", 
              plot.margin = margin(10,10,10,10),
              plot.title = element_text(size = 12, hjust = 1))
    
    text <- str_c(
        "N1 N2 N3 N4 N5 N6",
        "0 sample 1",
        "0 sample 2",
        "0 sample 3",
        "0 sample 4",
        "0 sample 5",
        "0 sample 6",
        "t6 merge 5 6",
        "t5 merge 4 5",
        "t4 merge 3 4",
        "t2 merge 1 2",
        "t3 merge 1 3",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    rev_tree_df <- rev_tree_out$rev_tree_df
    rev_tree_coord_df <- tree2node_coordinate(tree_df, rev_tree_df, 
                                              parsed_scenario, grid_unit)
    
    edge_coordinates <- node2edge_coordinate(tree_df, rev_tree_df, 
                                             parsed_scenario, 
                                             rev_tree_coord_df, grid_unit)
    
    g1 <- ggplot(edge_coordinates) +
        geom_segment(aes(x=x_start, y=y_start, xend=x_end, yend=y_end, col = Ne)) + 
        geom_label(aes(x=x_end, y=y_end, label = text)) +
        theme_void(base_size = 12) +
        ggtitle("(Warning! Time is not to scale)") +
        theme(legend.position = "left", 
              legend.justification = "top", 
              legend.direction = "vertical", 
              plot.margin = margin(10,10,10,10),
              plot.title = element_text(size = 12, hjust = 1))
    # FIXME
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 merge 2 3",
        "t3 varNe 2 N2+N3",
        "t2 merge 1 2",
        "t2 varNe 1 N1+N2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    rev_tree_df <- rev_tree_out$rev_tree_df
    rev_tree_coord_df <- tree2node_coordinate(tree_df, rev_tree_df, 
                                              parsed_scenario, grid_unit)
    
    edge_coordinates <- node2edge_coordinate(tree_df, rev_tree_df, 
                                             parsed_scenario, 
                                             rev_tree_coord_df, grid_unit)
    
    g1 <- ggplot(edge_coordinates) +
        geom_segment(aes(x=x_start, y=y_start, xend=x_end, yend=y_end, col = Ne)) + 
        geom_label(aes(x=x_end, y=y_end, label = text)) +
        theme_void(base_size = 12) +
        ggtitle("(Warning! Time is not to scale)") +
        theme(legend.position = "left", 
              legend.justification = "top", 
              legend.direction = "vertical", 
              plot.margin = margin(10,10,10,10),
              plot.title = element_text(size = 12, hjust = 1))
})

test_that("display_hist_model", {
    
    grid_unit <- 2
    
    text <- "N1 N2\n0 sample 1\n0 sample 2\nt sample 1\nt2 merge 1 2"
    parsed_scenario <- parse_scenario(text)
    data2plot <- prepare_hist_model_display(parsed_scenario, grid_unit)
    g1 <- display_hist_model(data2plot)
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 merge 2 3",
        "t2 merge 1 2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    data2plot <- prepare_hist_model_display(parsed_scenario, grid_unit)
    g1 <- display_hist_model(data2plot)
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 split 3 1 2 r3",
        "t2 merge 1 2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    data2plot <- prepare_hist_model_display(parsed_scenario, grid_unit)
    g1 <- display_hist_model(data2plot)
    
    text <- str_c(
        "N1 N2 N3 N4",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "0 sample 4",
        "t4 merge 3 4",
        "t2 merge 1 2",
        "t3 merge 1 3",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    data2plot <- prepare_hist_model_display(parsed_scenario, grid_unit)
    g1 <- display_hist_model(data2plot)
    
    text <- str_c(
        "N1 N2 N3 N4 N5 N6",
        "0 sample 1",
        "0 sample 2",
        "0 sample 3",
        "0 sample 4",
        "0 sample 5",
        "0 sample 6",
        "t6 merge 5 6",
        "t5 merge 4 5",
        "t4 merge 3 4",
        "t2 merge 1 2",
        "t3 merge 1 3",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    data2plot <- prepare_hist_model_display(parsed_scenario, grid_unit)
    g1 <- display_hist_model(data2plot)
    # FIXME
    
    text <- str_c(
        "N1 N2 N3",
        "0 sample 1", 
        "0 sample 2", 
        "0 sample 3",
        "t3 merge 2 3",
        "t3 varNe 2 N2+N3",
        "t2 merge 1 2",
        "t2 varNe 1 N1+N2",
        sep = "\n")
    parsed_scenario <- parse_scenario(text)
    data2plot <- prepare_hist_model_display(parsed_scenario, grid_unit)
    g1 <- display_hist_model(data2plot)
    
    # scenario with ghost pop
    text <- str_c(
        "N1 N2 N3 Nbc3",
        "0 sample 1",
        "50 sample 2",
        "0 sample 3",
        "t3-DB3 VarNe 3 NF3",
        "t3 merge 4 3",
        "t3lb merge 1 4",
        "t2 merge 1 2",
        sep = "\n"
    )
    parsed_scenario <- parse_scenario(text)
    data2plot <- prepare_hist_model_display(parsed_scenario, grid_unit)
    g1 <- display_hist_model(data2plot)
    
    # more complicated scenario with ghost pop
    text <- str_c(
        "N1 NKo NKa NJ12 N2 N3 N4 Na",
        "0 sample 1",
        "0 sample 2",
        "0 sample 3",
        "0 sample 4",
        "0 sample 5",
        "0 sample 6",
        "0 sample 7",
        "tJ12-DBJ12 varNe 4 NJ12B",
        "tJ12 VarNe 4 NgJ12",
        "tgJ12 merge 8 4",
        "tKa-DBKa varNe 3 NKaB",
        "tKa VarNe 3 NgKa",
        "tgKa merge 8 3",
        "tKo-DBKo varNe 2 NKoB",
        "tKo VarNe 2 NgKo",
        "tgKo merge 8 2",
        "t1 merge 8 1",
        "t2 merge 8 5",
        "t3 merge 8 6",
        "t4 merge 8 7",
        "ta VarNe 8 Naold",
        sep = "\n"
    )
    parsed_scenario <- parse_scenario(text)
    data2plot <- prepare_hist_model_display(parsed_scenario, grid_unit)
    g1 <- display_hist_model(data2plot)
})
