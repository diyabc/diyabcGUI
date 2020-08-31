#' Find the parent event to a given event in the parsed scenario
#' @keywords internal
#' @author Ghislain Durif
find_parent_event <- function(event_type_list, event_pop_list, 
                              event_id, pop_id) {
    if(event_id < length(event_type_list)) {
        candidate_event_ind <- (event_id+1):length(event_type_list)
        find_pop <- Reduce(
            "rbind",
            lapply(
                candidate_event_ind, 
                function(tmp_event_id) {
                    event_type <- event_type_list[tmp_event_id]
                    event_pop <- event_pop_list[[tmp_event_id]]
                    
                    tmp_check <- which(pop_id == event_pop)
                    if(length(tmp_check) == 0) tmp_check <- NA
                    
                    return(
                        data.frame(
                            pop_pos = tmp_check,
                            event_id = tmp_event_id,
                            event = event_type
                        )
                    )
                }
            )
        ) %>% drop_na()
        
        if(nrow(find_pop) == 0) {
            return(NA)
        } else {
            find_pop <- head(find_pop, 1)
            
            if(find_pop$event[1] != "split") {
                return(find_pop$event_id)
            } else {
                # split
                if(find_pop$pop_pos == 1) {
                    return(find_pop$event_id)
                } else if(find_pop$pop_pos == 2) {
                    return(find_pop$event_id + 0.1)
                } else if(find_pop$pop_pos == 3) {
                    return(find_pop$event_id + 0.2)
                } else {
                    return(NA)
                }
            }
        }
    } else {
        return(NA)
    }
}

#' Reformat a scenario to a tree representation
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Create a tree representation of events, starting from the output of 
#' the `parse_scenario` function, from leaves to root.
scenario2tree <- function(parsed_scenario) {
    
    Ne_list <- as.list(parsed_scenario$Ne_list_0)
    env <- environment()
    
    out <- Reduce(
        "rbind",
        lapply(
            1:parsed_scenario$nevent,
            function(event_id) {
                event_type <- parsed_scenario$event_type[event_id]
                event_time <- parsed_scenario$event_time[[event_id]]
                event_pop <- parsed_scenario$event_pop[[event_id]]
                event_param <- parsed_scenario$event_param[[event_id]]
                
                current_pop <- NA
                parent_pop <- NA
                parent_event <- NA
                
                tmp_out <- NULL
                
                if(event_type != "split") {
                    if(event_type %in% c("sample", "varNe")) {
                        current_pop <- event_pop
                        parent_pop <- event_pop
                    } else if(event_type == "merge") {
                        current_pop <- event_pop[1]
                        parent_pop <- event_pop[1]
                    }
                    
                    if(event_type == "varNe") {
                        Ne_list[[current_pop]] <- event_param
                        assign("Ne_list", value = Ne_list, envir = env)
                    }
                    
                    parent_event <- find_parent_event(
                        event_type_list = parsed_scenario$event_type, 
                        event_pop_list = parsed_scenario$event_pop, 
                        event_id = event_id, pop_id = parent_pop
                    )
                    
                    tmp_out <- data.frame(
                        id = event_id, event = event_type, pop = current_pop, 
                        parent1_id = parent_event, parent2_id = NA,
                        time = event_time, Ne = Ne_list[[current_pop]], 
                        param = ifelse(is.null(event_param), NA, event_param),
                        leaf = NA,
                        stringsAsFactors = FALSE
                    )
                } else {
                    ## split
                    current_pop <- event_pop[1]
                    parent_pop <- event_pop[2:3]
                    
                    parent_event <- rep(NA, 2)
                    if(event_id < parsed_scenario$nevent) {
                        parent_event <- unlist(
                            lapply(
                                parent_pop, 
                                function(pop_id) {
                                    return(
                                        find_parent_event(
                                            event_type_list = parsed_scenario$event_type, 
                                            event_pop_list = parsed_scenario$event_pop, 
                                            event_id = event_id, pop_id = pop_id
                                        )
                                    )
                                }
                            )
                        )
                    }
                    
                    tmp_out <- rbind(
                        data.frame(
                            id = event_id, event = event_type, 
                            pop = current_pop, 
                            parent1_id = event_id + 0.1, 
                            parent2_id = event_id + 0.2, 
                            time = event_time, Ne = Ne_list[[current_pop]], 
                            param = ifelse(is.null(event_param), 
                                           NA, event_param),
                            leaf = NA,
                            stringsAsFactors = FALSE
                        ),
                        data.frame(
                            id = event_id + 0.1, event = "split1", 
                            pop = parent_pop[1], 
                            parent1_id = parent_event[1], 
                            parent2_id = NA,
                            time = event_time, Ne = Ne_list[[parent_pop[1]]], 
                            param = ifelse(is.null(event_param), 
                                           NA, event_param),
                            leaf = NA,
                            stringsAsFactors = FALSE
                        ),
                        data.frame(
                            id = event_id + 0.2, event = "split2", 
                            pop = parent_pop[2], 
                            parent1_id = parent_event[2], 
                            parent2_id = NA,
                            time = event_time, Ne = Ne_list[[parent_pop[2]]], 
                            param = ifelse(is.null(event_param), 
                                           NA, event_param),
                            leaf = NA,
                            stringsAsFactors = FALSE
                        )
                    )
                }
                return(tmp_out)
            }
        )
    )
    
    # find leaf
    out$leaf <- unlist(
        lapply(
            split(out, seq(nrow(out))), 
            function(item) {
                if(item$id > 1) {
                    return(! item$pop %in% out$pop[1:(item$id-1)])
                } else {
                    return(TRUE)
                }
            }
        )
    )
    
    return(out)
}

#' Reverse scenario tree representation
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Reverse a scenario tree representation of events, starting from the output 
#' of the `scenario2tree` function, from root to leaves.
reverse_tree <- function(tree_df) {
    # tree_df: id  event pop parent1_id parent2_id time Ne param leaf
    
    msg_list <- list()
    valid <- TRUE
    
    # get root
    root_ind <- which(is.na(tree_df$parent1_id) & is.na(tree_df$parent2_id))
    if(length(root_ind) != 1) {
        valid <- FALSE
        msg_list <- append(
            msg_list,
            str_c(
                "Issue with scenario:",
                "populations", str_c(tree_df$pop[root_ind], collapse = ", "),
                "do not coalesce.",
                sep = " "
            )
        )
    }
    
    # check if binary
    # FIXME
    
    # reorder tree from top to bottom
    tree_df <- tree_df[c(root_ind, 
                         order(tree_df[-root_ind,]$parent1_id, 
                               decreasing = TRUE)
    ),]
    tree_df$root <- is.na(tree_df$parent1_id) & is.na(tree_df$parent2_id)
    
    # reverse the tree
    rev_tree_df <- Reduce(
        "rbind",
        lapply(
            split(tree_df, seq(nrow(tree_df))), 
            function(df_row) {
                tmp_df <- df_row[,!str_detect(colnames(df_row), "parent")]
                children <- tree_df$id[
                    c(which(tmp_df$id == tree_df$parent1_id),
                      which(tmp_df$id == tree_df$parent2_id))
                    ]
                children <- as.data.frame(
                    matrix(
                        c(children, rep(NA, 2 - length(children))), 
                        nrow = 1
                    )
                )
                colnames(children) <- c("child1_id", "child2_id")
                return(cbind(tmp_df, children))
            }
        )
    )
    
    # output
    return(lst(rev_tree_df, msg_list, valid))
}

#' Extract informations regarding tree context
#' @keywords internal
#' @author Ghislain Durif
#' @description 
#' Returns following informations: timing_list, time_coordinates, event_nb, npop
tree_context <- function(parsed_scenario, rev_tree_d, grid_unit = 2) {
    # timing list
    timing_list <- unique(unlist(parsed_scenario$event_time))
    
    # number of events by timing
    event_nb <- as.data.frame(
        table(unlist(parsed_scenario$event_time)),
        stringsAsFactors = FALSE
    ) %>% arrange(timing_list)
    colnames(event_nb) <- c("param", "count")
    
    # check number of existing pop per timing
    existing_pop <- lapply(
        timing_list, 
        function(item) {
            tmp_node <- rev_tree_df[
                (rev_tree_df$time == item) &
                    (rev_tree_df$event != "split"),]
            return(length(unique(tmp_node$pop)))
        }
    )
    names(existing_pop) <- timing_list
    
    # number of populations
    npop <- max(unlist(existing_pop))
    
    # y-coordinates (for each time)
    time_coordinates <- data.frame(
        param = timing_list,
        coord = seq(from = 0, by = grid_unit * npop, 
                    length.out = length(timing_list))
    )
    
    # output
    return(lst(
        timing_list,
        time_coordinates,
        event_nb,
        existing_pop,
        npop
    ))
}

#' Adapt grid unit to avoid node superpositon
#' @keywords internal
#' @author Ghislain Durif
adapt_grid_unit <- function(grid_unit, npop) {
    # FIXME
    if(npop < 1) {
        return(grid_unit)
    } else {
        return(grid_unit / npop)
    }
}

#' Assign coordinates to tree nodes
#' @keywords internal
#' @author Ghislain Durif
tree2node_coordinate <- function(tree_df, rev_tree_df, parsed_scenario, 
                                 grid_unit = 2) {
    
    # tree context
    info <- tree_context(parsed_scenario, rev_tree_df, grid_unit)
    timing_list <- info$timing_list
    time_coordinates <- info$time_coordinates
    event_nb <- info$event_nb
    existing_pop <- info$existing_pop
    npop <- info$npop
    
    # for each tree node, assign a coordinate
    rev_tree_coord_df <- NULL
    for(node_id in rev_tree_df$id) {
        x_coord <- NA
        y_coord <- NA
        orient <- NA # left (0) of right (1)
        grid_unit_fact <- NA # grid unit refactoring
        # current node (from leaf to root)
        current_node <- tree_df[tree_df$id == node_id,]
        # current node (from root to leaf)
        current_node_rev <- rev_tree_df[rev_tree_df$id == node_id,]
        
        if(current_node_rev$root) {
            x_coord <- 0
            grid_unit_fact <- grid_unit
        } else {
            # not split
            if(current_node$event != "split") {
                # parent
                parent_node <- rev_tree_coord_df[
                    rev_tree_coord_df$id == current_node$parent1_id,
                    ]
                
                # parent coord
                parent_x_coord <- parent_node$x_coord
                
                # grid unit factor
                grid_unit_fact <- parent_node$grid_unit_fact
                
                # different timing
                if(parent_node$time != current_node$time) {
                    
                    if(parent_node$event == "merge") {
                        fact <- (node_id == parent_node$child2_id) - 
                            (node_id == parent_node$child1_id)
                        
                        if(fact > 0) {
                            orient <- 1
                        } else if(fact < 0) {
                            orient <- 0
                        }
                        
                        parent_orient <- parent_node$orient
                        if(!is.na(parent_orient)) {
                            if(parent_orient != orient) {
                                grid_unit_fact <- grid_unit / 
                                    sqrt(existing_pop[[ current_node$time ]])
                            }
                        }
                        
                        x_coord <- parent_x_coord + fact * grid_unit_fact * 
                            abs(which(timing_list == current_node$time) - 
                                    which(timing_list == parent_node$time))
                        
                    } else if(parent_node$event == "split") {
                        x_coord <- parent_x_coord
                    } else {
                        # generic case of sample or varNe
                        orient <- parent_node$orient
                        if(!is.na(orient)) {
                            if(orient) {
                                x_coord <- parent_x_coord + grid_unit_fact * 
                                    abs(which(timing_list == current_node$time) - 
                                            which(timing_list == parent_node$time))
                            } else {
                                x_coord <- parent_x_coord - grid_unit_fact * 
                                    abs(which(timing_list == current_node$time) - 
                                            which(timing_list == parent_node$time))
                            }
                        } else {
                            x_coord <- parent_x_coord
                        }
                    }
                    
                } else {
                    # same timing
                    x_coord <- parent_x_coord
                }
            } else {
                # split (node between the two parents)
                parent1_node <- rev_tree_coord_df[
                    rev_tree_coord_df$id == current_node$parent1_id,
                    ]
                parent2_node <- rev_tree_coord_df[
                    rev_tree_coord_df$id == current_node$parent2_id,
                    ]
                x_coord <- (parent1_node$x_coord + parent2_node$x_coord)/2
                
                grid_unit_fact <- parent1_node$grid_unit_fact
            }
        }
        
        y_coord <- time_coordinates$coord[
            time_coordinates$param == current_node$time
            ]
        
        rev_tree_coord_df <- rbind(
            rev_tree_coord_df,
            cbind(
                current_node_rev,
                as.data.frame(lst(x_coord, y_coord, orient, grid_unit_fact))
            )
        )
    }
    
    ## add a top root node
    rev_tree_coord_df <- rbind(
        data.frame(
            id = rev_tree_coord_df$id[1] + 1,
            event = "root", pop = rev_tree_coord_df$pop[1], 
            time = NA, Ne = rev_tree_coord_df$Ne[1],
            param = NA, 
            leaf = FALSE, root = NA, 
            child1_id = rev_tree_coord_df$id[1], 
            child2_id = NA, 
            x_coord = rev_tree_coord_df$x_coord[1], 
            y_coord = rev_tree_coord_df$y_coord[1] + grid_unit, 
            orient = NA,
            grid_unit_fact = grid_unit,
            stringsAsFactors = FALSE
        ),
        rev_tree_coord_df
    )
    
    ## output
    return(rev_tree_coord_df)
}

#' Given two nodes, extract edge coordinate
#' @keywords internal
#' @author Ghislain Durif
extract_edge_coordinate <- function(current_node, child_node, ntime,
                                    grid_unit = 2) {
    # additional line for text (to manage split event)
    additional_row <- NULL
    # text to write
    current_text <- NA
    if(child_node$event == "sample") {
        current_text <- str_c("Sample\npop", child_node$pop, sep = " ")
    } else if(child_node$event == "split")  {
        additonal_text <- NA
        if(current_node$event == "split1") {
            additonal_text <- str_c(current_node$param)
        } else if(current_node$event == "split2") {
            additonal_text <- str_c("1 - ", current_node$param)
        }
        additional_row <- data.frame(
            x_start = NA,
            y_start = NA,
            x_end = (current_node$x_coord + child_node$x_coord)/2,
            y_end = child_node$y_coord - grid_unit / ntime,
            Ne = current_node$Ne,
            text = additonal_text,
            stringsAsFactors = FALSE
        )
    }
    # population effective size
    current_Ne <- current_node$Ne
    if((current_node$event == "merge") & (current_node$pop != child_node$pop)) {
        current_Ne <- child_node$Ne
    }
    # edge
    out <- rbind(
        data.frame(
            x_start = current_node$x_coord,
            y_start = current_node$y_coord,
            x_end = child_node$x_coord,
            y_end = child_node$y_coord,
            Ne = current_Ne,
            text = current_text,
            stringsAsFactors = FALSE
        ),
        additional_row
    )
    # output
    return(out)
}


#' Assign coordinates to tree edges (start and end points)
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom dplyr arrange
node2edge_coordinate <- function(tree_df, rev_tree_df, parsed_scenario, 
                                 rev_tree_coord_df,
                                 grid_unit = 2) {
    
    # tree context
    info <- tree_context(parsed_scenario, rev_tree_df, grid_unit)
    timing_list <- info$timing_list
    time_coordinates <- info$time_coordinates
    event_nb <- info$event_nb
    existing_pop <- info$existing_pop
    npop <- info$npop
    
    # for each edge, assign start and end coordinate + details:
    # x_start, y_start, x_end, y_end, Ne, text
    edge_coordinates <- NULL
    for(node_id in rev_tree_coord_df$id) {
        # print(str_c("### node id = ", node_id))
        x_start <- NA
        x_end <- NA
        y_start <- NA
        y_end <- NA
        # current node
        current_node <- rev_tree_coord_df[rev_tree_coord_df$id == node_id,]
        # child1 (if any)
        if(!is.na(current_node$child1_id)) {
            # child node
            child_node <- rev_tree_coord_df[
                rev_tree_coord_df$id == current_node$child1_id,]
            # extract edge
            edge_coordinates <- rbind(
                edge_coordinates,
                extract_edge_coordinate(current_node, child_node, 
                                        length(timing_list), grid_unit)
            )
        }
        # child2 (if any)
        if(!is.na(current_node$child2_id)) {
            # child node
            child_node <- rev_tree_coord_df[
                rev_tree_coord_df$id == current_node$child2_id,]
            # extract edge
            edge_coordinates <- rbind(
                edge_coordinates,
                extract_edge_coordinate(current_node, child_node, 
                                        length(timing_list), grid_unit)
            )
        }
    }
    
    ## output
    return(edge_coordinates)
}

#' Check historical model and reformat scenario encoding for display
#' @keywords internal
#' @author Ghislain Durif
prepare_hist_model_display <- function(parsed_scenario, grid_unit = 2) {
    
    edge_coordinates <- NULL
    time_coordinates <- NULL
    
    nevent <- parsed_scenario$nevent
    npop <- parsed_scenario$npop
    
    tree_df <- scenario2tree(parsed_scenario)
    rev_tree_out <- reverse_tree(tree_df)
    
    msg_list <- rev_tree_out$msg_list
    valid <- rev_tree_out$valid
    
    if(rev_tree_out$valid) {
        rev_tree_df <- rev_tree_out$rev_tree_df
        rev_tree_coord_df <- tree2node_coordinate(tree_df, rev_tree_df, 
                                                  parsed_scenario, grid_unit)
        
        edge_coordinates <- node2edge_coordinate(tree_df, rev_tree_df, 
                                                 parsed_scenario, 
                                                 rev_tree_coord_df, grid_unit)
        
        # tree context
        info <- tree_context(parsed_scenario, rev_tree_df, grid_unit)
        time_coordinates <- info$time_coordinates
    }
    
    return(lst(edge_coordinates, valid, msg_list, time_coordinates, 
               nevent, npop, grid_unit))
}

#' Box frame for tree graph
#' @keywords internal
#' @author Ghislain Durif
box_frame <- function(g, npop, nevent, grid_unit = 2) {
    # box x_width x y_width
    x_width <- grid_unit * npop
    y_height <- grid_unit * nevent
    # x frontier: -(x_width + 1)/2 , (x_width + 1)/2
    # y frontier: 0 , y_width + 1
    g <- g + xlim(c(-(x_width + 1)/2, (x_width + 1)/2 + 1)) +
        ylim(c(0, y_height + 1))
    return(g)
}

#' display historical model
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom ggplot2 aes annotate ggplot ggtitle geom_point geom_segment theme_void
display_hist_model <- function(data2plot) {
    
    # tree
    g1 <- ggplot(data2plot$edge_coordinates) +
        geom_segment(aes(x=x_start, y=y_start, 
                         xend=x_end, yend=y_end, col = Ne), 
                     size = 2, 
                     na.rm = TRUE) + 
        geom_label(aes(x=x_end, y=y_end, label = text), 
                   na.rm = TRUE) +
        theme_void(base_size = 12) + 
        scale_x_continuous(expand = c(0.2, 0.2)) + 
        scale_y_continuous(expand = c(0.2, 0)) +
        ggtitle("(Warning! Time is not to scale)") +
        theme(legend.position = "left", 
              legend.justification = "top", 
              legend.direction = "vertical", 
              plot.margin = margin(10,10,10,10),
              plot.title = element_text(size = 12, hjust = 1))
    
    # timeline
    g1 <- g1 + geom_segment(
            x = max(data2plot$edge_coordinates$x_end, na.rm = TRUE) + 
                data2plot$grid_unit,
            xend = max(data2plot$edge_coordinates$x_end, na.rm = TRUE) + 
                data2plot$grid_unit,
            y = max(data2plot$edge_coordinates$y_start, na.rm = TRUE),
            yend = min(data2plot$edge_coordinates$y_end, na.rm = TRUE), 
            na.rm = TRUE) +
        geom_label(
            data = data2plot$time_coordinates, 
            aes(
                x = max(data2plot$edge_coordinates$x_end, na.rm = TRUE) + 
                    data2plot$grid_unit,
                y = coord,
                label = str_pad(
                    param,
                    width = ifelse(str_length(param) == 1, 3, 0),
                    side = "both"
                )
            ), 
            na.rm = TRUE
        )
    
    return(g1)
}