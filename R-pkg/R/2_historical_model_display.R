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
            return(-1)
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
                    return(-1)
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
    
    Ne_list <- parsed_scenario$Ne_list_0
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
                        current_pop <- event_pop[2]
                        parent_pop <- event_pop[1]
                    } else if(event_type == "varNe") {
                        Ne_list[[current_pop]] <- event_param
                        assign("Ne_list", Ne_list, envir = env)
                    }
                    
                    parent_event <- find_parent_event(
                        event_type_list = parsed_scenario$event_type, 
                        event_pop_list = parsed_scenario$event_pop, 
                        event_id = event_id, pop_id = parent_pop
                    )
                    
                    tmp_out <- data.frame(
                        id = event_id, event = event_type, pop = parent_pop, 
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
        msg_list <- append(
            msg_list,
            str_c(
                "Issue with scenario: ",
                "populations", str_c(tree_df$pop[root_ind], collapse = ", "),
                "do not coalesce.",
                sep = "."
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

#' Assign coordinates to tree edges (start and end points)
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom dplyr arrange
tree_edge_coordinate <- function(tree_df, rev_tree_df, parsed_scenario,
                                 unit_grid = 2) {
    
    # timing list
    timing_list <- unique(unlist(parsed_scenario$event_time))
    
    # y-coordinates (for each time)
    time_coordinate <- data.frame(
        param = timing_list,
        coord = seq(0, 10, length.out = length(timing_list))
    )
    
    # number of events by timing
    event_nb <- as.data.frame(table(unlist(parsed_scenario$event_time))) %>% 
        arrange(timing_list)
    colnames(event_nb) <- c("param", "count")
    
    npop <- max(unlist(existing_pop))
    
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
    
    # for each tree node, assign a coordinate
    rev_tree_coord_df <- NULL
    for(node_id in rev_tree_df$id) {
        print(str_c("### node id = ", node_id))
        x_coord <- NA
        y_coord <- NA
        orient <- NA # left (0) of right (1)
        tmp_tree_df <- tree_df[tree_df$id == node_id,]
        tmp_rev_tree_df <- rev_tree_df[rev_tree_df$id == node_id,]
        if(tmp_rev_tree_df$root) {
            x_coord <- 0
        } else {
            # not root
            if(tmp_tree_df$event != "split") {
                parent_node <- rev_tree_coord_df[
                    rev_tree_coord_df$id == tmp_tree_df$parent1_id,
                ]
                parent_x_coord <- parent_node$x_coord
                
                if(parent_node$event == "merge") {
                    fact <- (node_id == parent_node$child2_id) - 
                        (node_id == parent_node$child1_id)
                    
                    if(fact > 0) {
                        orient <- 1
                    } else if(fact < 0) {
                        orient <- 0
                    }
                    
                    x_coord <- parent_x_coord + fact * unit_grid * 
                        existing_pop[[ tmp_tree_df$time ]] / npop
                } else if(parent_node$event == "split") {
                    x_coord <- parent_x_coord
                } else {
                    # sample or varNe (tricky)
                    orient <- parent_node$orient
                    if(!is.na(orient)) {
                        if(orient) {
                            x_coord <- parent_x_coord + unit_grid * 
                                existing_pop[[ tmp_tree_df$time ]] / npop
                        } else {
                            x_coord <- parent_x_coord - unit_grid * 
                                existing_pop[[ tmp_tree_df$time ]] / npop
                        }
                    } else {
                        x_coord <- parent_x_coord
                    }
                }
            } else {
                # split (node between the two parents)
                parent1_node <- rev_tree_coord_df[
                    rev_tree_coord_df$id == tmp_tree_df$parent1_id,
                    ]
                parent2_node <- rev_tree_coord_df[
                    rev_tree_coord_df$id == tmp_tree_df$parent2_id,
                    ]
                x_coord <- (parent1_node$x_coord + parent2_node$x_coord)/2
            }
        }
        
        y_coord <- time_coordinate$coord[
            time_coordinate$param == tmp_tree_df$time
            ]
        
        rev_tree_coord_df <- rbind(
            rev_tree_coord_df,
            cbind(
                tmp_rev_tree_df,
                as.data.frame(lst(x_coord, y_coord, orient))
            )
        )
    }
    
    # for each edge, write the following line:
    # x_start, y_start, x_end = 0, y_end, Ne, text
    
}

#' Check historical model and reformat scenario encoding for display
#' @keywords internal
#' @author Ghislain Durif
prepare_hist_model_display <- function() {
    
}

#' Box frame for tree graph
#' @keywords internal
#' @author Ghislain Durif
box_frame <- function(g, npop, nevent, unit_grid = 2) {
    # box x_width x y_width
    x_width <- unit_grid * npop
    y_height <- unit_grid * nevent
    # x frontier: -(x_width + 1)/2 , (x_width + 1)/2
    # y frontier: 0 , y_width + 1
    g <- g + xlim(c(-(x_width + 1)/2, (x_width + 1)/2)) +
        ylim(c(0, y_height + 1))
    return(g)
}



#' Plot historical model
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom ggplot2 aes annotate ggplot geom_point geom_segment theme_void
plot_hist_model <- function(scenario_param) {
    
    add_edge <- function(g, start, end, colour = "black") {
        g <- g + 
            geom_segment(x = start[1], y = start[2], 
                         xend = end[1], 
                         yend = end[2], 
                         size = 1.2, 
                         colour = colour)
        return(g)
    }
    
    add_timeline <- function(g, events=NULL) {
        g <- g + 
            geom_segment(x = 11, y = 0, 
                         xend = 11, 
                         yend = 11)
        g <- g + 
            geom_segment(x = 11-0.2, y = 10, 
                         xend = 11+0.2, 
                         yend = 10)
        g <- g + 
            geom_segment(x = 11-0.2, y = 0, 
                         xend = 11+0.2, 
                         yend = 0)
        if(!is.null(events) | missing(events)) {
            
        }
        return(g)
    }
    
    box_frame <- data.frame(x=c(0,12), y=c(0,11))
    g1 <- ggplot(box_frame, aes(x,y)) + geom_point(alpha=0) + 
        theme_void()
    # g1 <- add_edge(g1, start = c(5,10), end = c(5,11), colour = "black")
    # g1 <- add_edge(g1, start = c(5,10), end = c(0,0), colour = "black")
    # g1 <- add_edge(g1, start = c(5,10), end = c(10,0), colour = "black")
    # g1 <- add_timeline(g1)
    g1 <- add_edge(g1, start = c(0,0), end = c(10,10), colour = "black")
    g1 <- add_edge(g1, start = c(0,10), end = c(10,0), colour = "black")
    g1 <- g1 + annotate("text", label = "Soon available", x = 5, y = 9)
    return(g1)
}