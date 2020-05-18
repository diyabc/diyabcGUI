#' Plot historical model
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom ggplot2 ggplot geom_point geom_segment theme_void
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
    return(g1)
}