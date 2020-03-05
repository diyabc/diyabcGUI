#' Historical model definition module ui
#' @keywords internal
#' @author Ghislain Durif
hist_model_def_module_ui <- function(
    id, 
    value = "N1 N2\n0 sample 1\n0 sample 2\nt sample 1\nt2 merge 1 2"
) {
    ns <- NS(id)
    tagList(
        flowLayout(
            textAreaInput(
                ns("scenario"), 
                label = "Describe your scenario", 
                value = value, 
                rows = 10,
                resize = "none"
            ) %>% 
                helper(type = "markdown", 
                       content = "hist_model_description"),
            verticalLayout(
                plotOutput(ns("scenario_graph"), height = "200px"),
                actionButton(ns("save"), label = "Save")
            )
        )
    )
}

#' Historical model definition module server
#' @keywords internal
#' @author Ghislain Durif
hist_model_def_module_server <- function(input, output, session) {
    
    scenario_def <- reactiveValues()
    
    # scenario
    observe({
        scenario_def$raw <- input$scenario
        scenario_def$param <- parse_scenario(input$scenario)
    })
    
    # observeEvent(input$scenario, {
    #     scenario_def$raw <- input$scenario
    #     scenario_def$param <- parse_scenario(input$scenario)
    #     scenario_def$parsed <- TRUE
    # })
    
    # tmp graph
    # FIXME
    output$scenario_graph <- renderPlot({ 
        plot_hist_model(scenario_def$param)
    })
    
    # # Graph check
    # output$check <- renderText({ "Historical model is being checked." })
    # 
    # # Graph comments
    # output$warning <- renderUI({
    #     tagList(
    #         tags$div(
    #             tags$ul(
    #                 tags$li(
    #                     paste0("1000 to 20000 simulations per scenario ",
    #                            "are needed for scenario choice.")
    #                 ),
    #                 tags$li(
    #                     paste0("10000 to 100000 simulations under the ",
    #                            "scenario of interest are needed for ",
    #                            "parameter estimation.")
    #                 )
    #             )
    #         )
    #     )
    # })
    
    return(scenario_def)
}

#' Parse scenario
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_count str_detect str_extract_all str_split
#' @importFrom tibble lst
parse_scenario <- function(text) {
    if(is.null(text)) return(NULL)
    keywords <- c("sample", "varNe", "merge", "split")
    scenario <- unlist(str_split(text, pattern = "\n"))
    Ne_param <- NULL
    rate_param <- NULL
    time_param <- NULL
    scenario_check <- TRUE
    # number of populations
    npop <- ifelse(str_detect(scenario[1], pattern = " |[a-z]"),
                   str_count(scenario[1], pattern = " ") + 1,
                   0)
    # population effective size at time 0
    Ne_list_t0 <- unlist(
        str_extract_all(
            scenario[1], 
            pattern = paste0(c("[a-zA-Z][a-zA-Z0-9]*(?= |\n)", 
                               "[a-zA-Z][a-zA-Z0-9]*$"), 
                             collapse = "|")))
    
    ## events
    events <- str_extract_all(scenario, 
                              pattern = paste0(keywords, collapse = "|"))
    # no event on 1st line
    scenario_check <- scenario_check & (length(events[0]) == 0)
    # one event per remaining line
    scenario_check <- scenario_check & 
        all(unlist(lapply(events[-1], function(event) length(event) == 1)))
    ## parse events
    description <- lapply(scenario[-1], parse_event)
    # event number
    nevents <- length(description)
    # event type
    event_type <- unlist(lapply(description, function(event) event$event_type))
    # event time
    event_time <- lapply(description, function(event) event$event_time)
    # population concerned by events
    event_pop <- lapply(description, function(event) event$event_pop)
    # event parameters
    event_param <- lapply(description, function(event) event$event_param)
    # event check
    event_check <- unlist(lapply(description, function(event) event$event_check))
    scenario_check <- scenario_check & all(event_check)
    
    ## parameters
    Ne_param <- unique(c(unlist(event_param[event_type == "varNe"]),
                         Ne_list_t0))
    Ne_param <- Ne_param[!str_detect(string = Ne_param, 
                                     pattern = "^([0-9]+|[01]\\.?[0-9]?)$")]
    
    rate_param <- unique(unlist(event_param[event_type == "split"]))
    rate_param <- rate_param[!str_detect(string = rate_param, 
                                         pattern = "^([0-9]+|[01]\\.?[0-9]?)$")]
    
    time_param <- unique(unlist(event_time))
    time_param <- time_param[!str_detect(string = time_param, 
                                         pattern = "^([0-9]+|[01]\\.?[0-9]?)$")]
    
    # out
    return(lst(npop, nevents, event_type, event_time, 
               event_pop, event_param, event_check, Ne_param, Ne_list_t0, 
               rate_param, time_param, scenario_check))
}

#' Parse a scenario event
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_detect str_extract str_extract_all str_match
#' @importFrom tibble lst
parse_event <- function(event) {
    keywords <- c("sample", "varNe", "merge", "split")
    event_type <- NULL
    event_time <- NULL
    event_param <- NULL
    event_pop <- NULL
    event_check <- TRUE
    # check event presence and structure (<time> <event> ...)
    event_check <- event_check &
        str_detect(string = event,
                   pattern = paste0("^[a-zA-Z0-9]+ (",
                                    paste0(keywords, collapse = "|"),
                                    ")"))
    if(event_check) {
        # event type
        event_type <- as.vector(str_match(string = event, 
                                          pattern = paste0(keywords, 
                                                           collapse = "|")))
        
        # event time
        event_time <- str_extract(string = event,
                                  pattern = "^[a-zA-Z0-9]+(?= )")
        # check numeric value for <time>
        tmp <- str_detect(string = event_time,
                          pattern = "^[0-9]+$")
        if(tmp) {
            event_time <- as.numeric(event_time)
        }
        
        ## possible event
        out <- switch(
            event_type, 
            "sample" = parse_sample(event),
            "varNe" = parse_varNe(event),
            "merge" = parse_merge(event),
            "split" = parse_split(event))
        
        event_param <- out$event_param
        event_pop <- out$event_pop
        # check
        event_check <- event_check & out$event_check
    }
    # out
    return(lst(event_type, event_time, event_param, 
               event_pop, event_check))
}

#' Parse merge event
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_detect str_extract_all
#' @importFrom tibble lst
parse_merge <- function(event) {
    event_type <- "merge"
    event_param <- NULL
    event_pop <- NULL
    # <time> merge <pop0> <pop1>
    event_check <- str_detect(
                        string = event, 
                        pattern = paste0("^", time_regex(), 
                                         " merge [0-9]+ [0-9]+$"))
    if(event_check) {
        event_pop <- as.integer(unlist(str_extract_all(
                                        string = event, 
                                        pattern = "(?<=merge| )[0-9]+")))
    }
    return(lst(event_type, event_param, event_pop, event_check))
}

#' Parse sample event
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_detect str_extract
#' @importFrom tibble lst
parse_sample <- function(event) {
    event_type <- "sample"
    event_param <- NULL
    event_pop <- NULL
    # <time> sample <pop>
    event_check <- str_detect(string = event, 
                              pattern = paste0("^", time_regex(), 
                                               " sample [0-9]+$"))
    if(event_check) {
        event_pop <- as.integer(str_extract(string = event, 
                                            pattern = "(?<=sample )[0-9]+$"))
    }
    return(lst(event_type, event_param, event_pop, event_check))
}

#' Parse split event
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_detect str_extract
#' @importFrom tibble lst
parse_split <- function(event) {
    event_type <- "split"
    event_param <- NULL
    event_pop <- NULL
    # <time> split <pop0> <pop1> <pop2> <rate>
    event_check <- str_detect(
        string = event, 
        pattern = paste0("^", time_regex(), " split [0-9]+ [0-9]+ [0-9]+ ",
                         rate_regex(), "$"))
    if(event_check) {
        event_pop <- as.integer(unlist(str_extract_all(
                                        string = event, 
                                        pattern = "(?<=split| )[0-9]+(?= )")))
        event_param <- str_extract(
                            string = event, 
                            pattern = paste0("(?<= )", rate_regex(), "$"))
        # check numeric value for <rate>
        tmp <- str_detect(string = event_param,
                          pattern = paste0("^", num_rate_regex(), "$"))
        if(tmp) {
            event_param <- as.numeric(event_param)
            event_check <- event_check & (event_param >= 0 & event_param <= 1)
        }
    }
    return(lst(event_type, event_param, event_pop, event_check))
}

#' Parse varNe event
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_detect str_extract
#' @importFrom tibble lst
parse_varNe <- function(event) {
    event_type <- "varNe"
    event_param <- NULL
    event_pop <- NULL
    # <time> varNe <pop> <Ne>
    event_check <- str_detect(string = event, 
                              pattern = "^[a-zA-Z0-9]+ varNe [0-9]+ [a-zA-Z0-9]+$")
    if(event_check) {
        event_pop <- as.integer(str_extract(
                string = event, 
                pattern = "(?<=varNe )[0-9]+(?= [a-zA-Z0-9]+$)"))
        event_param <- str_extract(string = event, 
                                   pattern = "(?<= )[a-zA-Z0-9]+$")
        # check numeric value for <Ne>
        tmp <- str_detect(string = event_param,
                          pattern = "^[0-9]+$")
        if(tmp) {
            event_param <- as.integer(event_param)
        }
    }
    return(lst(event_type, event_param, event_pop, event_check))
}

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
    g1 <- add_edge(g1, start = c(5,10), end = c(5,11), colour = "black")
    g1 <- add_edge(g1, start = c(5,10), end = c(0,0), colour = "black")
    g1 <- add_edge(g1, start = c(5,10), end = c(10,0), colour = "black")
    g1 <- add_timeline(g1)
    return(g1)
}