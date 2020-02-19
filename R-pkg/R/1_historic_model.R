#' Historical model ui
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
#' @importFrom magrittr %>%
hist_model_ui <- function(id, label = "hist_model", add=FALSE) {
    ns <- NS(id)
    tagList(
        verticalLayout(
            flowLayout(
                textAreaInput(
                    ns("scenario"), 
                    label = "Describe your scenario", 
                    value = "N1 N2\n0 sample 1\n0 sample 2\nt sample 1\nt2 merge 1 2", 
                    rows = 10,
                    resize = "none"
                ),
                plotOutput(ns("scenario_graph"), height = "200px"),
                verticalLayout(
                    h5("Check"),
                    verbatimTextOutput(ns("check")),
                    h5("Comments"),
                    verbatimTextOutput(ns("comment"))
                )
            ),
            navbarPage("",
                tabPanel(
                    "Parameters",
                    uiOutput(ns("param_value"))
                ),
                tabPanel(
                    "Sample size",
                    uiOutput(ns("sample_param"))
                ),
                tabPanel(
                    "Simulations",
                    numericInput(
                        ns("nsimu"),
                        label = "Number of repetitions",
                        value = 1
                    )
                )
            )
        )
    )
}

#' Historical model module function
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
hist_model_module <- function(input, output, session) {
    
    # tmp graph
    # FIXME
    output$scenario_graph <- renderPlot({ plot( (1:3), c(0,1,0), 
                                                type = "l", 
                                                axes = FALSE,
                                                xlab = "",
                                                ylab = "")})
    
    # Graph check
    output$check <- renderText({ "Historical model is being checked." })
    
    # Graph comments
    output$comment <- renderText({
        paste0(
            "Warning:\n", 
            "- 1000 to 20000 simulations per scenario  are needed for scenario ",
            "choice.\n",
            "- 10000 to 100000 simulations under the scenario of interest are ",
            "needed for parameter estimation."
        )
    })
    
    # scenario
    scenario_param <- reactive({
        parse_scenario(input$scenario)
    })
    
    # parameters
    output$param_value <- renderUI({
        
        param_list <- scenario_param()$parameters
        print(paste0(param_list))
        
        # numeric input for each parameters
        numeric_input_list <- list()
        if(length(param_list) > 0) {
            numeric_input_list <- lapply(param_list, function(param) {
                numericInput(param, label = param, value = 100)
            })
        }
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, numeric_input_list)
    })
    
    # sample
    output$sample_param <- renderUI({
        
        npop <- scenario_param()$npop
        print(paste0(npop))
        
        # numeric input for each sample
        numeric_input_list <- list()
        if(!is.null(npop) & npop > 0) {
            numeric_input_list <- lapply(1:npop, function(pop) {
                pop_name <- paste0("pop", pop)
                tagList(
                    fluidRow(
                        column(
                            width = 2, 
                            h4(paste0("Population ", pop)),
                        ),
                        column(
                            width = 2, 
                            numericInput(
                                paste0(pop_name, "_f"),
                                label = NULL,
                                value = 25
                            )
                        ),
                        column(width = 2, 
                            numericInput(
                                paste0(pop_name, "_m"),
                                label = NULL,
                                value = 25
                            )
                        )
                    )
                )
            })
        }
        # sample table
        sample_table <- fluidRow(
            column(width=2, offset = 2, h4("Female")),
            column(width=2, h4("Male"))
        )
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        
        do.call(tagList, c(list(sample_table, numeric_input_list)))
    })
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
    parameters <- NULL
    scenario_check <- TRUE
    # number of populations
    npop <- ifelse(str_detect(scenario[1], pattern = " |[a-z]"),
                   str_count(scenario[1], pattern = " ") + 1,
                   0)
    # population effective size at time 0
    Ne_list <- unlist(
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
    parameters <- unique(unlist(c(Ne_list, event_time, event_param)))
    parameters <- parameters[!str_detect(string = parameters, 
                                         pattern = "^([0-9]+|[01]\\.?[0-9]?)$")]
    
    # out
    return(lst(parameters, npop, nevents, event_type, event_time, 
               event_pop, event_param, event_check, scenario_check))
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
