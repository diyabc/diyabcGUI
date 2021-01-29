#' Historical model definition module ui
#' @keywords internal
#' @author Ghislain Durif
hist_model_ui <- function(id) {
    ns <- NS(id)
    fluidRow(
        column(
            width = 4,
            verticalLayout(
                h4("Describe your scenario") %>% 
                    helper(type = "markdown", 
                           content = "hist_model_description"),
                textAreaInput(
                    ns("scenario"), 
                    label = NULL, 
                    height = "356px", 
                    resize = "none"
                ),
                actionButton(
                    ns("validate"),
                    label = "Validate",
                    icon = icon("check"),
                    width = '100%'
                ),
                uiOutput(ns("parser_msg"))
            )
        ),
        column(
            width = 8,
            graph_display_ui(ns("model_display"))
        )
    )
}

#' Historical model definition module server
#' @keywords internal
#' @author Ghislain Durif
#' @param project_dir project directory as a `reactive`.
#' @param raw_scenario raw scenario as a `reactive`.
hist_model_server <- function(input, output, session,
                              project_dir = reactive({NULL}), 
                              raw_scenario = reactive({NULL}),
                              scenario_id = reactive({NULL})) {
    # init local reactive values
    local <- reactiveValues(
        graph = NULL,
        parser_msg = NULL,
        project_dir = NULL,
        raw_scenario = NULL,
        scenario_id = NULL
    )
    # init output reactive values
    out <- reactiveValues(
        cond = NULL,
        raw = NULL,
        param = NULL,
        trigger = NULL
    )
    # get input
    observe({
        local$project_dir = project_dir()
        local$raw_scenario = raw_scenario()
        local$scenario_id = scenario_id()
        # logging("input raw scenario = ", local$raw_scenario)
    })
    # update if input provided
    observe({
        req(local$raw_scenario)
        updateTextAreaInput(session, "scenario", value = local$raw_scenario)
    })
    # parse and check input scenario
    observeEvent(input$validate, {
        # input (remove empty final line)
        out$raw <- str_replace(string = input$scenario, 
                               pattern = "\\n$", 
                               replacement = "")
        out$trigger <- ifelse(is.null(out$trigger), 0, out$trigger) + 1
        # parse
        out$param <- parse_scenario(out$raw)
        # parser message list
        local$parser_msg <- out$param$msg_list
        # if valid
        if(out$param$valid) {
            
            # check conditions
            out$cond <- check_condition(input$scenario, out$param)$cond
            
            # prepare data for graph
            data2plot <- prepare_hist_model_display(out$param, grid_unit = 2)
            
            # check for validity
            out$param$valid <- data2plot$valid
            
            # potential message
            local$parser_msg <- append(
                local$parser_msg,
                data2plot$msg_list
            )
            
            # if valid
            if(out$param$valid) {
                # graphical representation
                local$graph <- display_hist_model(data2plot)
            } else {
                local$graph <- NULL
            }
            
        } else {
            local$graph <- NULL
            out$cond <- NULL
        }
    })
    # update local
    observeEvent(out$raw, {
        local$raw_scenario <- out$raw
    })
    # update parser message
    observeEvent(local$parser_msg, {
        output$parser_msg <- renderUI({
            if(!is.null(local$parser_msg) & is.list(local$parser_msg) & 
               length(local$parser_msg) > 0) {
                helpText(
                    h4(icon("warning"), "Issue(s) with scenario"),
                    do.call(
                        tags$ul,
                        lapply(local$parser_msg, function(item) {
                            return(tags$li(item))
                        })
                    )
                )
            } else {
                tagList()
            }
        })
    })
    # # debugging
    # observe({
    #     logging("project dir :", local$project_dir)
    #     logging("historic model :", out$raw)
    # })
    
    ## graph display
    callModule(graph_display_server, "model_display", 
               graph = reactive(local$graph), 
               project_dir = reactive(local$project_dir),
               scenario_id = reactive(local$scenario_id))
    # output
    return(out)
}

#' Check timeline
#' @description 
#' Check for possible conditions regarding scenario validity, time consistency, 
#' etc.
#' @keywords internal
#' @author Ghislain Durif
check_condition <- function(raw_scenario, parsed_scenario) {
    # FIXME
    condition <- list()
    # condition <- c("Example of condition...", "Time should be positive.")
    # time related condition
    time_param <- parsed_scenario$time_param
    if(!is.null(time_param) & (length(time_param) > 1)) {
        time_cond <- lapply(
            1:(length(time_param)-1),
            function(ind) {
                return(str_c(time_param[ind+1], ">=", time_param[ind]))
            })
        condition <- c(condition, time_cond)
    }
    # output
    return(list(cond = condition))
}

#' Parse scenario
#' @description 
#' Extract event and parameters from raw scenario, check for syntax validity. 
#' 
#' Also check for scenario validity, regarding time consistency.
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_c str_count str_detect str_extract_all str_length str_split
#' @importFrom tibble lst
parse_scenario <- function(text) {
    # init output
    npop <- NULL
    nevent <- NULL 
    event_type <- NULL
    event_time <- NULL
    event_pop <- NULL
    event_param <- NULL
    valid <- TRUE
    Ne_param <- NULL
    Ne_list_0 <- NULL
    rate_param <- NULL
    time_param <- NULL 
    msg_list <- list()
    # possible events
    possible_events <- c("sample", "varNe", "merge", "split")
    event_regex <- str_c(
        "(?i)",
        str_c(possible_events, collapse = "|"),
        "(?-i)"
    )
    ## check if input is not empty
    if(is.null(text) | str_length(text) == 0) {
        valid <- FALSE
        msg_list <- append(msg_list, "Empty input")
    } else {
        # rewrite event if any
        text <- str_replace_all(text, pattern = "(?i)sample(?-i)", 
                                replacement = "sample")
        text <- str_replace_all(text, pattern = "(?i)varNe(?-i)", 
                                replacement = "varNe")
        text <- str_replace_all(text, pattern = "(?i)merge(?-i)", 
                                replacement = "merge")
        text <- str_replace_all(text, pattern = "(?i)split(?-i)", 
                                replacement = "split")
        # extract scenario rows
        scenario <- str_trim(unlist(str_split(text, pattern = "\n")))
        ## first line
        # initial population effective size
        Ne_list_0 <- unlist(
            str_extract_all(
                unlist(str_split(scenario[1], pattern = " ")), 
                pattern = param_regex()
            )
        )
        if(length(Ne_list_0) == 0) {
            valid <- FALSE
            msg_list <- append(
                msg_list, str_c("First row with initial population effective ",
                                "sizes is not well formatted."))
        }
        # number of populations
        npop <- length(Ne_list_0)
        ## events
        # no event on 1st line
        events <- str_extract_all(
            scenario, pattern = event_regex
        )
        if(length(events[0]) > 0) {
            valid <- FALSE
            msg_list <- append(
                msg_list, str_c("First row should specify initial population ", 
                                "effective sizes and not define an event."))
        } 
        # one event per remaining line
        if(any(unlist(lapply(events[-1], 
                             function(event) length(event) != 1)))) {
            valid <- FALSE
            msg_list <- append(
                msg_list, str_c("All rows (except first one) should define a ",
                                "single event."))
        }
        ## parse events
        event_list <- lapply(scenario[-1], parse_event)
        # event type
        event_type <- unlist(lapply(event_list, 
                                    function(event) event$event_type))
        # event time
        event_time <- lapply(event_list, function(event) event$event_time)
        # population concerned by events
        event_pop <- lapply(event_list, function(event) event$event_pop)
        # event parameters
        event_param <- lapply(event_list, function(event) event$event_param)
        # event check
        event_valid <- unlist(lapply(event_list, 
                                     function(event) event$event_valid))
        if(!all(event_valid)) {
            valid <- FALSE
            msg_list <- append(
                msg_list, lapply(which(!event_valid), function(row) {
                    return(
                        str_c("There is an issue with event format at row ", 
                              row+1)
                    )
                }))
        }
        # number of events
        nevent <- ifelse(length(scenario) > 0, length(scenario) - 1, 0)
        if(nevent == 0) {
            valid <- FALSE
            msg_list <- append(
                msg_list, str_c("No event in scenario."))
        } else {
            ## check populations
            pop_id <- unlist(lapply(event_list, 
                                    function(event) event$event_pop))
            if(max(pop_id) > npop) {
                valid <- FALSE
                msg_list <- append(
                    msg_list, 
                    str_c(
                        "Number of initial population effective sizes ", "
                    at row 1 is lower than number of populations used ", 
                        "in scenario event description."
                    )
                )
            }
            
            ## check for repetitions and historical inconsistency
            tmp <- Reduce(
                "rbind", 
                lapply(
                    1:nevent,
                    function(event_id) {
                        tmp_msg <- ""
                        tmp_valid <- TRUE
                        if(event_id > 1) {
                            ## check for use of non existing population
                            # check for previous merge regarding 
                            # the considered population
                            previous_merge <- which(
                                event_type[1:(event_id-1)] == "merge"
                            )
                            disappeared_pop <- unlist(
                                lapply(
                                    previous_merge, 
                                    function(ind) {
                                        return(event_pop[[ind]][2])
                                    }
                                )
                            )
                            check_previous_merge <- any(
                                event_pop[[event_id]] %in% disappeared_pop
                            )
                            if(check_previous_merge) {
                                tmp_valid <- FALSE
                            }
                            # check for previous split regarding the 
                            # considered population
                            previous_split <- which(
                                event_type[1:(event_id-1)] == "split"
                            )
                            disappeared_pop <- unlist(
                                lapply(
                                    previous_split, 
                                    function(ind) {
                                        return(event_pop[[ind]][1])
                                    }
                                )
                            )
                            check_previous_split <- any(
                                event_pop[[event_id]] %in% disappeared_pop
                            )
                            if(check_previous_split) {
                                tmp_valid <- FALSE
                            }
                            # message
                            if(!tmp_valid) {
                                tmp_msg <- str_c(
                                    "Use of non-existing population ", 
                                    "in event at row ",
                                    event_id+1
                                )
                            }
                        }
                        return(data.frame(tmp_valid, tmp_msg, 
                                          stringsAsFactors = FALSE))
                    }
                )
            )
            
            if(!all(tmp$tmp_valid)) {
                valid <- FALSE
                msg_list <- c(
                    msg_list,
                    as.list(tmp$tmp_msg[!tmp$tmp_valid])
                )
            }
            
            ## check for full coalescence
            # TODO -> done when preparing tree graphic representation
            
            ## check for duplicate
            if(length(scenario) != length(unique(scenario))) {
                valid <- FALSE
                msg_list <- c(
                    msg_list,
                    "One or more events are duplicated."
                )
            }
            
            ## parameters
            Ne_param <- unique(c(unlist(event_param[event_type == "varNe"]),
                                 Ne_list_0))
            Ne_param <- unique(unlist(
                str_extract_all(string = Ne_param, 
                                pattern = single_param_regex())
            ))
            Ne_param <- Ne_param[!str_detect(
                string = Ne_param, pattern = "^([0-9]+|[01]\\.?[0-9]?)$"
            )]
            
            rate_param <- unique(unlist(event_param[event_type == "split"]))
            rate_param <- rate_param[!str_detect(
                string = rate_param, pattern = "^([0-9]+|[01]\\.?[0-9]?)$"
            )]
            
            time_param <- unique(unlist(
                str_extract_all(string = event_time, 
                                pattern = single_time_regex())
            ))
            time_param <- time_param[!str_detect(
                string = time_param, pattern = "^([0-9]+|[01]\\.?[0-9]?)$"
            )]
        }
    }
    ## output
    return(lst(npop, nevent, 
               event_type, event_time, event_pop, event_param, 
               valid, Ne_param, Ne_list_0, rate_param, time_param, 
               msg_list))
}

#' Parse a scenario event
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_detect str_extract str_extract_all str_match
#' @importFrom tibble lst
parse_event <- function(event) {
    # possible event
    possible_events <- c("sample", "varNe", "merge", "split")
    event_regex <- str_c(possible_events, collapse = "|")
    # init output 
    event_type <- NULL
    event_time <- NULL
    event_param <- NULL
    event_pop <- NULL
    event_valid <- TRUE
    # event type
    event_type <- str_extract(string = event, 
                              pattern = event_regex)
    if(is.na(event_type)) {
        event_type <- NULL
        event_valid <- FALSE
    } else {
        # event time
        event_time <- str_extract(string = event, 
                                  pattern = str_c("^", time_regex(), "(?= )"))
        if(is.na(event_time)) {
            event_time <- NULL
            event_valid <- FALSE
        } else {
            # check numeric value for <time>
            num_check <- str_detect(string = event_time, pattern = "^[0-9]+$")
            if(num_check) {
                event_time <- as.numeric(event_time)
            }
            ## possible event
            out <- switch(
                event_type, 
                "sample" = parse_sample(event),
                "varNe" = parse_varNe(event),
                "merge" = parse_merge(event),
                "split" = parse_split(event))
            # param
            event_param <- out$event_param
            # pop
            event_pop <- out$event_pop
            # validity
            event_valid <- out$event_check
        }
    }
    # out
    return(lst(event_type, event_time, event_param, 
               event_pop, event_valid))
}

#' Parse merge event
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_c str_detect str_extract_all
#' @importFrom tibble lst
parse_merge <- function(event) {
    event_type <- "merge"
    event_param <- NULL
    event_pop <- NULL
    # <time> merge <pop0> <pop1>
    event_check <- str_detect(
        string = event, 
        pattern = str_c("^", time_regex(), " merge [0-9]+ [0-9]+$"))
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
#' @importFrom stringr str_c str_detect str_extract
#' @importFrom tibble lst
parse_sample <- function(event) {
    event_type <- "sample"
    event_param <- NULL
    event_pop <- NULL
    # <time> sample <pop>
    event_check <- str_detect(
        string = event, 
        pattern = str_c("^", time_regex(), " sample [0-9]+$"))
    if(event_check) {
        event_pop <- as.integer(str_extract(string = event, 
                                            pattern = "(?<=sample )[0-9]+$"))
    }
    return(lst(event_type, event_param, event_pop, event_check))
}

#' Parse split event
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_c str_detect str_extract
#' @importFrom tibble lst
parse_split <- function(event) {
    event_type <- "split"
    event_param <- NULL
    event_pop <- NULL
    # <time> split <pop3> <pop1> <pop2> <rate>
    event_check <- str_detect(
        string = event, 
        pattern = str_c("^", time_regex(), " split [0-9]+ [0-9]+ [0-9]+ ",
                        rate_regex(), "$"))
    if(event_check) {
        event_pop <- as.integer(unlist(str_extract_all(
            string = event, 
            pattern = "(?<=split| )[0-9]+(?= )")))
        event_param <- str_extract(
            string = event, 
            pattern = paste0("(?<= )", rate_regex(), "$"))
        # check numeric value for <rate>
        num_check <- str_detect(string = event_param,
                                pattern = paste0("^", num_rate_regex(), "$"))
        if(num_check) {
            event_param <- as.numeric(event_param)
            event_check <- event_check & (event_param >= 0 & event_param <= 1)
        }
    }
    return(lst(event_type, event_param, event_pop, event_check))
}

#' Parse varNe event
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_c str_detect str_extract
#' @importFrom tibble lst
parse_varNe <- function(event) {
    event_type <- "varNe"
    event_param <- NULL
    event_pop <- NULL
    # <time> varNe <pop> <Ne>
    event_check <- str_detect(
        string = event, 
        pattern = str_c("^", time_regex(), " varNe [0-9]+ ", 
                        param_regex(), "$"))
    if(event_check) {
        event_pop <- as.integer(str_extract(
            string = event, 
            pattern = "(?<=varNe )[0-9]+(?= )"))
        event_param <- str_extract(
            string = event, 
            pattern = str_c("(?<= )", param_regex(), "$"))
        # check numeric value for <Ne>
        num_check <- str_detect(string = event_param, pattern = "^[0-9]+$")
        if(num_check) {
            event_param <- as.integer(event_param)
        }
    }
    return(lst(event_type, event_param, event_pop, event_check))
}
