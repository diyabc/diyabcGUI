#' Training set sub-module ui
#' @keywords internal
#' @author Ghislain Durif
training_set_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("enable_def")),
        uiOutput(ns("enable_control"))
    )
}

#' Training set sub-module server
#' @keywords internal
#' @author Ghislain Durif
training_set_server <- function(input, output, session,
                                data_file = reactive({NULL}),
                                data_info = reactive({NULL}),
                                locus_type = reactive({NULL}),
                                seq_mode = reactive({NULL}),
                                new_proj = reactive({TRUE}),
                                proj_dir = reactive({NULL}), 
                                proj_file_list = reactive({NULL}), 
                                valid_proj = reactive({TRUE})) {
    
    # namespace
    ns <- session$ns
    
    # init local
    local <- reactiveValues(
        proj_header_content = NULL,
        # input
        data_file = NULL,
        data_info = NULL,
        locus_type = NULL,
        seq_mode = NULL,
        new_proj = NULL,
        proj_dir = NULL,
        proj_file_list = NULL,
        valid_proj = NULL
    )
    # get input
    observe({
        local$data_file <- data_file()
        local$data_info <- data_info()
        local$locus_type <- locus_type()
        local$seq_mode <- seq_mode()
        local$new_proj <- new_proj()
        local$proj_dir <- proj_dir()
        local$proj_file_list <- proj_file_list()
        local$valid_proj <- valid_proj()
        
        # # debugging
        # print(local$data_file)
    })
    
    # init output
    out <- reactiveValues(
        valid_proj = TRUE
    )
    
    # # debugging
    # observe({
    #     print("data info")
    #     print(local$data_info)
    # })
    
    # check project directory
    observe({
        proj_file_list <- reactivePoll(
            1000, session,
            checkFunc = function() {
                if(!is.null(local$proj_dir)) {
                    if(dir.exists(local$proj_dir))
                        list.files(local$proj_dir)
                    else
                        list()
                } else {
                    list()
                }
            },
            valueFunc = function() {
                if(!is.null(local$proj_dir)) {
                    if(dir.exists(local$proj_dir))
                        list.files(local$proj_dir)
                    else
                        list()
                } else {
                    list()
                }
            }
        )
        
        local$proj_file_list <- proj_file_list()
    })
    
    ## check headerRF.txt file (if present)
    observe({
        req(!is.null(local$locus_type))
        req(local$valid_proj)
        req(!is.null(local$proj_dir))
        
        proj_header_content <- reactiveFileReader(
            1000, session,
            file.path(local$proj_dir, "headerRF.txt"),
            function(file) {
                if(file.exists(file))
                    parse_diyabc_header(
                        file_name = file, 
                        file_type = "text/plain",
                        locus_type = local$locus_type
                    )
                else
                    list()
            }
        )
        
        local$proj_header_content <- proj_header_content()
    })
    
    # ## debugging
    # observeEvent(local$proj_header_content, {
    #     print("project header content")
    #     print(local$proj_header_content)
    # })
    
    ## training set def (if no header file provided)
    output$enable_def <- renderUI({
        req(!is.null(local$new_proj))
        req(!is.null(local$valid_proj))
        
        if(!local$valid_proj) {
            helpText(
                icon("warning"), "Project set up is not valid."
            )
        } else if(local$new_proj | 
                  !("headerRF.txt" %in% local$proj_file_list)) {
            tagList(
                training_set_def_ui(ns("def"))
            )
        } else {
            tagList(
                show_existing_proj_ui(ns("show"))
            )
        }
    })
    
    ## training set def
    training_set_def <- callModule(
        training_set_def_server, "def",
        data_file = reactive(local$data_file),
        data_info = reactive(local$data_info),
        locus_type = reactive(local$locus_type),
        seq_mode = reactive(local$seq_mode),
        proj_dir = reactive(local$proj_dir),
        raw_scenario_list = reactive({NULL}),
        valid_proj = reactive(local$valid_proj)
    )
    
    ## training set show (if existing)
    show_existing_proj <- callModule(
        show_existing_proj_server, "show",
        proj_header_content = reactive(local$proj_header_content),
        valid_proj = reactive(local$valid_proj)
    )
    
    # get output
    observeEvent(training_set_def$trigger, {
        req(!is.null(training_set_def$trigger))
        req(!is.null(training_set_def$valid_def))
        local$valid_proj <- local$valid_proj & training_set_def$valid_def
        
        # debugging
        print("training set valid def")
        print(training_set_def$valid_def)
    })
    
    ## enable control
    output$enable_control <- renderUI({
        req(!is.null(local$valid_proj))
        
        if(local$valid_proj) {
            training_set_action_ui(ns("action"))
        } else {
            NULL
        }
    })
    
    ## action
    callModule(
        training_set_action_server, "action", 
        proj_dir = reactive(local$proj_dir),
        valid = reactive(local$valid_proj)
    )
    
    # update output
    observe({
        out$valid_proj <- local$valid_proj
    })
    
    # output
    return(out)
}

#' Show existing project set up module ui
#' @keywords internal
#' @author Ghislain Durif
show_existing_proj_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        h3(icon("history"), "Historical models"),
        helpText(
            "Historical scenarii defined in the provided header file."
        ),
        uiOutput(ns("show_scenarii")),
        hr(),
        h3(icon("chart-bar"), "Priors and conditions"),
        helpText(
            "Priors defined in the provided header file."
        ),
        uiOutput(ns("show_priors")),
        hr(),
        h4("Condition setting"),
        helpText(
            "Conditions defined in the provided header file."
        ),
        uiOutput(ns("show_conditions")),
        hr(),
        h3(icon("dna"), "Locus description"),
        helpText(
            "Locus settings defined in the provided header file."
        ),
        uiOutput(ns("show_loci")),
        hr()
    )
}

#' Show existing project set up module server
#' @keywords internal
#' @author Ghislain Durif
show_existing_proj_server <- function(input, output, session,
                                      proj_header_content = reactive({NULL}),
                                      valid_proj = reactive({FALSE})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        # input
        proj_header_content = NULL,
        valid_proj = NULL
    )
    # get input
    observe({
        local$proj_header_content <- proj_header_content()
        local$valid_proj <- valid_proj()
    })
    
    # show historical model
    output$show_scenarii <- renderUI({
        req(!is.null(local$valid_proj))
        req(!is.null(local$proj_header_content$raw_scenario_list))
        
        if(local$valid_proj & 
           length(local$proj_header_content$raw_scenario_list) > 0) {
            tagList(
                do.call(
                    flowLayout,
                    lapply(
                        local$proj_header_content$raw_scenario_list, 
                        function(item) tags$pre(item)
                    )
                )
            )
        } else {
            NULL
        }
    })
    
    # show priors
    output$show_priors <- renderUI({
        req(!is.null(local$valid_proj))
        req(!is.null(local$proj_header_content$raw_prior_list))
        
        if(local$valid_proj & 
           length(local$proj_header_content$raw_prior_list) > 0) {
            tagList(
                do.call(
                    tags$ul,
                    lapply(
                        local$proj_header_content$raw_prior_list,
                        function(item) tags$li(tags$code(item))
                    )
                )
            )
        } else {
            NULL
        }
    })
    
    # show conditions
    output$show_conditions <- renderUI({
        req(!is.null(local$valid_proj))
        req(!is.null(local$proj_header_content$raw_cond_list))
        
        if(local$valid_proj & 
           length(local$proj_header_content$raw_cond_list) > 0) {
            tagList(
                do.call(
                    tags$ul,
                    lapply(
                        local$proj_header_content$raw_cond_list, 
                        function(item) tags$li(tags$code(item))
                    )
                )
            )
        } else {
            NULL
        }
    })
    
    # show locus description
    output$show_loci <- renderUI({
        req(!is.null(local$valid_proj))
        req(!is.null(local$proj_header_content$loci_description))
        
        if(local$valid_proj & 
           length(local$proj_header_content$loci_description) > 0) {
            tagList(
                do.call(
                    tags$ul,
                    lapply(
                        local$proj_header_content$loci_description, 
                        function(item) tags$li(tags$code(item))
                    )
                )
            )
        } else {
            NULL
        }
    })
    
}

#' Training set sub-module ui
#' @keywords internal
#' @author Ghislain Durif
training_set_def_ui <- function(id) {
    ns <- NS(id)
    tagList(
        hist_model_panel_ui(ns("hist_model_panel")),
        br(),
        hr(),
        prior_cond_set_ui(ns("prior_cond")),
        br(),
        hr(),
        locus_setup_ui(ns("locus_setup")),
        br(),
        br(),
        actionBttn(
            ns("validate"),
            label = "Validate",
            style = "fill",
            block = TRUE
        ),
        br(),
        hr()
    )
}

#' Training set sub-module server
#' @keywords internal
#' @author Ghislain Durif
training_set_def_server <- function(input, output, session, 
                                    data_file = reactive({NULL}),
                                    data_info = reactive({NULL}),
                                    locus_type = reactive({NULL}),
                                    seq_mode = reactive({NULL}),
                                    proj_dir = reactive({NULL}),
                                    raw_scenario_list = reactive({NULL}),
                                    valid_proj = reactive({TRUE})) {
    
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        cond_list = list(),
        param_list = list(),
        param_count_list = list(),
        raw_cond_list = list(),
        raw_param_list = list(),
        data_file = NULL,
        data_info = NULL,
        locus_type = NULL,
        seq_mode = NULL,
        proj_dir = NULL,
        raw_scenario_list = NULL,
        scenario_list = NULL,
        valid_proj = NULL
    )
    # get input
    observe({
        local$data_file <- data_file()
        local$data_info <- data_info()
        local$locus_type <- locus_type()
        local$seq_mode <- seq_mode()
        local$proj_dir <- proj_dir()
        local$raw_scenario_list <- raw_scenario_list()
        local$valid_proj <- valid_proj()
    })
    
    # init output
    out <- reactiveValues(
        cond_list = list(),
        param_list = list(),
        raw_param_list = list(),
        trigger = NULL,
        valid_def = FALSE
    )
    
    ## historic model panel
    hist_model_def <- callModule(
        hist_model_panel_server, "hist_model_panel",
        project_dir = reactive(local$proj_dir), 
        raw_scenario_list = reactive(local$raw_scenario_list)
    )
    
    # # debugging
    # observe({
    #     print("hist_model_def")
    #     print(hist_model_def$scenario_list)
    # })
    
    # update local
    observe({
        local$scenario_list <- hist_model_def$scenario_list
        # extract input
        if(length(local$scenario_list) > 0) {
            # param list
            local$param_list <- Reduce("c", lapply(
                local$scenario_list,
                function(item) {
                    c(lapply(
                        item$param$Ne_param,
                        function(subitem) return(list(type = "N", 
                                                      name = subitem))
                    ),
                    lapply(
                        item$param$time_param,
                        function(subitem) return(list(type = "T", 
                                                      name = subitem))
                    ),
                    lapply(
                        item$param$rate_param,
                        function(subitem) return(list(type = "A", 
                                                      name = subitem))
                    ))
                }
            ))
            
            # print("--- param list")
            # print(local$param_list)
            
            # param count list
            local$param_count_list <- lapply(
                local$scenario_list,
                function(item) {
                    # print(item$param)
                    return(length(item$param$Ne_param) + 
                               length(item$param$time) + 
                               length(item$param$rate))
                }
            )
            
            # print("--- param count list")
            # print(local$param_count_list)
            
            # print("----")
            # condition
            local$cond_list <- Reduce("c", lapply(
                local$scenario_list,
                function(item) return(item$cond)
            ))
            # raw scenario
            local$raw_scenario_list <- Reduce("c", lapply(
                local$scenario_list,
                function(item) return(item$raw)
            ))
        }
    })
    
    ## parameter prior and conditon setting
    prior_cond_set <- callModule(
        prior_cond_set_server, "prior_cond",
        cond_list = reactive(local$cond_list),
        param_list = reactive(local$param_list)
    )
    
    # update param list
    observe({
        req(!is.null(prior_cond_set$raw_prior_list))
        req(!is.null(prior_cond_set$raw_cond_list))
        # print(prior_cond_set$raw_prior_list)
        local$raw_param_list <- unique(prior_cond_set$raw_prior_list)
        local$raw_cond_list <- prior_cond_set$raw_cond_list
    })
    
    ## locus setup
    locus_setup <- callModule(
        locus_setup_server, "locus_setup",
        data_info = reactive(local$data_info),
        locus_type = reactive(local$locus_type),
        seq_mode = reactive(local$seq_mode)
    )
    
    ## write header file (if necessary) and check it
    observeEvent(input$validate, {
        
        # print("proj_dir =")
        # print(local$proj_dir)
        # print("raw_param_list =")
        # print(local$raw_param_list)
        # print("param_count_list =")
        # print(local$param_count_list)
        # print("scenario_list =")
        # print(local$raw_scenario_list)
        # print("cond_list =")
        # print(local$raw_cond_list)
        # print("data_file =")
        # print(local$data_file)
        # print("locus_type =")
        # print(local$locus_type)
        # print("seq_mode =")
        # print(local$seq_mode)
        # print("locus =")
        # print(locus_setup$locus)
        
        req(!is.null(local$proj_dir))
        req(!is.null(local$data_file))
        req(!is.null(local$raw_scenario_list))
        req(!is.null(local$param_count_list))
        req(!is.null(local$raw_param_list))
        req(!is.null(local$raw_cond_list))
        req(!is.null(local$locus_type))
        req(!is.null(local$seq_mode))
        req(!is.null(locus_setup$locus))
        
        write_header(local$proj_dir, local$data_file, 
                     local$raw_scenario_list, local$param_count_list, 
                     unlist(local$raw_param_list), 
                     unlist(local$raw_cond_list), 
                     local$locus_type, local$seq_mode, locus_setup$locus)
        
        file_check <- parse_diyabc_header(
            file_name = file.path(local$proj_dir, "header.txt"),
            file_type = "text/plain",
            data_type = local$locus_type
        )
        
        # # debugging
        # print("is header ok")
        # print(file_check$valid)
        
        out$valid_def <- file_check$valid
        
        if(out$valid_def) {
            showNotification(
                id = ns("headerfile_ok"),
                duration = 5,
                closeButton = TRUE,
                type = "message",
                tagList(
                    tags$p(
                        icon("check"),
                        paste0("Project is ready to run simulations.")
                    )
                )
            )
        } else {
            showNotification(
                id = ns("headerfile_ok"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"),
                        paste0("Project is not ready to run simulations.")
                    )
                )
            )
        }
        
        out$trigger <- ifelse(!is.null(out$trigger), out$trigger, 0) + 1
    })
    

    # get output
    observe({
        out$cond_list = local$cond_list
        out$param_list = local$param_list
        out$raw_param_list = local$raw_param_list
    })
    ## output
    return(out)
}

#' Historical model input panel module ui
#' @keywords internal
#' @author Ghislain Durif
hist_model_panel_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("history"), "Define historical models"),
        br(),
        fluidRow(
            column(
                width = 2,
                actionButton(
                    ns("add"), 
                    label = tags$span(icon("plus"), "Add")
                )
            ),
            column(
                width = 4,
                uiOutput(ns("scenario_nb"))
            )
        ),
        br(),
        tabsetPanel(id = ns("scenario_tabs"))
    )
}

#' Historical model input panel module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyjs onclick
#' @importFrom shinyWidgets actionBttn
#' @importFrom stringr str_c
hist_model_panel_server <- function(input, output, session,
                                    project_dir = reactive({NULL}), 
                                    raw_scenario_list = reactive({NULL})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        count = NULL,
        project_dir = NULL,
        raw_scenario_list = NULL,
        scenario_list = list(),
        scenario_nb = 0
    )
    # get input
    observe({
        local$project_dir <- project_dir()
        # FIXME raw_scenario_list
    })
    # init output
    out <- reactiveValues(scenario_list = NULL)
    
    # add scenario
    observeEvent(input$add, {
        # increment count
        local$count <- ifelse(is.null(local$count), 0, local$count) + 1
        local$scenario_nb <- ifelse(is.null(local$scenario_nb), 
                                    0, local$scenario_nb) + 1
        # id
        id <- local$count
        # add new tab
        appendTab(
            inputId = "scenario_tabs",
            tabPanel(
                title = closable_tab_title(id, 
                                           label = str_c("Scenario ", id),
                                           ns = ns),
                value = id,
                tags$br(),
                hist_model_ui(ns(str_c("model", id)))
            ),
            select = TRUE
        )
        # observe closing
        observe({
            shinyjs::onclick(id = str_c("close", id), {
                local$scenario_nb <<- ifelse(!is.null(local$scenario_nb), 
                                             local$scenario_nb - 1, 0)
                removeTab(inputId = "scenario_tabs", target = str_c(id))
                local$scenario_list[[ id ]] <<- NULL
            })
        })
        # server function
        local$scenario_list[[ id ]] <<- callModule(
            hist_model_server, str_c("model", id),
            project_dir = reactive(local$project_dir), 
            raw_scenario = reactive(local$raw_scenario)
        )
    })
    
    # update scenario number
    output$scenario_nb <- renderUI({
        helpText(
            str_c("Current number of scenarii = ", local$scenario_nb)
        )
    })
    
    # update output
    observe({
        out$scenario_list <- local$scenario_list
        # if(length(local$scenario_list) > 0) {
        #     print(local$scenario_list[[1]]$raw)
        #     print(local$scenario_list[[1]]$param)
        # }
    })
    
    ## output
    return(out)
}

#' Return closable panel title
#' @keywords internal
#' @author Ghislain Durif
closable_tab_title <- function(id, label, ns) {
    tags$span(
        label,
        HTML("&nbsp;"),
        tags$span(
            actionButton(
                ns(str_c("close", id)), 
                label = NULL,
                icon = icon("close")
            )
        )
    )
}

#' Parameter prior and condition setting module ui
#' @keywords internal
#' @author Ghislain Durif
prior_cond_set_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("chart-bar"), "Priors and conditions"),
        uiOutput(ns("param_prior_def")),
        hr(),
        h4("Condition setting"),
        textAreaInput(
            ns("cond_set"), 
            label = NULL, 
            rows = 12,
            resize = "none"
        ),
        helpText(
            "Enter a single condition per line.",
            "Conditions should have the following format: XX<YY.", 
            "where 'XX' and 'YY' are parameters of the same type.",
            "You can use the standard comparison signs: '>', '>=', '<', '=<'."
        ),
        uiOutput(ns("cond_help")),
        uiOutput(ns("cond_format"))
    )
}

#' Parameter prior and condition setting module server
#' @keywords internal
#' @author Ghislain Durif
#' @param cond_list list of conditions on parameters, as a `reactive`.
#' @param param_list list of scenario parameters, as a `reactive`.
#' @importFrom stringr str_c
prior_cond_set_server <- function(input, output, session, 
                                  cond_list = reactive({list()}),
                                  param_list = reactive({list()})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        cond_list = NULL,
        cond_check = NULL,
        param_list = NULL,
        prior_list = NULL
    )
    # get input
    observe({
        local$cond_list <- cond_list()
        local$param_list <- unique(param_list())
    })
    # init output
    out <- reactiveValues(
        raw_prior_list = list(),
        raw_cond_list = list()
    )
    
    # # debugging
    # observe({
    #     print("param list")
    #     print(unique(local$param_list))
    # })
    
    # render ui param prior setting
    observeEvent(local$param_list, {
        output$param_prior_def <- renderUI({
            tag_list <- lapply(
                local$param_list,
                function(item) return(prior_ui(ns(str_c("prior_", item$name))))
            )
            do.call(tagList, tag_list)
        })
    })
    # server side
    observe({
        local$prior_list <<- lapply(
            local$param_list,
            function(item) {
                req(item$name)
                req(item$type)
                callModule(prior_server, str_c("prior_", item$name),
                           param_name = reactive({item$name}),
                           param_type = reactive({item$type}))
            }
        )
    })
    observe({
        out$raw_prior_list <- unlist(lapply(
            local$prior_list,
            function(item) return(item$raw)
        ))
        # print(out$raw_prior_list)
    })
    # condition help
    output$cond_help <- renderUI({
        req(!is.null(local$cond_list))
        helpText(
            "We recommend that you enter the following conditions:",
            do.call(
                tags$ul,
                lapply(unique(local$cond_list), function(item) {
                    return(tags$li(item))
                })
            )
        )
    })
    # get input condition
    observeEvent(input$cond_set, {
        req(!is.null(input$cond_set))
        req(!is.null(local$param_list))
        # FIXME
        # input (remove last empty line)
        # logging("input cond set = ", input$cond_set)
        if(str_length(input$cond_set) > 0) {
            input_cond_list <- str_split(
                str_replace(
                    string = input$cond_set,
                    pattern = "\\n$",
                    replacement = ""
                ), 
                pattern = "\\n"
            )
            # check
            local$cond_check <- check_cond(input_cond_list, local$param_list)
            # output possible issues
            output$cond_format <- renderUI({
                req(!is.null(local$cond_check$valid))
                req(!is.null(local$cond_check$msg))
                # print(local$cond_check)
                if(!local$cond_check$valid) {
                    helpText(
                        tags$span(icon("warning"), 
                                  "Issues with your input conditions:"),
                        do.call(
                            tags$ul,
                            lapply(local$cond_check$msg, function(item) {
                                return(tags$li(item))
                            })
                        )
                    )
                } else {
                    tagList()
                }
            })
            # output
            if(local$cond_check$valid) {
                out$raw_cond_list <- input_cond_list
            } else {
                out$raw_cond_list <- list()
            }
        } else {
            out$raw_cond_list <- list()
        }
    })
    
    ## output
    return(out)
}

#' Check condition provided by users
#' @keywords internal
#' @author Ghislain Durif
check_cond <- function(input_cond_list, param_list) {
    # init output
    msg <- NULL
    valid <- TRUE
    # check if input
    if(length(input_cond_list) > 0 & length(param_list) > 0) {
        # check condition formatting
        format_check <- str_detect(
            string = unlist(input_cond_list),
            pattern = str_c("^", single_param_regex(), "(<|=<|>|>=)",
                            single_param_regex(),  "$")
        )
        if(!all(format_check)) {
            msg <- append(
                msg, 
                str_c(
                    "Syntax issue with input conditions of following line(s):", 
                     str_c(which(!format_check), collapse = " "), 
                    sep = " "
                )
            )
            valid <- FALSE
        }
        # concerned parameters
        input_param <- str_extract_all(
            string = unlist(input_cond_list),
            pattern = single_param_regex()
        )
        # check for duplicate
        # FIXME
        if(length(unique(input_param)) < length(input_param)) {
            logging("check for duplicate")
            msg <- append(
                msg,
                "Possible duplicated conditions."
            )
            valid <- FALSE
        }
        # check for parameter validity
        # FIXME
    }
    # output
    return(lst(msg, valid))
}

#' Prior choice module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyWidgets radioGroupButtons
prior_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(
                width = 1,
                textOutput(
                    ns("param_name")
                )
            ),
            column(
                width = 4,
                radioGroupButtons(
                    ns("prior_type"),
                    label = NULL,
                    choices = list("Uniform" = "UN", "Log-Unif." = "LU",
                                   "Normal" = "NO", "Log-Norm." = "LN"),
                    selected = "UN",
                    justified = TRUE
                )
            ),
            column(
                width = 7,
                fluidRow(
                    column(
                        width = 3,
                        splitLayout(
                            tags$p(
                                "Min.", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            numericInput(
                                ns("min"), label = NULL,
                                value = 10, step = 0.01
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    ),
                    column(
                        width = 3,
                        splitLayout(
                            tags$p(
                                "Max.", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            numericInput(
                                ns("max"), label = NULL,
                                value = 10000, step = 0.01
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    ),
                    column(
                        width = 3,
                        splitLayout(
                            tags$p(
                                "Mean", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            numericInput(
                                ns("mean"), label = NULL,
                                value = 0, step = 0.01
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    ),
                    column(
                        width = 3,
                        splitLayout(
                            tags$p(
                                "Std. dev.", 
                                style="text-align:right;margin-right:1em;vertical-align:middle;"
                            ),
                            numericInput(
                                ns("stdev"), label = NULL,
                                value = 0, step = 0.01
                            ),
                            cellWidths = c("40%", "60%")
                        )
                    )
                )
            )
        )
    )
}

#' Prior choice module server
#' @keywords internal
#' @author Ghislain Durif
#' @param param_name parameter name as a `reactive`.
#' @param param_type parameter type as a `reactive`, `'N'` for population 
#' effective size, `'T'` for time, and `'A'` for admixture rate.
#' @importFrom shinyjs disable enable
#' @importFrom stringr str_c
prior_server <- function(input, output, session, 
                         param_name = reactive({NULL}),
                         param_type = reactive({NULL})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(param_name = NULL, param_type = NULL)
    # get input
    observe({
        local$param_name <- param_name()
        local$param_type <- param_type()
    })
    # init output
    out <- reactiveValues(raw = NULL, valid = TRUE)
    # update param name output
    output$param_name <- renderText({local$param_name})
    ## check for min/max
    observe({
        req(local$param_name)
        req(input$min)
        req(input$max)
        if(input$min >= input$max) {
            out$valid <- FALSE
            showNotification(
                id = ns("issue_min_max"),
                type = "warning",
                closeButton = TRUE,
                duration = 10,
                tags$p(
                    icon("warning"),
                    str_c(
                        "For parameter `", local$param_name, "`: ",
                        "min should be lower than max."
                    )
                )
            )
        } else {
            out$valid <- TRUE
        }
    })
    ## disable mean and stdev if uniform or log-uniform
    observeEvent(input$prior_type, {
        req(input$prior_type)
        if(input$prior_type %in% c("UN", "LU")) {
            updateNumericInput(session, "mean", value = 0)
            updateNumericInput(session, "stdev", value = 0)
            shinyjs::disable("mean")
            shinyjs::disable("stdev")
        } else {
            shinyjs::enable("mean")
            shinyjs::enable("stdev")
        }
    })
    ## check for normal and log-normal parameter setting
    observe({
        req(local$param_name)
        req(input$prior_type)
        req(is.numeric(input$min))
        req(is.numeric(input$max))
        req(is.numeric(input$mean))
        if(input$prior_type %in% c("NO", "LN")) {
            if(input$mean < input$min | input$mean > input$max) {
                out$valid <- FALSE
                showNotification(
                    id = ns("issue_norm_lognorm"),
                    type = "warning",
                    closeButton = TRUE,
                    duration = 10,
                    tags$p(
                        icon("warning"),
                        str_c(
                            "For parameter `", local$param_name, "`: ",
                            "mean should be between max and min values."
                        )
                    )
                )
            } else {
                out$valid <- TRUE
            }
        }
    })
    ## deal with admixture rate
    observeEvent(local$param_type, {
        req(local$param_type)
        if(local$param_type == "A") {
            updateNumericInput(session, "min", value = 0.001, min = 0, max = 1)
            updateNumericInput(session, "max", value = 0.999, min = 0, max = 1)
        } else {
            updateNumericInput(session, "min", value = 10, 
                               min = NULL, max = NULL)
            updateNumericInput(session, "max", value = 10000, 
                               min = NULL, max = NULL)
        }
    })
    # observe({
    #     logging("min = ", input$min)
    #     logging("max = ", input$max)
    #     logging("mean = ", input$mean)
    #     logging("stdev = ", input$stdev)
    # })
    ## encode output
    observe({
        req(local$param_name)
        req(local$param_type)
        req(input$prior_type)
        req(is.numeric(input$min))
        req(is.numeric(input$max))
        req(is.numeric(input$mean))
        req(is.numeric(input$stdev))
        out$raw <- str_c(local$param_name, " ",
                         local$param_type, " ",
                         input$prior_type, "[",
                         input$min, ",", input$max, ",",
                         input$mean, ",", input$stdev, "]")
        # logging("raw prior def = ", out$raw)
    })
    ## output
    return(out)
}

#' Locus setup ui
#' @keywords internal
#' @author Ghislain Durif
locus_setup_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("dna"), "Locus description"),
        uiOutput(ns("setup"))
    )
}

#' Locus setup server
#' @keywords internal
#' @author Ghislain Durif
locus_setup_server <- function(input, output, session, 
                               data_info = reactive({NULL}),
                               locus_type = reactive({NULL}),
                               seq_mode = reactive({NULL})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        data_info = NULL,
        locus_type = NULL,
        seq_mode = NULL
    )
    # get input
    observe({
        local$data_info <- data_info()
        local$locus_type <- locus_type()
        local$seq_mode <- seq_mode()
    })
    
    # # debugging
    # observe({
    #     print("data info")
    #     print(local$data_info)
    #     print("locus type")
    #     print(local$locus_type)
    #     print("seq mode")
    #     print(local$seq_mode)
    # })
    
    # init out
    out <- reactiveValues(
        locus = NULL
    )
    # render ui
    output$setup <- renderUI({
        
        # # debugging
        # print("locus_type")
        # print(local$locus_type)
        # print("locus")
        # print(local$data_info$locus_type)
        
        req(local$locus_type)
        req(local$seq_mode)
        req(!is.null(local$data_info$locus))
        
        if(local$locus_type == "snp") {
            tag_list <- lapply(
                local$data_info$locus,
                function(item) {
                    return(
                        fluidRow(
                            column(
                                width = 2,
                                item
                            ),
                            column(
                                width = 4,
                                numericInput(
                                    inputId = ns(str_c(
                                        "num_",
                                        str_extract(item,
                                                    pattern = "(A|H|X|Y|M)")
                                    )),
                                    label = "Number of locus",
                                    min = 0,
                                    max = as.numeric(
                                        str_extract(item,
                                                    pattern = "^[0-9]+")
                                    ),
                                    value = as.numeric(
                                        str_extract(item,
                                                    pattern = "^[0-9]+")
                                    )
                                )
                            ),
                            column(
                                width = 4,
                                numericInput(
                                    inputId = ns(str_c(
                                        str_extract(item,
                                                    pattern = "(A|H|X|Y|M)"),
                                        "_from"
                                    )),
                                    label = "from",
                                    min = 1,
                                    max = as.numeric(
                                        str_extract(item,
                                                    pattern = "^[0-9]+")
                                    ),
                                    value = 1
                                )
                            )
                        )
                    )
                }
            )
            do.call(tagList, tag_list)
        } else if(local$locus_type == "mss") {
            # FIXME
            warning("not supported at the moment")
        }
    })
    
    ## update output
    # FIXME
    observe({
        out$locus <- lapply(
            local$data_info$locus,
            function(item) {
                locus <- str_extract(item,
                                    pattern = "(A|H|X|Y|M)")
                n_loci <- input[[ str_c("num_", locus) ]]
                start_loci <- input[[ str_c(locus, "_from") ]]
                return(str_c(
                    n_loci,
                    str_c("<", locus, ">"),
                    "G1",
                    "from", start_loci,
                    sep = " "
                ))
            }
        )
        # # debugging
        # print("locus setup")
        # print(out$locus)
    })
    
    ## output
    return(out)
    
}

#' Training set simulation action module ui
#' @keywords internal
#' @author Ghislain Durif
training_set_action_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3(icon("gear"), "Run"),
        numericInput(
            ns("nrun"),
            label = "Number of simulation",
            value = 100,
            min = 0
        ),
        helpText(
            tags$div(
                icon("warning"),
                "The number of simulations depends on your analysis:"
            ),
            tags$ul(
                tags$li(
                    "1000 to 20000 simulations per scenario are needed",
                    "for" , tags$b("model choice"), "."
                ),
                tags$li(
                    "1000 to 100000 simulations under the scenario",
                    "of interest are needed for",
                    tags$b("parameter estimation"), "."
                )
            )
        ),
        hr(),
        checkboxInput(
            ns("prior_mod_check"),
            label = "Run prior and scenario checking",
            value = FALSE
        ),
        hr(),
        actionBttn(
            inputId = ns("simulate"),
            label = "Simulate",
            style = "fill",
            block = TRUE
        )
    )
}

#' Training set simulation action module server
#' @keywords internal
#' @author Ghislain Durif
#' @param project_dir project directory as a `reactive`.
#' @param project_name project name as a `reactive`.
#' @param validation boolean indicating if the project setting are validated, 
#' as a `reactive`.
#' @param raw_param scenario parameter raw setting as a `reactive`. 
#' @param raw_scenario list of raw scenario as a `reactive`.
training_set_action_server <- function(input, output, session,
                                       proj_dir = reactive({NULL}),
                                       valid = reactive({FALSE})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        proj_file_list = NULL,
        valid = NULL,
        proj_dir = NULL
    )
    # get input
    observe({
        local$proj_dir <- proj_dir()
        local$valid <- valid()
    })
    
    # project file list
    observeEvent(local$proj_dir, {
        req(!is.null(local$proj_dir))
        local$proj_file_list <- list.files(local$proj_dir)
    })
    
    # # debugging
    # observe({
    #     print("Training set action input")
    #     print(local$proj_dir)
    #     print(local$valid)
    # })
    
    # init output
    out <- reactiveValues()
    
    # deactivate simulate if not valid
    observeEvent(local$valid, {
        req(!is.null(local$valid))
        if(local$valid) {
            shinyjs::enable("simulate")
        } else {
            shinyjs::disable("simulate")
        }
    })
    
    ## run simulation
    observeEvent(input$simulate, {
        
        req(input$nrun)
        req(local$proj_dir)
        req(!is.null(input$prior_mod_check))
        
        # # debugging
        # print("check options")
        # print(getOption("diyabcGUI"))
        # print(getOption("shiny.maxRequestSize"))
        
        # logging("Running simulation")
        cmd_out <- diyabc_run_trainset_simu(
            local$proj_dir, 
            as.integer(input$nrun),
            input$prior_mod_check
        )
        
        # check run
        if(cmd_out$run_check == 0) {
            showNotification(
                id = ns("run_ok"),
                duration = 5,
                closeButton = TRUE,
                type = "message",
                tagList(
                    tags$p(
                        icon("check"),
                        "Simulations are done."
                    )
                )
            )
        } else {
            showNotification(
                id = ns("run_not_ok"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"),
                        "A problem during simulations."
                    )
                )
            )
        }
    })
}
