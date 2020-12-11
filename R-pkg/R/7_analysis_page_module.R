#' Analysis page ui
#' @keywords internal
#' @author Ghislain Durif
analysis_page_ui <- function(id) {
    ns <- NS(id)
    tagList(
        tags$style(HTML(".box-header{text-align: center;}")),
        fluidRow(
            box(
                title = tags$b("Project settings"),
                width = 12,
                status = "primary", solidHeader = FALSE,
                collapsible = FALSE,
                analysis_proj_set_ui(ns("proj_set"))
            ),
            box(
                title = "Training set simulations",
                width = 12,
                status = "info", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                training_set_ui(ns("train_set"))
            ),
            box(
                title = "Random Forest Analyses",
                width = 12,
                status = "warning", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                rf_module_ui(ns("rf"))
            ),
            box(
                title = tags$b("Project housekeeping"),
                width = 12,
                status = "danger", solidHeader = FALSE,
                collapsible = FALSE, collapsed = FALSE,
                proj_action_ui(ns("proj_action"))
            )
        )
    )
}

#' Analysis page server
#' @keywords internal
#' @author Ghislain Durif
analysis_page_server <- function(input, output, session) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        locus_type = NULL,
        seq_mode = NULL,
        new_proj = NULL,
        proj_dir = NULL,
        proj_header = NULL,
        scenario_list = NULL
    )
    # init output
    out <- reactiveValues(
        setting = NULL,
        scenario = NULL,
        reset = NULL
    )
    ## project setting
    proj_set <- callModule(analysis_proj_set_server, "proj_set")
    # # output: 
    # data_file = NULL,
    # data_info = NULL,
    # locus_type = NULL,
    # seq_mode = NULL,
    # new_proj = NULL,
    # proj_dir = mk_proj_dir(),
    # proj_file_list = character(0),
    # proj_header_content = list(),
    # proj_name = NULL,
    # valid_data_file = FALSE
    # valid_proj = FALSEvalid_proj = FALSE
    
    # # debugging
    # observe({
    #     pprint("#### Project settings ####")
    #     pprint(reactiveValuesToList(proj_set))
    # })
    
    
    # FIXME
    # update local
    observe({
        # FIXME obsolete
        local$proj_dir <- proj_set$proj_dir
        local$proj_name <- proj_set$proj_name
    })
    
    ## Training set sub-module
    training_set <- callModule(
        training_set_server, "train_set",
        data_file = reactive(proj_set$data_file),
        data_info = reactive(proj_set$data_info),
        locus_type = reactive(proj_set$locus_type),
        seq_mode = reactive(proj_set$seq_mode),
        new_proj = reactive(proj_set$new_proj),
        proj_dir = reactive(proj_set$proj_dir),
        proj_file_list = reactive({NULL}), 
        valid_proj = reactive(proj_set$valid_proj)
    )
    
    # # debugging
    # observe({
    #     pprint("training set valid proj")
    #     pprint(training_set$valid_proj)
    # })
    
    ## random forest module
    rf <- callModule(
        rf_module_server, "rf",
        locus_type = reactive(proj_set$locus_type),
        proj_dir = reactive(proj_set$proj_dir),
        # proj_file_list = reactive(proj_set$proj_file_list),
        valid_proj = reactive(proj_set$valid_proj)
    )
    
    ## action
    proj_action <- callModule(
        proj_action_server, "proj_action",
        proj_dir = reactive(proj_set$proj_dir),
        proj_name = reactive(proj_set$proj_name)
    )
    
    ## reset
    observeEvent(proj_action$reset, {
        req(proj_action$reset)
        out$reset <- proj_action$reset
        # session$reload()
    })
    
    # output
    return(out)
}

#' Analysis project setting ui
#' @keywords internal
#' @author Ghislain Durif
analysis_proj_set_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3("Project name"),
        textInput(ns("proj_name"), label = NULL, placeholder = "project name"),
        hr(),
        h3("Data type"),
        data_type_ui(ns("data_type")),
        hr(),
        h3("Project type"),
        helpText(
            "You can either: (i) start with a new project;", 
            "(ii) open one of your own an existing project;", 
            "or (iii) open one of the joined examples."
        ),
        radioGroupButtons(
            ns("proj_type"),
            label = NULL,
            choices = c("New project" = "new", 
                        "Existing project" = "existing", 
                        "Example" = "example"),
            selected = "new",
            justified = TRUE
        ),
        conditionalPanel(
            condition = "input.proj_type == 'existing'",
            ns = ns,
            h4(tags$b("Project files")),
            helpText(
                "Use ctrl+click to select more than one file."
            ),
            fileInput(
                ns("file_input"),
                label = NULL,
                buttonLabel = "Select file(s)", 
                multiple = TRUE,
                accept = c(
                    ".txt",
                    ".bin"
                )
            ),
            uiOutput(ns("file_check")),
            helpText(
                "If you do not provide some of the files, or if some files",
                "have formating issues (identified with a", icon("times"),
                ")", "you will be able to (re)configure the corresponding", 
                "settings below."
            )
        ),
        conditionalPanel(
            condition = "input.proj_type == 'example'",
            ns = ns,
            h4(tags$b("Select an example")),
            selectInput(
                ns("proj_example"),
                label = NULL,
                choices = c("", "Not available at the moment"),
                selected = NULL,
                multiple = FALSE
            ),
        ),
        hr(),
        h3("Data file"),
        conditionalPanel(
            condition = "input.proj_type !== 'example'",
            ns = ns,
            input_data_ui(ns("input_data_file"))
        ),
        check_data_ui(ns("check_data_file")),
        hr(),
        uiOutput(ns("proj_is_ready"))
    )
}

#' Analysis project setting ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom dplyr distinct
#' @importFrom fs file_copy file_delete
analysis_proj_set_server <- function(input, output, session) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        data_file = NULL,
        file_input = NULL,
        # data.frame with 4 columns:
        # name (chr), size (int), type (chr), datapath (chr)
        header_data_file = NULL
    )
    # init output
    out <- reactiveValues(
        data_file = NULL,
        data_info = NULL,
        locus_type = NULL,
        seq_mode = NULL,
        new_proj = TRUE,
        proj_dir = mk_proj_dir(),
        proj_file_list = character(0),
        proj_header_content = list(),
        proj_name = NULL,
        valid_data_file = FALSE,
        valid_proj = FALSE
    )
    
    # clean on exit
    session$onSessionEnded(function() {
        isolate(tryCatch(fs::dir_delete(out$proj_dir)))
    })
    
    ## project name
    observeEvent(input$proj_name, {
        req(input$proj_name)
        out$proj_name <- input$proj_name
    })
    
    ## data type
    data_type <- callModule(data_type_server, "data_type")
    observe({
        req(data_type$locus_type)
        req(data_type$seq_mode)
        out$locus_type <- data_type$locus_type
        out$seq_mode <- data_type$seq_mode
    })
    
    ## new or existing project
    observe({
        req(input$proj_type)
        if(input$proj_type == "new") {
            out$new_proj <- TRUE
        } else if(input$proj_type == "existing") {
            out$new_proj <- FALSE
            req(!is.null(local$local$proj_file_list))
            if("headerRF.txt" %in% local$proj_file_list) {
                out$new_proj <- FALSE
            } else {
                out$new_proj <- TRUE
            }
        } else if(input$proj_type == "example") {
            out$new_proj <- FALSE
        }
    })
    
    ## Manage existing project
    possible_files <- c("headerRF.txt", "reftableRF.bin", "statobsRF.txt")
    # copy uploaded files to project working directory (server-side)
    observeEvent(input$file_input, {
        ## user file input
        req(input$file_input)
        # data.frame with 4 columns:
        # name (chr), size (int), type (chr), datapath (chr)
        req(is.data.frame(input$file_input))
        req(nrow(input$file_input) > 0)
        
        ## extraction
        new_file_input <- input$file_input
        
        ## filename check
        new_file_input$valid <- (new_file_input$name %in% possible_files)
        
        # # debugging
        # pprint("new file input")
        # pprint(new_file_input)
        
        ## delete non related files
        lapply(
            split(new_file_input, seq(nrow(new_file_input))),
            function(item) {
                if(!item$valid) {
                    if(file.exists(item$datapath)) {
                        # logging("deleting:", item$datapath)
                        fs::file_delete(item$datapath)
                    }
                }
            }
        )
        new_file_input <- new_file_input[new_file_input$valid,]
        
        # # debugging
        # pprint("new file input")
        # pprint(new_file_input)
        
        ## copy files to project directory
        if(nrow(new_file_input) > 0) {
            lapply(
                split(new_file_input, seq(nrow(new_file_input))),
                function(item) {
                    fs::file_copy(item$datapath,
                                  file.path(out$proj_dir, item$name),
                                  overwrite = TRUE)
                    if(file.exists(item$datapath)) {
                        # logging("deleting:", item$datapath)
                        fs::file_delete(item$datapath)
                    }
                }
            )
            new_file_input$datapath <- file.path(
                out$proj_dir, 
                new_file_input$name
            )
            
            ## update file input list
            if(is.null(local$file_input)) {
                local$file_input <- new_file_input
            } else {
                local$file_input <- rbind(
                    local$file_input[!local$file_input$name 
                                     %in% new_file_input$name,],
                    new_file_input
                )
            }
        }
    })
    
    # pprint possible files when uploading existing projects
    output$file_check <- renderUI({
        helpText(
            tags$p(
                "Project-related files can be:", 
                tags$div(
                    style = "column-count:2;",
                    do.call(
                        tags$ul, 
                        lapply(
                            possible_files,
                            function(item) tags$li(
                                if(item %in% local$file_input$name) {
                                    ind <- which(item == local$file_input$name)
                                    if(local$file_input$valid[ind]) {
                                        tags$div(
                                            item,
                                            icon("check")
                                        )
                                    } else {
                                        tags$div(
                                            item,
                                            icon("times")
                                        )
                                    }
                                } else {
                                    item
                                }
                            )
                        )
                    )
                )
            )
        )
    })
    
    # debugging
    observe({
        logging("project directory:", out$proj_dir)
    })
    
    ## Manage example project
    # update possible input
    observe({
        req(!is.null(data_type$locus_type))
        req(!is.null(data_type$seq_mode))
        
        if(data_type$locus_type == "mss") {
            updateSelectInput(
                session, 
                "proj_example", 
                choices = c("", "Not available at the moment"),
                selected = NULL
            )
        } else if(data_type$locus_type == "snp" & 
                  data_type$seq_mode == "indseq") {
            possible_choices <- basename(
                list.dirs(
                    example_dir()
                )
            )
            possible_choices <- possible_choices[str_detect(possible_choices,
                                                            "IndSeq")]
            updateSelectInput(
                session, 
                "proj_example", 
                choices = c("", possible_choices),
                selected = NULL
            )
        } else if(data_type$locus_type == "snp" & 
                 data_type$seq_mode == "poolseq") {
            possible_choices <- basename(
                list.dirs(
                    example_dir()
                )
            )
            possible_choices <- possible_choices[str_detect(possible_choices,
                                                            "PoolSeq")]
            updateSelectInput(
                session, 
                "proj_example", 
                choices = c("", possible_choices),
                selected = NULL
            )
        }
    })
    # copy files if required
    observeEvent(input$proj_example, {
        
        req(input$proj_type == "example")
        req(input$proj_example)
        
        # copy files
        proj_files <- list.files(
            file.path(
                example_dir(),
                input$proj_example
            )
        )
        fs::file_copy(
            path = file.path(
                example_dir(),
                input$proj_example,
                proj_files
            ),
            new_path = file.path(
                out$proj_dir,
                proj_files
            ),
            overwrite = TRUE
        )
        
        # update file input
        # data.frame with 4 columns:
        # name (chr), size (int), type (chr), datapath (chr)
        local$file_input <- data.frame(
            name = proj_files,
            size = rep(NA, length(proj_files)),
            type = rep(NA, length(proj_files)),
            datapath = file.path(
                out$proj_dir,
                proj_files
            ),
            valid = rep(TRUE, length(proj_files))
        )
        
        ## file type
        ind <- which(local$file_input$name == "headerRF.txt")
        local$file_input$type[ind] <- "text/plain"
        ind <- which(local$file_input$name == "reftableRF.bin")
        local$file_input$type[ind] <- "application/octet-stream"
        ind <- which(local$file_input$name == "statObsRF.txt")
        local$file_input$type[ind] <- "text/plain"
    })
    
    ## check current project header file
    observeEvent(local$file_input, {
        
        req(is.data.frame(local$file_input))
        req(nrow(local$file_input) > 0)
        req(!is.null(input$proj_type))
        
        ## header check
        if("headerRF.txt" %in% local$file_input$name) {
            ind <- which(local$file_input$name == "headerRF.txt")
            header_file_content <- parse_diyabc_header(
                file_name = local$file_input$datapath[ind],
                file_type = local$file_input$type[ind],
                locus_type = data_type$locus_type
            )
            # valid header file
            local$file_input$valid[ind] <- header_file_content$valid
            # header data file name
            local$header_data_file <- header_file_content$data_file
            # header data file content
            out$proj_header_content <- header_file_content
            # data from example ?
            if(input$proj_type == "example") {
                local$data_file <- header_file_content$data_file
            }
        }

        ## delete non valid files
        lapply(
            split(local$file_input, seq(nrow(local$file_input))),
            function(item) {
                if(!item$valid) {
                    if(file.exists(item$datapath)) {
                        logging("deleting:", item$datapath)
                        fs::file_delete(item$datapath)
                    }
                }
            }
        )

        ## project file list
        out$proj_file_list <- local$file_input$name[local$file_input$valid]

        # # debugging
        # pprint("file_input")
        # pprint(local$file_input)
    })

    ## Data file file
    data_file <- callModule(input_data_server, "input_data_file",
                            proj_dir = reactive(out$proj_dir))
    
    # update local
    observe({
        req(!is.null(data_file$name))
        local$data_file <- data_file$name
    })
    
    ## Data file check
    check_data <- callModule(
        check_data_server, "check_data_file",
        data_file = reactive(local$data_file),
        expected_data_file = reactive(local$header_data_file),
        locus_type = reactive(out$locus_type),
        seq_mode = reactive(out$seq_mode),
        proj_dir = reactive(out$proj_dir)
    )
    
    # update output
    observe({
        out$data_file <- data_file$name
        out$data_info <- check_data$info
        out$valid_data_file <- check_data$valid
    })
    
    # valid set up ?
    observe({
        
        req(!is.null(out$valid_data_file))
        
        # check header if required
        valid_header <- TRUE
        if(!is.null(out$proj_header_content$valid)) {
            valid_header <- out$proj_header_content$valid
        }
        
        out$valid_proj <- valid_header & out$valid_data_file
        
        # # debugging
        # logging("valid proj?", out$valid_proj)
    })
    
    output$proj_is_ready <- renderUI({
        if(!(out$valid_proj & out$valid_data_file)) {
            h3(icon("warning"), "Project set up is not ready.", 
               style="color:red;text-align:center;")
        } else {
            h4(icon("check"), "Project set up is ok.",
               style="text-align:center;")
        }
    })
    
    
    ## output
    return(out)
}

#' Input data ui
#' @keywords internal
#' @author Ghislain Durif
input_data_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fileInput(
            ns("data_file"),
            label = NULL, 
            buttonLabel = "Select file",
            multiple = FALSE
        )
    )
}

#' Input data server
#' @keywords internal
#' @author Ghislain Durif
#' @param proj_dir string as a `reactive`, project directory.
input_data_server <- function(input, output, session,
                              proj_dir = reactive({NULL})) {
    ## init local
    local <- reactiveValues(
        # input
        proj_dir = NULL
    )
    ## get input
    observe({
        local$proj_dir <- proj_dir()
        # # debugging
        # pprint(paste0("input proj dir = ", local$proj_dir))
    })
    ## init output
    out <- reactiveValues(
        name = NULL
    )
    ## get data file
    observeEvent(input$data_file, {
        # input$data_file = data.frame with 4 columns:
        # name (chr), size (int), type (chr), datapath (chr)
        req(local$proj_dir)
        req(input$data_file)
        req(is.data.frame(input$data_file))
        req(nrow(input$data_file) > 0)
        # data file
        out$name <- input$data_file$name
        # copy to project directory
        fs::file_copy(input$data_file$datapath,
                      file.path(local$proj_dir, out$name),
                      overwrite = TRUE)
        
        if(file.exists(input$data_file$datapath)) {
            # logging("deleting:", input$data_file$datapath)
            fs::file_delete(input$data_file$datapath)
        }
    })
    # # debugging
    # observe({
    #     logging("data file = ", out$file)
    # })
    
    ## output
    return(out)
}

#' Check data ui
#' @keywords internal
#' @author Ghislain Durif
check_data_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("missing_file")),
        uiOutput(ns("data_info"))
    )
}

#' check data server
#' @keywords internal
#' @author Ghislain Durif
#' @param data_file string as a `reactive`, data file uploaded by the user.
#' @param expected_data_file string as a `reactive`, expected data file if a 
#' header file is provided (NULL otherwise).
#' @param locus_type string as a `reactive`, `"mss"` or `"snp"`.
#' @param seq_mode string as a `reactive`, `"indseq"` or `"poolseq"`.
#' @param proj_dir string as a `reactive`, project directory.
check_data_server <- function(input, output, session,
                              data_file = reactive({NULL}),
                              expected_data_file = reactive({NULL}),
                              locus_type = reactive({"snp"}),
                              seq_mode = reactive({"indseq"}),
                              proj_dir = reactive({NULL})) {
    # init local
    local <- reactiveValues(
        file_check = NULL,
        data_info = NULL,
        # input
        data_file = NULL,
        exp_data_file = NULL,
        locus_type = NULL,
        seq_mode = NULL,
        proj_dir = NULL
    )
    # get input
    observe({
        local$data_file <- data_file()
        local$exp_data_file <- expected_data_file()
        local$locus_type <- locus_type()
        local$seq_mode <- seq_mode()
        local$proj_dir <- proj_dir()
        
        # # debugging
        # pprint(paste0("input data file = ", local$data_file))
        # pprint(paste0("expected data file = ", local$exp_data_file))
        # pprint(paste0("input locus type = ", local$locus_type))
        # pprint(paste0("input seq mode = ", local$seq_mode))
        # pprint(paste0("input proj dir = ", local$proj_dir))
    })
    # init output
    out <- reactiveValues(
        data_file = NULL, 
        data_info = NULL,
        valid = FALSE
    )
    # # debugging
    # observe({
    #     logging("data file = ", out$file)
    # })
    
    ## message if missing file
    output$missing_file <- renderUI({
        if(is.null(local$data_file)) {
            helpText(
                icon("warning"), "Missing data file"
            )
        } else {
            NULL
        }
    })
    
    # data check
    observe({
        req(!is.null(local$data_file))
        req(!is.null(local$proj_dir))
        req(!is.null(local$locus_type))
        req(!is.null(local$seq_mode))
        # check
        local$file_check <- check_data_file(
            local$data_file, local$proj_dir, 
            local$locus_type, local$seq_mode,
            local$exp_data_file
        )
        # data info
        req(!is.null(local$file_check))
        req(!is.null(local$file_check$valid))
        # valid data
        out$valid <- local$file_check$valid
        # data spec
        req(!is.null(local$file_check$spec))
        out$info <- local$file_check$spec
    })
    
    # user feedback
    output$data_info <- renderUI({
        req(!is.null(local$file_check))
        # show data info
        if(local$file_check$valid) {
            req(local$file_check$msg)
            helpText(
                h5("Data file info"),
                do.call(
                    tags$ul,
                    lapply(local$file_check$msg, function(item) {
                        return(tags$li(item))
                    })
                )
            )
        } else {
            tmp_msg <- NULL
            if(!is.null(local$file_check$err)) {
                tmp_msg <- do.call(
                    tags$ul,
                    lapply(local$file_check$err, function(item) {
                        return(tags$li(item))
                    })
                )
            }
            helpText(
                icon("warning"), "Issue with data file.",
                tmp_msg
            )
        }
    })
    
    # output
    return(out)
}

#' Analysis project action ui
#' @keywords internal
#' @author Ghislain Durif
proj_action_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(
                width = 6,
                downloadButton(
                    ns("save"), 
                    label = "Save",
                    style = "width:100%;"
                )
            ),
            column(
                width = 6,
                actionButton(
                    ns("reset"),
                    label = tags$span(icon("refresh"), "Reset"),
                    width = "100%"
                )
            )
        )
    )
}

#' Analysis project action server
#' @keywords internal
#' @author Ghislain Durif
proj_action_server <- function(input, output, session,
                               proj_dir = reactive({NULL}),
                               proj_name = reactive({NULL})) {
    # init local
    local <- reactiveValues(
        proj_dir = NULL,
        proj_name = NULL
    )
    # get input
    observe({
        local$proj_dir <- proj_dir()
        local$proj_name <- proj_name()
    })
    # init output
    out <- reactiveValues(reset = NULL)
    # save
    output$save <- downloadHandler(
        filename = function() {
            file_name <- "project_name.zip"
            if(!is.null(local$proj_name)) {
                if(str_length(local$proj_name) > 0) {
                    file_name <- str_c(local$proj_name, ".zip")
                }
            }
            return(file_name)
        },
        content = function(file) {
            wd <- getwd()
            on.exit(setwd(wd))
            setwd(local$proj_dir)
            cleanup_diyabc_run(local$proj_dir)
            cleanup_abcranger_run(local$proj_dir)
            zip(file, list.files(local$proj_dir))
        }
    )
    
    ## reset
    observeEvent(input$reset, {
        ask_confirmation(
            inputId = "reset_confirmation",
            title = "Want to confirm ?"
        )
    })
    observeEvent(input$reset_confirmation, {
        req(!is.null(input$reset_confirmation))
        if(isTRUE(input$reset_confirmation)) {
            out$reset <- ifelse(!is.null(out$reset), out$reset, 0) + 1
        }
    })
    
    ## output
    return(out)
}
