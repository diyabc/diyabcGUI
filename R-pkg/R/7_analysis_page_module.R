#' Analysis page ui
#' @keywords internal
#' @author Ghislain Durif
analysis_page_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "Project settings",
                width = 12,
                status = "primary", solidHeader = TRUE,
                collapsible = FALSE,
                # new_proj_set_ui(ns("proj_set")) %>% 
                #     helper(type = "markdown", 
                #            content = "analysis_project"),
                # hr(),
                # input_data_ui(ns("input_data"))
                analysis_proj_set_ui(ns("proj_set"))
            )
        ),
        fluidRow(
            box(
                title = "Training set simulations",
                width = 12,
                status = "info", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                training_set_ui(ns("train_set"))
            )
        ),
        fluidRow(
            box(
                title = "Random Forest Analyses",
                width = 12,
                status = "warning", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                rf_module_ui(ns("rf"))
            )
        )
        ,
        fluidRow(
            box(
                title = "Project action",
                width = 12,
                status = "danger", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE,
                "FILLME"
            )
        )
    )
}

#' Analysis page server
#' @keywords internal
#' @author Ghislain Durif
#' @param proj_name project name as a `reactive`.
analysis_page_server <- function(input, output, session,
                                 proj_name = reactive({NULL})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        locus_type = NULL,
        seq_mode = NULL,
        new_proj = NULL,
        proj_dir = NULL,
        proj_header = NULL,
        proj_name = NULL,
        scenario_list = NULL
    )
    # get input
    observe({
        local$proj_name = proj_name()
    })
    # init output
    out <- reactiveValues(
        setting = NULL,
        scenario = NULL
    )
    ## project setting
    proj_set <- callModule(analysis_proj_set_server, "proj_set", 
                           proj_name = reactive(local$proj_name))
    # output: 
    # data_file, data_info,
    # locus_type, seq_mode,
    # new_proj,
    # proj_dir, proj_file_list, proj_header, proj_name,
    # valid_data_file, valid_proj, valid_proj_file
    
    # # debugging
    # observe({
    #     print("#### Project settings ####")
    #     print(reactiveValuesToList(proj_set))
    # })
    
    ## write files if required
    observeEvent(proj_set$valid_proj, {
        req(!is.null(proj_set$valid_proj))
        req(proj_set$proj_dir)
        req(proj_set$proj_name)
        
        # # debugging
        # print("Writing project file (if necessary)")
        # print(proj_set$valid_proj)
        # print(proj_set$proj_dir)
        # print(proj_set$proj_name)
        # print(proj_set$proj_file_list)
        
        if(proj_set$valid_proj) {
            # write project file if not existing
            if(!"diyabcGUI_proj.txt" %in% proj_set$proj_file_list) {
                write_proj_file(proj_set$proj_dir, proj_set$proj_name)
            }
        }
    })
    
    
    ##
    
    setting <- proj_set # FIXME
    # update local
    observe({
        # FIXME obsolete
        local$proj_dir <- proj_set$proj_dir
        local$proj_name <- proj_set$proj_name
    })
    # # update output
    # observe({
    #     out$setting <- setting
    # })
    # ## input data
    # input_data <- callModule(input_data_server, "input_data")
    ## Training set sub-module
    # training_set <- callModule(training_set_server, "train_set",
    #                            project_dir = reactive(proj_set$proj_dir),
    #                            project_name = reactive(proj_set$proj_name),
    #                            data_file = reactive(proj_set$data_file),
    #                            valid_data_file = reactive(proj_set$valid),
    #                            locus_type = reactive(proj_set$locus_type),
    #                            validation = reactive(setting$validation))
    
    ## Training set sub-module
    training_set <- callModule(
        training_set_server, "train_set",
        data_info = reactive(proj_set$data_info),
        locus_type = reactive(proj_set$locus_type),
        seq_mode = reactive(proj_set$seq_mode),
        proj_dir = reactive(proj_set$proj_dir),
        proj_file_list = reactive(proj_set$proj_file_list), 
        valid_proj = reactive(proj_set$valid_proj))
    
    # output
    return(out)
}

#' Analysis project setting ui
#' @keywords internal
#' @author Ghislain Durif
analysis_proj_set_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h4(tags$b("Project type")),
        locus_type_ui(ns("locus_type")),
        hr(),
        helpText(
            "You can either start with a new project", 
            "or open an existing project (one of your own or", 
            "one of the project examples joined with the application)."
        ),
        radioGroupButtons(
            ns("proj_type"),
            label = NULL,
            choices = c("New", "Existing", "Example"),
            selected = "New",
            justified = TRUE
        ),
        conditionalPanel(
            condition = "input.proj_type == 'Existing'",
            ns = ns,
            h4(tags$b("Project files")),
            fileInput(
                ns("file_input"),
                label = NULL, 
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
            condition = "input.proj_type == 'Example'",
            ns = ns,
            selectInput(
                ns("proj_example"),
                label = "Examples",
                choices = c("", "Not available at the moment"),
                selected = NULL,
                multiple = FALSE
            ),
        ),
        hr(),
        h4(tags$b("Project name")),
        fluidRow(
            column(
                width = 10,
                textInput(
                    ns("proj_name"), 
                    label = NULL,
                    placeholder = "project_name"
                )
            ),
            column(
                width = 2,
                actionButton(
                    ns("edit_proj_name"),
                    label = "Edit",
                    icon = icon("edit"),
                    width = "100%"
                )
            )
        ),
        uiOutput(ns("proj_name_feedback")),
        hr(),
        h4(tags$b("Data file")),
        input_data_ui(ns("data_file")),
        hr(),
        uiOutput(ns("proj_is_ready"))
    )
}

#' Analysis project setting ui
#' @keywords internal
#' @author Ghislain Durif
#' @param proj_name project name as a `reactive`.
#' @param reset reactive to pass a reset command.
#' @importFrom fs file_copy file_delete
analysis_proj_set_server <- function(input, output, session, 
                                     proj_name = reactive({NULL}),
                                     reset = reactive({NULL})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        file_input = NULL,
        header_data_file = NULL,
        proj_file_check = NULL,
        proj_name = NULL,
        reset = NULL
    )
    # init output
    out <- reactiveValues(
        data_file = NULL,
        data_info = NULL,
        locus_type = NULL,
        seq_mode = NULL,
        new_proj = NULL,
        proj_dir = mk_proj_dir(),
        proj_file_list = character(0),
        proj_header = NULL,
        proj_name = NULL,
        valid_data_file = TRUE,
        valid_proj = TRUE,
        valid_proj_file = TRUE
    )
    # get input
    observe({
        local$proj_name <- proj_name()
        local$reset <- reset()
    })
    # clean on exit
    session$onSessionEnded(function() {
        isolate(tryCatch(fs::dir_delete(out$proj_dir)))
    })
    # # FIXME reset
    # observeEvent(reset, {
    #     local$proj_dir <- mk_proj_dir(session)
    # })
    # locus type
    locus_type <- callModule(locus_type_server, "locus_type")
    observe({
        req(locus_type$locus_type)
        out$locus_type <- locus_type$locus_type
        out$seq_mode <- locus_type$seq_mode
    })
    # new or existing project
    observeEvent(input$proj_type, {
        req(input$proj_type)
        if(input$proj_type == "New") {
            shinyjs::disable("edit_proj_name")
            shinyjs::enable("proj_name")
            out$new_proj <- TRUE
        } else if(input$proj_type %in% c("Existing", "Example")) {
            shinyjs::enable("edit_proj_name")
            shinyjs::disable("proj_name")
            out$new_proj <- FALSE
        }
    })
    ## Manage existing project
    possible_files <- c("diyabcGUI_proj.txt", "header.txt", "headerRF.txt", 
                        "reftableRF.bin", "statobsRF.txt")
    # check and copy uploaded files to project working directory (server-side)
    observeEvent(input$file_input, {
        req(input$file_input)
        # data.frame with 4 columns:
        # name (chr), size (int), type (chr), datapath (chr)
        req(is.data.frame(input$file_input))
        req(nrow(input$file_input) > 0)
        local$file_input <- input$file_input
        # filename check
        local$file_input$valid <- ifelse(
            local$file_input$name %in% possible_files,
            TRUE,
            FALSE
        )
        # delete non related files
        lapply(
            split(local$file_input, seq(nrow(local$file_input))),
            function(item) {
                if(!item$valid) {
                    tryCatch(fs::file_delete(item$datapath))
                }
            }
        )
        # copy files to project directory
        lapply(
            split(local$file_input, seq(nrow(local$file_input))),
            function(item) {
                if(item$valid) {
                    fs::file_copy(item$datapath,
                                  file.path(out$proj_dir, item$name),
                                  overwrite = TRUE)
                    tryCatch(fs::file_delete(item$datapath))
                }
            }
        )
        local$file_input$datapath <- file.path(
            out$proj_dir, 
            local$file_input$name
        )
        # project file check
        if("diyabcGUI_proj.txt" %in% local$file_input$name) {
            ind <- which(local$file_input$name == "diyabcGUI_proj.txt")
            proj_file <- parse_diyabc_project(
                file_name = local$file_input$datapath[ind], 
                file_type = local$file_input$type[ind]
            )
            local$file_input$valid[ind] <- proj_file$valid
            if(proj_file$valid) {
                local$proj_name <- proj_file$proj_name
            }
            # interface
            shinyjs::enable("edit_proj_name")
            shinyjs::disable("proj_name")
        } else {
            # interface
            shinyjs::disable("edit_proj_name")
            shinyjs::enable("proj_name")
        }
        # header check
        if("header.txt" %in% local$file_input$name) {
            ind <- which(local$file_input$name == "header.txt")
            header_file <- parse_diyabc_header(
                file_name = local$file_input$datapath[ind], 
                file_type = local$file_input$type[ind],
                data_type = locus_type$locus_type
            )
            local$file_input$valid[ind] <- header_file$valid
            local$header_data_file <- header_file$data_file
            out$proj_header <- header_file
        }
        # project file list
        out$proj_file_list <- local$file_input$name[local$file_input$valid]
        # valid project file
        out$valid_proj_file <- any(local$file_input$valid)
    })
    # possible files when uploading existing projects
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
    
    # # debugging
    # observe({
    #     print(local$file_input)
    # })
    
    # if existing project, allow to modify its name
    observeEvent(input$edit_proj_name, {
        req(input$edit_proj_name)
        shinyjs::enable("proj_name")
    })
    
    ## Manage example project
    # TODO
    
    ## get/set project name
    observeEvent(local$proj_name, {
        req(local$proj_name)
        updateTextInput(session, "proj_name", value = local$proj_name)
    })
    observeEvent(input$proj_name, {
        out$proj_name <- input$proj_name
    })
    output$proj_name_feedback <- renderUI({
        help_text <- helpText(
            icon("warning"), "Missing project name"
        )
        if(is.null(input$proj_name)) {
            help_text
        } else if(str_length(input$proj_name) == 0) {
            help_text
        } else {
            NULL
        }
    })
    
    ## Data file
    data_file <- callModule(input_data_server, "data_file",
                            expected_data_file = reactive(local$header_data_file),
                            locus_type = reactive(out$locus_type),
                            seq_mode = reactive(out$seq_mode),
                            proj_dir = reactive(out$proj_dir))
    
    # update output
    observe({
        out$data_file <- data_file$file
        out$data_info <- data_file$info
        out$valid_data_file <- data_file$valid
    })
    
    # valid set up ?
    observe({
        req(!is.null(out$valid_data_file))
        req(!is.null(out$valid_proj_file))
        if(!is.null(out$proj_name)) {
            out$valid_proj <- out$valid_data_file & out$valid_proj_file &
                (str_length(out$proj_name) > 0)
        } else {
            out$valid_proj <- FALSE
        }
        if(!out$valid_proj) {
            output$proj_is_ready <- renderUI({
                h2(icon("warning"), "Project set up is not ready.", 
                   style="color:red")
            })
        } else {
            output$proj_is_ready <- renderUI({
                h3(icon("check"), "Project set up is ok.")
            })
        }
    })
    
    ## output
    return(out)
}

#' Create a project directory server-side
#' @keywords internal
#' @author Ghislain Durif
mk_proj_dir <- function(tag = "diyabc") {
    # create tmp dir
    tmp_dir <- tempfile(tag)
    dir.create(tmp_dir, showWarnings = FALSE)
    # output
    return(tmp_dir)
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
            multiple = FALSE
        ),
        uiOutput(ns("missing_file")),
        # conditionalPanel(
        #     condition = "input.data_file.length > 0",
        #     ns = ns,
        #     helpText(
        #         icon("warning"), "Missing data file"
        #     )
        # ),
        uiOutput(ns("data_info")) #%>% withSpinner() # FIXME
    )
}

#' Input data server
#' @keywords internal
#' @author Ghislain Durif
#' @param expected_data_file string as a `reactive`, expected data file if a 
#' header file is provided.
#' @param locus_type string as a `reactive`, `"mss"` or `"snp"`.
#' @param seq_mode string as a `reactive`, `"indseq"` or `"poolseq"`.
#' @param proj_dir string as a `reactive`, project directory.
input_data_server <- function(input, output, session,
                              expected_data_file = reactive({NULL}),
                              locus_type = reactive({"snp"}),
                              seq_mode = reactive({"indseq"}),
                              proj_dir = reactive({NULL})) {
    # init local
    local <- reactiveValues(
        exp_data_file = NULL,
        file_check = NULL,
        locus_type = NULL,
        seq_mode = NULL,
        data_info = NULL,
        proj_dir = NULL
    )
    # get input
    observe({
        local$exp_data_file <- expected_data_file()
        local$locus_type <- locus_type()
        local$seq_mode <- seq_mode()
        local$proj_dir <- proj_dir()
        
        # # debugging
        # print(paste0("input data file = ", local$exp_data_file))
        # print(paste0("input locus type = ", local$locus_type))
        # print(paste0("input seq mode = ", local$seq_mode))
        # print(paste0("input proj dir = ", local$proj_dir))
    })
    # init output
    out <- reactiveValues(
        file = NULL, 
        data_info = NULL,
        valid = FALSE
    )
    # message if missing file
    output$missing_file <- renderUI({
        if(is.null(input$data_file)) {
            helpText(
                icon("warning"), "Missing data file"
            )
        } else {
            # if existing project ?
            if(!is.null(local$exp_data_file)) {
                req(is.data.frame(input$data_file))
                req(nrow(input$data_file) > 0)
                if(local$exp_data_file != input$data_file$name) {
                    helpText(
                        icon("warning"), 
                        "Provided data file does not correspond",
                        "to provided header file."
                    )
                } else {
                    NULL
                }
            } else {
                NULL
            }
        }
    })
    # get data file
    observeEvent(input$data_file, {
        req(input$data_file)
        req(local$locus_type)
        req(local$proj_dir)
        # data.frame with 4 columns:
        # name (chr), size (int), type (chr), datapath (chr)
        req(is.data.frame(input$data_file))
        req(nrow(input$data_file) > 0)
        # data file
        out$file <- file.path(local$proj_dir, input$data_file$name)
        # copy to project directory
        fs::file_copy(input$data_file$datapath,
                      out$file,
                      overwrite = TRUE)
        tryCatch(fs::file_delete(input$data_file$datapath))
    })
    # # debugging
    # observe({
    #     logging("data file = ", out$file)
    # })
    # data check and user feedback
    observeEvent(out$file, {
        output$data_info <- renderUI({
            out_text <- list()
            # check
            local$file_check <- check_data_file(
                out$file, local$proj_dir, local$locus_type, local$seq_mode,
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
                helpText(
                    icon("warning"), "Issue with data file."
                )
            }
        })
    })
    # output
    return(out)
}
