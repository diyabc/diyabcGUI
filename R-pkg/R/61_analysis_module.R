#' Analysis module ui
#' @keywords internal
#' @author Ghislain Durif
analysis_module_ui <- function(id) {
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
                title = tags$b("Project administration"),
                width = 12,
                status = "danger", solidHeader = FALSE,
                collapsible = FALSE, collapsed = FALSE,
                proj_admin_ui(ns("proj_admin"))
            )
        )
    )
}

#' Analysis module server
#' @keywords internal
#' @author Ghislain Durif
analysis_module_server <- function(input, output, session) {

    ## project setting
    proj_set <- callModule(analysis_proj_set_server, "proj_set")

    ## Training set sub-module
    # training_set <- callModule(training_set_server, "train_set")

    ## random forest sub-module
    # rf <- callModule(rf_module_server, "rf")

    ## action
    proj_admin <- callModule(proj_admin_server, "proj_admin", tag = "ap")

    ## reset
    observeEvent(proj_admin$reset, {
        req(proj_admin$reset)
        session$reload()
    })

    ## clean on exit
    session$onSessionEnded(function() {
        isolate(tryCatch(function() {
            if(isTruthy(env$ap$proj_dir)) fs::dir_delete(env$ap$proj_dir)
        }))
    })
}

#' Analysis project setting ui
#' @keywords internal
#' @author Ghislain Durif
analysis_proj_set_ui <- function(id) {
    ns <- NS(id)
    tagList(
        proj_name_ui(ns("proj_name")),
        hr(),
        h3("Data type"),
        data_type_ui(ns("data_type")),
        hr(),
        proj_type_ui(ns("proj_type")),
        hr(),
        uiOutput(ns("feedback"))
    )
}

#' Analysis project setting server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom dplyr distinct
#' @importFrom fs file_copy file_delete
analysis_proj_set_server <- function(input, output, session) {


    # init local
    local <- reactiveValues()

    # init output
    out <- reactiveValues()

    ## project name
    proj_name <- callModule(proj_name_server, "proj_name", tag = "ap")

    ## data type
    data_type <- callModule(data_type_server, "data_type", tag = "ap")

    ## project type
    proj_type <- callModule(proj_type_server, "proj_type")

    # output$proj_is_ready <- renderUI({
    #     if(!(out$valid_proj & out$valid_data_file)) {
    #         h3(icon("warning"), "Project set up is not ready.",
    #            style="color:red;text-align:center;")
    #     } else {
    #         h4(icon("check"), "Project set up is ok.",
    #            style="text-align:center;")
    #     }
    # })
    #

    ## output
    return(out)
}

#' Project type setting ui
#' @keywords internal
#' @author Ghislain Durif
proj_type_ui <- function(id) {
    ns <- NS(id)
    # inline help for project type
    proj_type_help <- tagList(
        "You can either:",
        tags$ol(
            tags$li("start with a new project;"),
            tags$li("open one of your own an existing project;"),
            tags$li("open one of the included examples.")
        )
    )
    # ui
    tagList(
        h3("Project type") %>%
            helper(type = "inline", content = as.character(proj_type_help)),
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
            existing_proj_ui(ns("existing_proj"))
        ),
        conditionalPanel(
            condition = "input.proj_type == 'example'",
            ns = ns,
            example_proj_ui(ns("example_proj")),
        ),
        conditionalPanel(
            condition = "input.proj_type !== 'new'",
            ns = ns,
            proj_file_list_ui(ns("proj_file_list")),
            proj_file_check_ui(ns("proj_file_check"))
        ),
        hr(),
        h3("Data file"),
        helpText(
            icon("clock"),
            "Loading and checking the data file may take some time."
        ),
        conditionalPanel(
            condition = "input.proj_type !== 'example'",
            ns = ns,
            input_data_ui(ns("input_data_file")),
        ),
        check_data_ui(ns("check_data_file")),
        hr(),
        uiOutput(ns("feedback_data"))
    )
}

#' Project type setting server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom dplyr distinct
#' @importFrom fs file_copy file_delete
proj_type_server <- function(input, output, session) {

    # init local
    local <- reactiveValues(
        file_list = NULL,
        valid_files = FALSE
    )

    ## Existing project
    callModule(
        existing_proj_server, "existing_proj",
        proj_type = reactive(input$proj_type)
    )

    ## Example project
    callModule(
        example_proj_server, "example_proj",
        proj_type = reactive(input$proj_type)
    )

    ## File list for existing or example project
    callModule(
        proj_file_list_server, "proj_file_list",
        proj_type = reactive(input$proj_type)
    )

    # ## new or existing project
    # observe({
    #     req(input$proj_type)
    #     if(input$proj_type == "new") {
    #         out$new_proj <- TRUE
    #     } else if(input$proj_type == "existing") {
    #         out$new_proj <- FALSE
    #         req(!is.null(local$local$proj_file_list))
    #         if("headerRF.txt" %in% local$proj_file_list) {
    #             out$new_proj <- FALSE
    #         } else {
    #             out$new_proj <- TRUE
    #         }
    #     } else if(input$proj_type == "example") {
    #         out$new_proj <- FALSE
    #     }
    # })
    #
    # ## Manage existing project
    # possible_files <- c("headerRF.txt", "reftableRF.bin", "statobsRF.txt")
    # # copy uploaded files to project working directory (server-side)
    # observeEvent(input$file_input, {
    #     ## user file input
    #     req(input$file_input)
    #     # data.frame with 4 columns:
    #     # name (chr), size (int), type (chr), datapath (chr)
    #     req(is.data.frame(input$file_input))
    #     req(nrow(input$file_input) > 0)
    #
    #     ## extraction
    #     new_file_input <- input$file_input
    #
    #     tmp_proj_check <- existing_proj_file_check(
    #         new_file_input, possible_files, out$proj_dir, local$file_input
    #     )
    #
    #     local$file_input <- tmp_proj_check$file_input
    #     local$existing_proj_zip <- tmp_proj_check$existing_proj_zip
    # })
    #
    # # print possible files when uploading existing projects
    # output$file_check <- renderUI({
    #     helpText(
    #         icon("comment"), "Project-related files check",
    #         tags$p(
    #             tags$div(
    #                 style = "column-count:2;",
    #                 do.call(
    #                     tags$ul,
    #                     lapply(
    #                         possible_files,
    #                         function(item) tags$li(
    #                             if(item %in% local$file_input$name) {
    #                                 ind <- which(item == local$file_input$name)
    #                                 if(local$file_input$valid[ind]) {
    #                                     tags$div(
    #                                         tags$code(item),
    #                                         icon("check")
    #                                     )
    #                                 } else {
    #                                     tags$div(
    #                                         tags$code(item),
    #                                         icon("times")
    #                                     )
    #                                 }
    #                             } else {
    #                                 tags$code(item)
    #                             }
    #                         )
    #                     )
    #                 )
    #             )
    #         )
    #     )
    # })
    #
    # ## Manage example project
    # # update possible input
    # observe({
    #     req(!is.null(data_type$locus_type))
    #     req(!is.null(data_type$seq_mode))
    #
    #     if(data_type$locus_type == "mss") {
    #         updateSelectInput(
    #             session,
    #             "proj_example",
    #             choices = c("", "Not available at the moment"),
    #             selected = NULL
    #         )
    #     } else if(data_type$locus_type == "snp" &
    #               data_type$seq_mode == "indseq") {
    #         possible_choices <- basename(
    #             list.dirs(
    #                 example_dir()
    #             )
    #         )
    #         possible_choices <- possible_choices[str_detect(possible_choices,
    #                                                         "IndSeq")]
    #         updateSelectInput(
    #             session,
    #             "proj_example",
    #             choices = c("", possible_choices),
    #             selected = NULL
    #         )
    #     } else if(data_type$locus_type == "snp" &
    #              data_type$seq_mode == "poolseq") {
    #         possible_choices <- basename(
    #             list.dirs(
    #                 example_dir()
    #             )
    #         )
    #         possible_choices <- possible_choices[str_detect(possible_choices,
    #                                                         "PoolSeq")]
    #         updateSelectInput(
    #             session,
    #             "proj_example",
    #             choices = c("", possible_choices),
    #             selected = NULL
    #         )
    #     }
    # })
    # # copy files if required
    # observeEvent(input$proj_example, {
    #
    #     req(input$proj_type == "example")
    #     req(input$proj_example)
    #
    #     # copy files
    #     proj_files <- list.files(
    #         file.path(
    #             example_dir(),
    #             input$proj_example
    #         )
    #     )
    #     fs::file_copy(
    #         path = file.path(
    #             example_dir(),
    #             input$proj_example,
    #             proj_files
    #         ),
    #         new_path = file.path(
    #             out$proj_dir,
    #             proj_files
    #         ),
    #         overwrite = TRUE
    #     )
    #
    #     # update file input
    #     # data.frame with 4 columns:
    #     # name (chr), size (int), type (chr), datapath (chr)
    #     local$file_input <- data.frame(
    #         name = proj_files,
    #         size = rep(NA, length(proj_files)),
    #         type = rep(NA, length(proj_files)),
    #         datapath = file.path(
    #             out$proj_dir,
    #             proj_files
    #         ),
    #         valid = rep(TRUE, length(proj_files))
    #     )
    #
    #     ## file type
    #     ind <- which(local$file_input$name == "headerRF.txt")
    #     local$file_input$type[ind] <- "text/plain"
    #     ind <- which(local$file_input$name == "reftableRF.bin")
    #     local$file_input$type[ind] <- "application/octet-stream"
    #     ind <- which(local$file_input$name == "statObsRF.txt")
    #     local$file_input$type[ind] <- "text/plain"
    # })
    #
    # ## check current project header file
    # observeEvent(local$file_input, {
    #
    #     req(is.data.frame(local$file_input))
    #     req(nrow(local$file_input) > 0)
    #     req(!is.null(input$proj_type))
    #
    #     # # debugging
    #     # pprint("file input")
    #     # pprint(local$file_input)
    #
    #     ## header check
    #     if("headerRF.txt" %in% local$file_input$name) {
    #         ind <- which(local$file_input$name == "headerRF.txt")
    #         header_file_content <- parse_diyabc_header(
    #             file_name = local$file_input$datapath[ind],
    #             file_type = local$file_input$type[ind],
    #             locus_type = data_type$locus_type
    #         )
    #         # valid header file
    #         local$file_input$valid[ind] <- header_file_content$valid
    #         # header data file name
    #         local$header_data_file <- header_file_content$data_file
    #         # header data file content
    #         out$proj_header_content <- header_file_content
    #         # data from example ?
    #         if(input$proj_type == "example") {
    #             local$data_file <- header_file_content$data_file
    #         }
    #     }
    #
    #     # # debugging
    #     # pprint("file input")
    #     # pprint(local$file_input)
    #
    #     ## delete non valid files
    #     lapply(
    #         split(local$file_input, seq(nrow(local$file_input))),
    #         function(item) {
    #             if(!item$valid) {
    #                 if(file.exists(item$datapath)) {
    #                     logging("deleting:", item$datapath)
    #                     fs::file_delete(item$datapath)
    #                 }
    #             }
    #         }
    #     )
    #
    #     ## project file list
    #     out$proj_file_list <- local$file_input$name[local$file_input$valid]
    #
    #     # # debugging
    #     # pprint("file_input")
    #     # pprint(local$file_input)
    # })
    #
    # ## Data file file
    # data_file <- callModule(
    #     input_data_server, "input_data_file",
    #     proj_dir = reactive(out$proj_dir),
    #     existing_proj_zip = reactive(local$existing_proj_zip)
    # )
    #
    # # update local if data file upload
    # observe({
    #     req(!is.null(data_file$name))
    #     local$data_file <- data_file$name
    # })
    #
    # # data file extracted from existing project zip file
    # observe({
    #     req(!is.null(local$existing_proj_zip))
    #     req(!is.null(local$header_data_file))
    #
    #     if(local$existing_proj_zip) {
    #         local$data_file <- local$header_data_file
    #     }
    # })
    #
    # ## Data file check
    # check_data <- callModule(
    #     check_data_server, "check_data_file",
    #     data_file = reactive(local$data_file),
    #     expected_data_file = reactive(local$header_data_file),
    #     locus_type = reactive(out$locus_type),
    #     seq_mode = reactive(out$seq_mode),
    #     proj_dir = reactive(out$proj_dir)
    # )
    #
    # # update output
    # observe({
    #     out$data_file <- data_file$name
    #     out$data_info <- check_data$info
    #     out$valid_data_file <- check_data$valid
    # })
    #
    # # valid set up ?
    # observe({
    #
    #     req(!is.null(out$valid_data_file))
    #     req(!is.null(local$valid_proj_name))
    #
    #     # check header if required
    #     valid_header <- TRUE
    #     if(!is.null(out$proj_header_content$valid)) {
    #         valid_header <- out$proj_header_content$valid
    #     }
    #
    #     out$valid_proj <- local$valid_proj_name & valid_header & out$valid_data_file
    #
    #     # # debugging
    #     # logging("valid proj?", out$valid_proj)
    # })
    #
    # output$proj_is_ready <- renderUI({
    #     if(!(out$valid_proj & out$valid_data_file)) {
    #         h3(icon("warning"), "Project set up is not ready.",
    #            style="color:red;text-align:center;")
    #     } else {
    #         h4(icon("check"), "Project set up is ok.",
    #            style="text-align:center;")
    #     }
    # })
}

#' Feedback on project file list ui
#' @keywords internal
#' @author Ghislain Durif
proj_file_list_ui <- function(id) {
    ns <- NS(id)
    tagList(
        helpText(
            uiOutput(ns("feedback_proj_file"))
        )
    )
}

#' Feedback on project file list server
#' @keywords internal
#' @author Ghislain Durif
#' @param proj_type character string, `"new"`, `"existing"` or `"example"`.
proj_file_list_server <- function(input, output, session,
                                  proj_type = reactive({NULL})) {
    # init local
    local <- reactiveValues(proj_type = NULL)

    # get input
    observe({
        local$proj_type <- proj_type()
    })

    # # debugging
    # observe({
    #     req(env$ap$file_modif)
    #     req(env$ap$proj_file_list)
    #     pprint("file modif")
    #     pprint(env$ap$file_modif)
    #     pprint("file list")
    #     pprint(env$ap$proj_file_list)
    # })

    # feedback on list of uploaded files
    observeEvent(
        {c(env$ap$file_modif, env$ap$proj_file_list, local$proj_type)}, {
        req(local$proj_type %in% c("existing", "example"))
        # output
        output$feedback_proj_file <- renderUI({
            # default
            tag_list <- tags$div(
                icon("warning"), "No file was uploaded.",
                style = "color: #F89406; margin-top: -15px;"
            )
            # else
            if(isTruthy(env$ap) && isTruthy(env$ap$proj_file_list)) {
                # project files
                proj_file_list <- env$ap$proj_file_list

                if(length(proj_file_list) > 0) {
                    # expected files
                    expected_files1 <- c("headerRF.txt", "header.txt")
                    expected_files2 <- c("statobsRF.txt", "reftableRF.bin")
                    expected_files <- c(expected_files1, expected_files2)

                    # important project files that are present
                    important_files <- expected_files[expected_files %in%
                                                          proj_file_list]

                    # additional files
                    additional_files <- proj_file_list[!proj_file_list %in%
                                                           important_files]

                    # missing files ?
                    missing_files <- NULL

                    missing_header <- !any(expected_files1 %in% proj_file_list)
                    if(missing_header) {
                        missing_files <- c(missing_files, "headerRF.txt")
                    }

                    missing_files2 <- !(expected_files2 %in% proj_file_list)
                    if(any(missing_files2)) {
                        missing_files <- c(missing_files,
                                           expected_files2[missing_files2])
                    }
                    # project core files
                    subitem1 <- NULL
                    if(length(important_files) > 0) {
                        subitem1 <- tags$div(
                            do.call(tags$ul, lapply(
                                important_files,
                                function(item) return(tags$li(tags$code(item)))
                            ))
                        )
                    } else {
                        subitem1 <- tags$b("none")
                    }
                    # additional files
                    subitem2 <- NULL
                    if(length(additional_files) > 0) {
                        subitem2 <- tags$div(
                            do.call(tags$ul, lapply(
                                additional_files,
                                function(item) return(tags$li(tags$code(item)))
                            ))
                        )
                    } else {
                        subitem2 <- tags$b("none")
                    }
                    item1 <- helpText(
                        h5(icon("comment"), tags$b("Uploaded files")),
                        fluidRow(
                            column(
                                width = 6,
                                tagList(
                                    tags$p("Project core files:", subitem1)
                                )
                            ),
                            column(
                                width = 6,
                                tagList(
                                    tags$p("Additional files:", subitem2)
                                )
                            )
                        )
                    )
                    # missing files
                    item2 <- NULL
                    if(length(missing_files) > 0) {
                        item2 <- tags$div(
                            tags$p(
                                icon("warning"),
                                "Potentially missing files",
                                "for an existing project:",
                                tags$div(
                                    do.call(tags$ul, lapply(
                                        missing_files,
                                        function(item)
                                            return(tags$li(tags$code(item)))
                                    ))
                                ),
                                tags$b("Note:"),
                                "you will be able to generate them below."
                            ),
                            style = "color: #F89406;"
                        )
                    }
                    tag_list <- tagList(item1, item2)
                }
            }
            # output
            tag_list
        })
    })
}

#' Project file check ui
#' @keywords internal
#' @author Ghislain Durif
proj_file_check_ui <- function(id) {
    ns <- NS(id)
    tagList()
}

#' Project file check server
#' @keywords internal
#' @author Ghislain Durif
proj_file_check_server <- function(input, output, session) {
    # TODO

    ## file check
    observe({c(env$ap$file_modif, env$ap$proj_file_list)}, {
        req(env$ap$proj_dir)
        req(env$ap$locus_type)
        # file check
        file_check <- check_proj_file(
            env$ap$proj_dir, env$ap$locus_type
        )
        # header ?
        if(!is.null(file_check$header_check)) {
            # FIXME
        }
    })
}

#' Existing project ui
#' @keywords internal
#' @author Ghislain Durif
existing_proj_ui <- function(id) {
    ns <- NS(id)
    # inline help for proj file input
    proj_file_help <- tagList(
        tags$ul(
            tags$li(
                "You can", tags$b("either"), "upload:",
                "a project", tags$code("zip"),
                "file generated in a previous run",
                tags$b("or"),
                "single project-related files, including",
                tags$code("headerRF.txt"), ", ",
                tags$code("reftableRF.bin"), ", ",
                tags$code("statobsRF.txt"), "and your observed data file."
            ),
            tags$li(
                "You", tags$b("cannot"), "upload both a project",
                tags$code("zip"), "file",
                "and single project-related files, those will be ignored.",
                style = "margin-top: 10px;"
            ),
            tags$li(
                "When uploading", tags$b("single project-related files"),
                "you", tags$b("should"), "upload all required files",
                "at the same time (use", tags$code("CTRL+click"),
                "to select multiple files in the file chooser window).",
                style = "margin-top: 10px;"
            ),
            tags$li(
                "If you", tags$b("re-upload"), "a file or a group of files,",
                "it will", tags$b("delete"), "and", tags$b("replace"),
                "any previous upload.",
                style = "margin-top: 10px;"
            ),
            tags$li(
                "If some project files are missing or have formating issues",
                "you will be able to (re)configure",
                "the corresponding settings in the next panel.",
                style = "margin-top: 10px;"
            )
        )
    )
    # ui
    tagList(
        h4(tags$b("Project files")) %>%
            helper(
                type = "inline",
                content = as.character(proj_file_help)
            ),
        helpText(
            "Use", tags$code("CTRL+click"), "to select more than one file."
        ),
        fileInput(
            ns("file_input"),
            label = NULL,
            buttonLabel = "Select file(s)",
            multiple = TRUE,
            accept = c(
                ".txt",
                ".bin",
                ".zip"
            )
        ),
        uiOutput(ns("feedback_existing"))
    )
}

#' Existing project server
#' @keywords internal
#' @author Ghislain Durif
#' @param proj_type character string, `"new"`, `"existing"` or `"example"`.
existing_proj_server <- function(input, output, session,
                                 proj_type = reactive({NULL})) {
    # init local
    local <- reactiveValues(proj_type = NULL)

    # get input
    observe({
        local$proj_type <- proj_type()
    })

    # # file_input = data.frame with fields 'name', 'size', 'type', 'datapath'
    # # debugging
    # observe({
    #     pprint("file input")
    #     print(input$file_input)
    # })

    # Feedback on file upload
    observe({
        req(local$proj_type)
        req(local$proj_type == "existing")

        # feedback on missing file
        feedbackWarning("file_input", !isTruthy(input$file_input),
                        "Missing file(s).")
    })

    # reset file upload when another mode is chosen
    observe({
        req(local$proj_type)
        req(local$proj_type != "existing")
        shinyjs::reset("file_input")
    })
    
    # clean project directory when choosing this mode
    observeEvent(local$proj_type, {
        req(local$proj_type)
        req(local$proj_type == "existing")
        req(env$ap$proj_dir)
        
        # clean before upload
        clean_proj_dir(env$ap$proj_dir)
        # file modification
        update_proj_file("ap")
    })

    # manage file upload (copy to project directory)
    observeEvent(input$file_input, {
        req(input$file_input)
        req(env$ap$proj_dir)

        # upload
        input_check <- tryCatch(
            proj_file_input(input$file_input, env$ap$proj_dir),
            error = function(e) return(NULL)
        )

        # feedback
        output$feedback_existing <- renderUI({
            if(is.null(input_check) || !input_check$valid) {
                msg <- "Issue(s) with uploaded file(s)."
                feedbackWarning(
                    "file_input", is.null(input_check) || !input_check$valid,
                    msg
                )
                if(length(input_check$msg) > 0) {
                    tags$div(
                        icon("warning"), "Issue(s) with uploaded file(s):",
                        do.call(tags$ul, lapply(input_check$msg, tags$li)),
                        style = "color: #F89406; margin-top: -15px;"
                    )
                } else {
                    NULL
                }
            } else {
                NULL
            }
        })

        # update project file list and check files
        if(!is.null(input_check) && input_check$valid) {
            # file modification
            update_proj_file("ap")
        } else {
            # clean after failed upload
            clean_proj_dir(env$ap$proj_dir)
            # file modification
            update_proj_file("ap")
        }
    })
}

#' Example project ui
#' @keywords internal
#' @author Ghislain Durif
example_proj_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h4(tags$b("Select an example")),
        selectInput(
            ns("proj_example"),
            label = NULL,
            choices = c("", "Not available at the moment"),
            selected = NULL,
            multiple = FALSE
        )
    )
}

#' Example project server
#' @keywords internal
#' @author Ghislain Durif
#' @param proj_type character string, `"new"`, `"existing"` or `"example"`.
example_proj_server <- function(input, output, session,
                                proj_type = reactive({NULL})) {

    # init local
    local <- reactiveValues(proj_type = NULL)

    # get input
    observe({
        local$proj_type <- proj_type()
    })
    
    # clean project directory when choosing this mode
    observeEvent(local$proj_type, {
        req(local$proj_type)
        req(local$proj_type == "example")
        req(env$ap$proj_dir)
        
        # clean before upload
        clean_proj_dir(env$ap$proj_dir)
        # file modification
        update_proj_file("ap")
    })

    # update possible input
    observeEvent(local$proj_type, {
        req(local$proj_type)
        req(local$proj_type == "example")
        req(env$ap$locus_type)

        ## MSS
        if(env$ap$locus_type == "mss") {
            updateSelectInput(
                session,
                "proj_example",
                choices = c("", "Not available at the moment"),
                selected = NULL
            )
        ## SNP
        } else if(env$ap$locus_type == "snp") {
            req(env$ap$seq_mode)
            ## IndSeq
            if(env$ap$seq_mode == "indseq") {
                possible_choices <- basename(
                    list.dirs(
                        example_dir()
                    )
                )
                possible_choices <-
                    possible_choices[str_detect(possible_choices, "IndSeq")]
                updateSelectInput(
                    session,
                    "proj_example",
                    choices = c("", possible_choices),
                    selected = NULL
                )
            ## PoolSeq
            } else if(env$ap$seq_mode == "poolseq") {
                possible_choices <- basename(
                    list.dirs(
                        example_dir()
                    )
                )
                possible_choices <-
                    possible_choices[str_detect(possible_choices, "PoolSeq")]
                updateSelectInput(
                    session,
                    "proj_example",
                    choices = c("", possible_choices),
                    selected = NULL
                )
            }
        }
    })

    # manage file upload (copy to project directory)
    observeEvent(input$proj_example, {
        req(input$proj_example)
        req(env$ap$proj_dir)
        
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
                env$ap$proj_dir,
                proj_files
            ),
            overwrite = TRUE
        )
        
        # file modification
        update_proj_file("ap")
    })
}

#' Input data ui
#' @keywords internal
#' @author Ghislain Durif
input_data_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("feedback")),
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
                              proj_dir = reactive({NULL}),
                              existing_proj_zip = reactive({NULL})) {
    ## init local
    local <- reactiveValues(
        # input
        proj_dir = NULL,
        existing_proj_zip = NULL
    )
    ## get input
    observe({
        local$proj_dir <- proj_dir()
        local$existing_proj_zip <- existing_proj_zip()
        # # debugging
        # pprint(paste0("input proj dir = ", local$proj_dir))
    })
    ## init output
    out <- reactiveValues(
        name = NULL
    )

    ## feedback
    output$feedback <- renderUI({
        if(!is.null(local$existing_proj_zip)) {
            if(local$existing_proj_zip) {
                helpText(
                    icon("comment"),
                    "Data file was already extracted from project zip file."
                )
            } else {
                NULL
            }
        } else {
            NULL
        }
    })

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
                icon("comment"), "Data file info",
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
