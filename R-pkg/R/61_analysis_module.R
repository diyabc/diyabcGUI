#' Analysis setup module ui
#' @keywords internal
#' @author Ghislain Durif
analysis_setup_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        tags$style(HTML(".box-header{text-align: center;}")),
        fluidRow(
            box(
                title = tags$b("Project settings"),
                width = 12,
                status = "primary", solidHeader = TRUE,
                collapsible = FALSE,
                analysis_proj_set_ui(ns("proj_set"))
            ),
            box(
                title = "Navigation",
                width = 12,
                analysis_tab_selector_ui(ns("tab_selector"))
            )
        )
    )
}

#' Analysis setup module server
#' @keywords internal
#' @author Ghislain Durif
analysis_setup_module_server <- function(input, output, session, parent) {
    
    ## project setting
    proj_set <- callModule(analysis_proj_set_server, "proj_set")
    
    ## tab selector
    callModule(analysis_tab_selector_server, "tab_selector", parent = parent)
}

#' Analysis training set simu module ui
#' @keywords internal
#' @author Ghislain Durif
analysis_ts_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        tags$style(HTML(".box-header{text-align: center;}")),
        fluidRow(
            box(
                title = "Training set simulations",
                width = 12,
                status = "info", solidHeader = TRUE,
                collapsible = FALSE, collapsed = FALSE,
                train_set_simu_ui(ns("train_set"))
            ),
            box(
                title = "Navigation",
                width = 12,
                analysis_tab_selector_ui(ns("tab_selector"))
            )
        )
    )
}

#' Analysis training set simu module server
#' @keywords internal
#' @author Ghislain Durif
analysis_ts_module_server <- function(input, output, session, parent) {
    
    ## Training set sub-module
    callModule(train_set_simu_server, "train_set")
    
    ## tab selector
    callModule(analysis_tab_selector_server, "tab_selector", parent = parent)
}

#' Analysis random forest analysis module ui
#' @keywords internal
#' @author Ghislain Durif
analysis_rf_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        tags$style(HTML(".box-header{text-align: center;}")),
        fluidRow(
            box(
                title = "Random Forest Analyses",
                width = 12,
                status = "warning", solidHeader = TRUE,
                collapsible = FALSE, collapsed = FALSE,
                rf_module_ui(ns("rf"))
            ),
            box(
                title = "Navigation",
                width = 12,
                analysis_tab_selector_ui(ns("tab_selector"))
            )
        )
    )
}

#' Analysis random forest analysis module server
#' @keywords internal
#' @author Ghislain Durif
analysis_rf_module_server <- function(input, output, session, parent) {
    
    ## random forest sub-module
    callModule(rf_module_server, "rf")
    
    ## tab selector
    callModule(analysis_tab_selector_server, "tab_selector", parent = parent)
}

#' Analysis project administration module ui
#' @keywords internal
#' @author Ghislain Durif
analysis_admin_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        tags$style(HTML(".box-header{text-align: center;}")),
        fluidRow(
            box(
                title = tags$b("Project administration"),
                width = 12,
                status = "danger", solidHeader = TRUE,
                collapsible = FALSE, collapsed = FALSE,
                proj_admin_ui(ns("proj_admin"))
            ),
            box(
                title = "Navigation",
                width = 12,
                analysis_tab_selector_ui(ns("tab_selector"))
            )
        )
    )
}

#' Analysis project administration module server
#' @keywords internal
#' @author Ghislain Durif
analysis_admin_module_server <- function(input, output, session, parent) {
    
    ## admin
    proj_admin <- callModule(proj_admin_server, "proj_admin", tag = "ap")
    
    ## tab selector
    callModule(analysis_tab_selector_server, "tab_selector", parent = parent)
    
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

#' Analysis project tab selector ui
#' @keywords internal
#' @author Ghislain Durif
analysis_tab_selector_ui <- function(id) {
    ns <- NS(id)
    fluidPage(
        actionGroupButtons(
            inputIds = c(
                ns("setup_link"),
                ns("ts_link"),
                ns("rf_link"),
                ns("admin_link")
            ),
            labels = c(
                "Project settings", 
                "Training set simulations",
                "Randon forest analysis",
                "Project admininstration"
            ),
            fullwidth = TRUE
        )
    )
}

#' Analysis project tab selector server
#' @keywords internal
#' @author Ghislain Durif
analysis_tab_selector_server <- function(input, output, session, parent) {
    
    observeEvent(input$setup_link, {
        log_debug("go to 'project settings' tab")
        updateTabItems(parent, "app_menu", selected = "analysis_setup_tab")
    })
    observeEvent(input$ts_link, {
        log_debug("go to 'training set simulation' tab")
        updateTabItems(parent, "app_menu", selected = "analysis_ts_tab")
    })
    observeEvent(input$rf_link, {
        log_debug("go to 'random forest' tab")
        updateTabItems(parent, "app_menu", selected = "analysis_rf_tab")
    })
    observeEvent(input$admin_link, {
        log_debug("go to 'project admin' tab")
        updateTabItems(parent, "app_menu", selected = "analysis_admin_tab")
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
        proj_config_ui(ns("proj_config")),
        hr(),
        data_file_ui(ns("data_file")),
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
    
    ## project name
    callModule(proj_name_server, "proj_name", tag = "ap")
    
    ## data type
    callModule(data_type_server, "data_type", tag = "ap")
    
    ## reset project when data type change
    observeEvent({c(env$ap$locus_type, env$ap$seq_mode)}, {
        req(env$ap$proj_dir)
        
        # clean before upload
        clean_proj_dir(env$ap$proj_dir)
        # reset env
        reset_ap()
        # file modification
        update_proj_file("ap")
        upload_proj_file("ap")
    })
    
    ## project type
    callModule(proj_type_server, "proj_type")
    
    ## project status
    callModule(proj_config_server, "proj_config")
    
    ## data file
    callModule(data_file_server, "data_file")
    
    # output$proj_is_ready <- renderUI({
    #     if(!(out$valid_proj & out$valid_data_file)) {
    #         h3(icon("warning"), "Project set up is not ready",
    #            style="color:red;text-align:center;")
    #     } else {
    #         h4(icon("check"), "Project set up is ok",
    #            style="text-align:center;")
    #     }
    # })
    #
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
            tags$li("start with a new project"),
            tags$li("open one of your own an existing project"),
            tags$li("open one of the included examples")
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
            condition = "input.proj_type == 'new'",
            ns = ns,
            new_proj_ui(ns("new_proj"))
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
        )
    )
}

#' Project type setting server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom dplyr distinct
#' @importFrom fs file_copy file_delete
proj_type_server <- function(input, output, session) {
    
    ## project type
    observeEvent(input$proj_type, {
        req(input$proj_type)
        env$ap$proj_type <- input$proj_type
    })
    
    ## New project
    callModule(new_proj_server, "new_proj")
    
    ## Existing project
    callModule(existing_proj_server, "existing_proj")
    
    ## Example project
    callModule(example_proj_server, "example_proj")
    
    ## File list for existing or example project
    callModule(proj_file_list_server, "proj_file_list")
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
proj_file_list_server <- function(input, output, session) {
    
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
    observeEvent({
        c(env$ap$file_modif, env$ap$proj_file_list, env$ap$proj_type)
    }, {
        req(env$ap$proj_type %in% c("existing", "example"))
        # output
        output$feedback_proj_file <- renderUI({
            # default
            tag_list <- tags$div(
                icon("warning"), "No file was uploaded",
                style = "color: #F89406; margin-top: -15px;"
            )
            # if example ?
            if(env$ap$proj_type == "example") {
                tag_list <- tags$div(
                    icon("warning"), "No example was selected",
                    style = "color: #F89406; margin-top: -15px;"
                )
            }
            # else
            if(isTruthy(env$ap$proj_file_list)) {
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
                    
                    missing_header <- !any(expected_files1 %in% 
                                               proj_file_list)
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
                                function(item) 
                                    return(tags$li(tags$code(item)))
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
                                function(item) 
                                    return(tags$li(tags$code(item)))
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
                                "you will be able to generate them below"
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
    tagList(
        uiOutput(ns("global_feedback")),
        uiOutput(ns("feedback_header")),
        uiOutput(ns("feedback_reftable")),
        uiOutput(ns("feedback_statobs"))
    )
}

#' Project file check server
#' @keywords internal
#' @author Ghislain Durif
proj_file_check_server <- function(input, output, session) {
    
    ## file check
    observeEvent({
        c(env$ap$file_modif, env$ap$proj_file_list, 
           env$ap$proj_dir, env$ap$locus_type)
    }, {
        # # debugging
        # print("## triggering project file check")
        # pprint(env$ap$proj_type)
        # pprint(env$ap$proj_dir)
        # pprint(env$ap$locus_type)
        
        req(env$ap$proj_type %in% c("new", "existing", "example"))
        req(env$ap$proj_dir)
        req(env$ap$locus_type)
        
        # file check
        file_check <- check_proj_file(env$ap$proj_dir, env$ap$locus_type)
        # update env
        env$ap$header_check <- file_check$header_check
        env$ap$reftable_check <- file_check$reftable_check
        env$ap$statobs_check <- file_check$statobs_check
    })
    
    ## OUTPUT
    # global
    output$global_feedback <- renderUI({
        if(is.null(env$ap$header_check) && is.null(env$ap$reftable_check) &&
           is.null(env$ap$statobs_check)) {
            helpText(
                icon("warning"), "Project is not configured yet",
                tags$p(tags$ul(tags$li(
                    "For a new project, you will be able to configure it",
                    "in the panel below"
                ))),
                tags$p(tags$ul(tags$li(
                    "For an existing or an example project,",
                    "you will be able to check the configuration",
                    "or modify it in the panel below"
                )))
            )
        } else {
            helpText(h5(icon("comment"), "Current setup"))
        }
    })
    # header
    output$feedback_header <- renderUI({
        if(isTruthy(env$ap$header_check)) {
            if(isTruthy(env$ap$header_check$valid)) {
                # data file
                data_file <- NULL
                if(env$ap$header_check$data_file %in% env$ap$proj_file_list) {
                    data_file <- tagList(
                        "Data file:", 
                        tags$code(env$ap$header_check$data_file)
                    )
                } else {
                    data_file <- tagList(
                        "Expected data file:", 
                        tags$code(env$ap$header_check$data_file),
                        "(it can be uploaded below)"
                    )
                }
                # output
                helpText(
                    tags$p(tags$ul(tags$li(data_file))),
                    tags$p(tags$ul(tags$li(
                        tags$code(env$ap$header_check$header_file),
                        "file is ok",
                        "with", 
                        tags$b(as.character(env$ap$header_check$n_scen)),
                        ifelse(
                            env$ap$header_check$n_scen > 1, 
                            "scenarii", "scenario"
                        )
                    )))
                )
            } else {
                tags$div(
                    tags$p(
                        icon("warning"), 
                        "Issue with provided", 
                        tags$code(env$ap$header_check$header_file),
                        "file:",
                        do.call(
                            tags$ul,
                            lapply(env$ap$header_check$msg, tags$li)
                        )
                    ),
                    style = "color: #F89406;"
                )
            }
        } else {
            NULL
        }
    })
    # reftable
    output$feedback_reftable <- renderUI({
        if(isTruthy(env$ap$reftable_check)) {
            if(isTruthy(env$ap$reftable_check$valid)) {
                helpText(tags$p(tags$ul(tags$li(
                    tags$code("reftableRF.bin"), "file is ok",
                    "with",
                    tags$b(as.character(env$ap$reftable_check$n_stat)),
                    "summary statistics computed over",
                    tags$b(as.character(env$ap$reftable_check$n_rec)),
                    "simulations in the training set"
                ))))
            } else {
                tags$div(
                    tags$p(
                        icon("warning"), 
                        "Issue with provided", tags$code("reftableRF.bin"),
                        "file:",
                        do.call(
                            tags$ul,
                            lapply(env$ap$header_check$msg, tags$li)
                        )
                    ),
                    style = "color: #F89406;"
                )
            }
        } else {
            NULL
        }
    })
    # statobs
    output$feedback_statobs <- renderUI({
        if(isTruthy(env$ap$statobs_check)) {
            if(isTruthy(env$ap$statobs_check$valid)) {
                helpText(tags$p(tags$ul(tags$li(
                    tags$code("statobsRF.txt"), "file is ok"
                ))))
            } else {
                tags$div(
                    tags$p(
                        icon("warning"), 
                        "Issue with provided", tags$code("statobsRF.txt"),
                        "file:",
                        do.call(
                            tags$ul,
                            lapply(env$ap$statobs_check$msg, tags$li)
                        )
                    ),
                    style = "color: #F89406;"
                )
            }
        } else {
            NULL
        }
    })
}

#' New project ui
#' @keywords internal
#' @author Ghislain Durif
new_proj_ui <- function(id) {
    ns <- NS(id)
    tagList(
        helpText(
            icon("comment"), "You will be able to upload your data file", 
            "and configure your project below"
        )
    )
}

#' New project server
#' @keywords internal
#' @author Ghislain Durif
new_proj_server <- function(input, output, session) {
    
    # clean project directory when choosing this mode
    observeEvent(env$ap$proj_type, {
        req(env$ap$proj_type == "new")
        req(env$ap$proj_dir)
        
        # clean before upload
        clean_proj_dir(env$ap$proj_dir)
        # reset env
        reset_ap()
        # file modification
        update_proj_file("ap")
        upload_proj_file("ap")
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
                tags$code("statobsRF.txt"), "and your observed data file"
            ),
            tags$li(
                "You", tags$b("cannot"), "upload both a project",
                tags$code("zip"), "file",
                "and single project-related files, those will be ignored",
                style = "margin-top: 10px;"
            ),
            tags$li(
                "When uploading", tags$b("single project-related files"),
                "you", tags$b("should"), "upload all required files",
                "at the same time (use", tags$code("CTRL+click"),
                "to select multiple files in the file chooser window)",
                style = "margin-top: 10px;"
            ),
            tags$li(
                "If you", tags$b("re-upload"), "a file or a group of files,",
                "it will", tags$b("delete"), "and", tags$b("replace"),
                "any previous upload",
                style = "margin-top: 10px;"
            ),
            tags$li(
                "If some project files are missing or have formating issues",
                "you will be able to (re)configure",
                "the corresponding settings in the next panel",
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
            "Use", tags$code("CTRL+click"), "to select more than one file"
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
existing_proj_server <- function(input, output, session) {
    
    # # file_input = data.frame with fields 'name', 'size', 'type', 'datapath'
    # # debugging
    # observe({
    #     pprint("file input")
    #     print(input$file_input)
    # })
    
    # Feedback on file upload
    observe({
        req(env$ap$proj_type == "existing")
        
        # feedback on missing file
        feedbackWarning("file_input", !isTruthy(input$file_input),
                        "Missing file(s)")
    })
    
    # reset file upload when another mode is chosen
    observeEvent({c(env$ap$proj_type, env$ap$locus_type, env$ap$seq_mode)}, {
        shinyjs::reset("file_input")
    })
    
    # clean project directory when choosing this mode
    observeEvent(env$ap$proj_type, {
        req(env$ap$proj_type == "existing")
        req(env$ap$proj_dir)
        
        # clean before upload
        clean_proj_dir(env$ap$proj_dir)
        # reset env
        reset_ap()
        # file modification
        update_proj_file("ap")
        upload_proj_file("ap")
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
                msg <- "Issue(s) with uploaded file(s)"
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
        if(!is.null(input_check) && isTruthy(input_check$valid)) {
            # file modification
            update_proj_file("ap")
            upload_proj_file("ap")
        } else {
            # clean before upload
            clean_proj_dir(env$ap$proj_dir)
            # reset env
            reset_ap()
            # file modification
            update_proj_file("ap")
            upload_proj_file("ap")
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
example_proj_server <- function(input, output, session) {
    
    # clean project directory when choosing this mode
    observeEvent(env$ap$proj_type, {
        req(env$ap$proj_type == "example")
        req(env$ap$proj_dir)
        
        # clean before upload
        clean_proj_dir(env$ap$proj_dir)
        # reset env
        reset_ap()
        # file modification
        update_proj_file("ap")
        upload_proj_file("ap")
    })
    
    # update possible input
    observeEvent({c(env$ap$proj_type, env$ap$locus_type, env$ap$seq_mode)}, {
        req(env$ap$proj_type == "example")
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
            file.path(example_dir(), input$proj_example)
        )
        fs::file_copy(
            path = file.path(example_dir(), input$proj_example, proj_files),
            new_path = file.path(env$ap$proj_dir, proj_files),
            overwrite = TRUE
        )
        
        # file modification
        update_proj_file("ap")
        upload_proj_file("ap")
    })
}

#' Project configuration feedback ui
#' @keywords internal
#' @author Ghislain Durif
proj_config_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3("Project configuration"),
        proj_file_check_ui(ns("proj_file_check"))
    )
}

#' Project configuration feedback server
#' @keywords internal
#' @author Ghislain Durif
proj_config_server <- function(input, output, session) {
    callModule(proj_file_check_server, "proj_file_check")
}

#' Data file ui
#' @keywords internal
#' @author Ghislain Durif
data_file_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3("Data file"),
        uiOutput(ns("input_data")),
        br(),
        check_data_ui(ns("check_data_file"))
    )
}

#' Data file server
#' @keywords internal
#' @author Ghislain Durif
data_file_server <- function(input, output, session) {
    
    ns <- session$ns
    
    # invalidate check in case of upload
    observeEvent({
        c(env$ap$proj_type, env$ap$locus_type, env$ap$seq_mode, 
          env$ap$file_upload)
    }, {
        env$ap$data_check <- NULL
    })
    
    # input data file
    output$input_data <- renderUI({
        if(isTruthy(env$ap$header_check) && 
           isTruthy(env$ap$header_check$valid) &&
           isTruthy(env$ap$header_check$data_file) &&
           isTruthy(env$ap$proj_file_list) &&
           (env$ap$header_check$data_file %in% env$ap$proj_file_list)) {
            # update data file in env
            env$ap$data_file <- env$ap$header_check$data_file
            # output
            helpText(
                icon("comment"),
                "Data file was already provided"
            )
        } else {
            input_data_file_ui(ns("input_data_file"))
        }
    })
    
    # input data file (if necessary)
    callModule(input_data_file_server, "input_data_file")
    
    # check data
    callModule(check_data_server, "check_data_file")
}

#' Input data file ui
#' @keywords internal
#' @author Ghislain Durif
input_data_file_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fileInput(
            ns("data_file"),
            label = NULL,
            buttonLabel = "Select file",
            multiple = FALSE
        ),
        uiOutput(ns("feedback"))
    )
}

#' Input data file server
#' @keywords internal
#' @author Ghislain Durif
#' @param proj_dir string as a `reactive`, project directory.
input_data_file_server <- function(input, output, session) {
    
    # init local
    local <- reactiveValues(upload = FALSE)
    
    # File upload ?
    observeEvent({
        c(env$ap$proj_type, env$ap$locus_type, env$ap$seq_mode)
    }, {
        local$upload <- FALSE
    })
    
    # Feedback on file upload
    observe({
        # feedback on missing file
        feedbackWarning("data_file", !local$upload,
                        "Missing data file")
    })
    
    # reset file upload when another mode is chosen
    observeEvent({
        c(env$ap$proj_type, env$ap$locus_type, env$ap$seq_mode)
    }, {
        shinyjs::reset("data_file")
    })
    
    ## get data file
    observeEvent(input$data_file, {
        local$upload <- TRUE
        # input$data_file = data.frame with 4 columns:
        # name (chr), size (int), type (chr), datapath (chr)
        req(env$ap$proj_dir)
        req(nrow(input$data_file) == 1)
        # check data file name (if header exists)
        if(isTruthy(env$ap$header_check) &&
           isTruthy(env$ap$header_check$valid) &&
           isTruthy(env$ap$header_check$data_file)) {
            req(input$data_file$name == env$ap$header_check$data_file)
        }
        # save data file name
        env$ap$data_file <- input$data_file$name
        # copy to project directory
        fs::file_copy(input$data_file$datapath,
                      file.path(env$ap$proj_dir, input$data_file$name),
                      overwrite = TRUE)
        
        if(file.exists(input$data_file$datapath)) {
            # logging("deleting:", input$data_file$datapath)
            fs::file_delete(input$data_file$datapath)
        }
        
        # file modification
        update_proj_file("ap")
        upload_proj_file("ap")
    })
    
    ## feedback
    output$feedback <- renderUI({
        if(local$upload) {
            if(isTruthy(nrow(input$data_file) == 1)) {
                if(isTruthy(env$ap$header_check) &&
                   isTruthy(env$ap$header_check$valid) &&
                   isTruthy(env$ap$header_check$data_file) &&
                   (input$data_file$name != env$ap$header_check$data_file)) {
                    tags$div(
                        icon("warning"), 
                        "Provided data file name does not match",
                        "expected data file name from", 
                        tags$code(env$ap$header_check$header_file), "file",
                        style = "color: #F89406;"
                    )
                } else {
                    NULL
                }
            }
        } else {
            tags$div(
                icon("warning"), 
                "Missing data file",
                style = "color: #F89406;"
            )
        }
    })
}

#' Check data ui
#' @keywords internal
#' @author Ghislain Durif
check_data_ui <- function(id) {
    ns <- NS(id)
    tagList(
        helpText(
            icon("clock"),
            "Checking the data file may take some time"
        ),
        fluidRow(
            column(
                width = 4,
                actionButton(
                    ns("check"),
                    label = "Check data",
                    icon = icon("check"),
                    width = '100%'
                )
            ),
            column(
                width = 8,
                uiOutput(ns("feedback_check"))
            )
        ),
        br(),
        uiOutput(ns("feedback")),
        uiOutput(ns("data_info"))
    )
}

#' Check data server
#' @keywords internal
#' @author Ghislain Durif
check_data_server <- function(input, output, session) {
    
    # init local
    local <- reactiveValues(run = FALSE)
    
    ## warning
    output$feedback_check <- renderUI({
        
        if(!isTruthy(env$ap$data_check) && !local$run) {
            tags$p(
                tags$div(
                    icon("warning"), "Data file was not checked",
                    style = "color: #F89406;"
                )
            )
        } else {
            NULL
        }
    })
    
    ## run data check
    observeEvent(input$check, {
        req(input$check)
        req(!local$run)
        req(env$ap$proj_dir)
        req(env$ap$locus_type)
        req(env$ap$seq_mode)
        req(env$ap$data_file)
        
        # ask to run check
        local$run <- TRUE
    })
    
    ## data check
    observeEvent(local$run, {
        req(isTruthy(local$run))
        req(env$ap$data_file)
        req(env$ap$proj_dir)
        req(env$ap$locus_type)
        req(env$ap$seq_mode)

        # data file check
        env$ap$data_check <- check_data_file(
            env$ap$data_file, env$ap$proj_dir,
            env$ap$locus_type, env$ap$seq_mode
        )

        # run is over
        local$run <- FALSE
    })
    
    ## data info
    output$data_info <- renderUI({
        req(env$ap$locus_type)
        req(env$ap$seq_mode)
        
        # data case
        tmp_data_case <- NULL
        if(env$ap$locus_type == "mss") {
            tmp_data_case <- "Microsat/Sequence"
        ## snp locus / indseq
        } else if((env$ap$locus_type == "snp") &&
                  (env$ap$seq_mode == "indseq")) {
            tmp_data_case <- "SNP IndSeq"
        ## snp locus / poolseq
        } else if((env$ap$locus_type == "snp") &&
                  (env$ap$seq_mode == "poolseq")) {
            tmp_data_case <- "SNP PoolSeq"
        }

        # output
        if(isTruthy(env$ap$data_check)) {
            if(isTruthy(env$ap$data_check$valid)) {
                format_data_info(
                    env$ap$data_check, env$ap$locus_type, env$ap$seq_mode
                )
            } else if(isTruthy(env$ap$data_check$msg)) {
                tags$div(
                    tags$p(
                        icon("warning"),
                        "Issue with your", tags$b(tmp_data_case), "data file:",
                        do.call(
                            tags$ul, lapply(env$ap$data_check$msg, tags$li)
                        )
                    ),
                    style = "color: #F89406;"
                )
            } else {
                tags$div(
                    tags$p(
                        icon("warning"),
                        "Issue with your", tags$b(tmp_data_case), "data file"
                    ),
                    style = "color: #F89406;"
                )
            }
        } else {
            NULL
        }
    })
}
