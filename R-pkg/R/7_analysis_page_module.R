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
        proj_dir = NULL,
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
    setting <- proj_set # FIXME
    # update local
    observe({
        local$proj_dir <- proj_set$proj_dir
        local$proj_name <- proj_set$proj_name
    })
    # # update output
    # observe({
    #     out$setting <- setting
    # })
    # ## input data
    # input_data <- callModule(input_data_server, "input_data")
    # ## Training set sub-module
    # training_set <- callModule(training_set_server, "train_set", 
    #                            project_dir = reactive(local$project_dir),
    #                            project_name = reactive(local$project_name),
    #                            data_file = reactive(input_data$data_file),
    #                            valid_data_file = reactive(input_data$valid),
    #                            locus_type = reactive(input_data$locus_type),
    #                            validation = reactive(setting$validation))
    # output
    return(out)
}

#' Analysis project setting ui
#' @keywords internal
#' @author Ghislain Durif
analysis_proj_set_ui <- function(id) {
    ns <- NS(id)
    tagList(
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
            uiOutput(ns("file_check"))
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
                width = 11,
                textInput(
                    ns("proj_name"), 
                    label = NULL,
                    placeholder = "project_name"
                )
            ),
            column(
                width = 1,
                actionButton(
                    ns("edit_proj_name"),
                    label = "Edit",
                    icon = icon("edit"),
                    width = "100%"
                )
            )
        )
    )
}

#' Analysis project setting ui
#' @keywords internal
#' @author Ghislain Durif
#' @param proj_name project name as a `reactive`.
#' @param reset reactive to pass a reset command
analysis_proj_set_server <- function(input, output, session, 
                                     proj_name = reactive({NULL}),
                                     reset = reactive({NULL})) {
    # init local
    local <- reactiveValues(
        input_file_list = NULL,
        proj_file_check = NULL,
        reset = NULL
    )
    # init output
    out <- reactiveValues(
        proj_dir = mk_proj_dir(session),
        proj_name = NULL
    )
    # get input
    observe({
        out$proj_name <- proj_name()
        local$reset <- reset()
    })
    # FIXME reset
    observeEvent(reset, {
        local$proj_dir <- mk_proj_dir(session)
    })
    # new or existing project
    observeEvent(input$proj_type, {
        req(input$proj_type)
        if(input$proj_type == "New") {
            shinyjs::disable("edit_proj_name")
            shinyjs::enable("proj_name")
        } else if(input$proj_type %in% c("Existing", "Example")) {
            shinyjs::enable("edit_proj_name")
            shinyjs::disable("proj_name")
        }
    })
    ## Manage existing project
    possible_files <- c("diyabcGUI_proj.txt", "header.txt", "headerRF.txt", 
                        "reftableRF.bin", "statobsRF.txt")
    # check and copy uploaded files to project working directory (server-side)
    observeEvent(input$file_input, {
        req(input$file_input)
        # data.frame with 4 columns: name (chr), size (int), type (chr), datapath (chr)
        req(is.data.frame(input$file_input))
        req(nrow(input$file_input) > 0)
        local$input_file_list <- input$file_input
        # FIXME check
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
                                if(item %in% local$input_file_list) {
                                    tags$div(
                                        item,
                                        icon("check")
                                    )
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
        print(input$file_input)
    })
    # copy uploaded file to project working directory (server-side)
    # observeEvent(input$proj_file, {
    #     ##
    #     
    # })
    
    
    # if existing project, allow to modify its name
    observeEvent(input$edit_proj_name, {
        req(input$edit_proj_name)
        shinyjs::enable("proj_name")
    })
    
    ## Manage example project
    # TODO
    
    ## get project name
    # TODO
}

#' Create a project directory server-side
#' @keywords internal
#' @author Ghislain Durif
mk_proj_dir <- function(session, tag = "diyabc") {
    # create tmp dir
    tmp_dir <- tempfile(tag)
    dir.create(tmp_dir, showWarnings = FALSE)
    # clean on exit
    session$onSessionEnded(function() {
        unlink(tmp_dir)
    })
    # output
    return(tmp_dir)
}

#' Input data ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyFiles shinyFilesButton
input_data_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h4("Data file"),
        tags$span(
            shinyFilesButton(
                ns("file_choice"), 
                label = "Browse" ,
                title = "Choose a data file", 
                multiple = FALSE,
                buttonType = "default", 
                class = NULL
            ),
            verbatimTextOutput(ns("filename"), placeholder = TRUE)
        ),
        uiOutput(ns("data_info"))
    )
}

#' Input data server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyFiles parseFilePaths shinyFileChoose
input_data_server <- function(input, output, session) {
    # init local
    local <- reactiveValues(data_info = NULL)
    # init output
    out <- reactiveValues(data_file = NULL, valid = FALSE, locus_type = NULL)
    # get filename
    shinyFileChoose(
        input, 
        "file_choice", 
        roots = c(home = normalizePath('~')), 
        session = session
    )
    observe({
        req(!is.null(input$file_choice))
        selected_file <- parseFilePaths(c(home = normalizePath('~')), 
                                        input$file_choice)
        output$filename <- renderText({
            as.character(selected_file$datapath)
        })
        out$data_file <- unname(selected_file$datapath)
    })
    # # debugging
    # observe({
    #     logging("data file = ", out$data_file)
    # })
    # data info
    observeEvent(out$data_file, {
        req(!is.null(out$data_file))
        req(str_length(out$data_file) > 0)
        local$data_info <- check_data_file(out$data_file)
        req(!is.null(local$data_info$valid))
        out$valid <- local$data_info$valid
        out$locus_type <- local$data_info$locus_type
    })
    
    # show data info
    observeEvent(local$data_info, {
        req(local$data_info$msg)
        output$data_info <- renderUI({
            helpText(
                h5("Data file info"),
                do.call(
                    tags$ul,
                    lapply(local$data_info$msg, function(item) {
                        return(tags$li(item))
                    })
                )
            )
        })
    })
    # output
    return(out)
}

#' Check data file
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom tools file_ext
check_data_file <- function(data_file) {
    msg <- list()
    valid <- TRUE
    info <- NULL
    header <- NULL
    n_loci <- NULL
    n_pop <- NULL
    n_indiv <- NULL
    locus_type <- NULL
    logging("data_file = ", data_file)
    # file exists?
    if(!file.exists(data_file)) {
        msg <- append(msg, "Input file does not exist")
        valid <- FALSE
    # check file type
    } else if(tools::file_ext(data_file) != "snp") {
        # FIXME
        msg <- append(msg, "Only SNP files are managed at the moment")
        valid <- FALSE
    } else {
        # header
        info <- str_c(
            read.table(file = data_file, nrows = 1),
            collapse = " "
        )
        msg <- append(
            msg,
            str_extract(info, pattern = "<MAF=.*>")
        )
        # header
        header <- read.table(file = data_file, skip = 1, nrows = 1)
        # nb of locus
        n_loci <- length(header) - 3
        msg <- append(
            msg,
            str_c(n_loci, "loci", sep = " ")
        )
        # content
        content <- read.table(file = data_file, skip = 2)
        n_pop <- length(unique(content[,3]))
        n_indiv <- nrow(content)
        msg <- append(
            msg,
            str_c(n_indiv, "individuals from", n_pop, "populations", sep = " ")
        )
        # locus type
        candidate_locus <- c("A", "H", "X", "Y", "M")
        locus_type <- unlist(lapply(candidate_locus, function(type) {
            count <- str_count(str_c(header[-(1:3)], collapse = " "), 
                               pattern = type)
            if(count > 0)
            return(str_c(count,
                         " <", type, ">"))
        }))
        msg <- append(
            msg,
            str_c(locus_type, collapse =  " ")
        )
        # fix header
        header <- str_c(header, collapse = " ")
    }
    # output    
    out <- tibble::lst(msg, valid, header, locus_type, n_loci, n_pop, n_indiv)
}
