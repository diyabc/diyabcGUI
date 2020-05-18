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
                collapsible = TRUE,
                new_proj_set_ui(ns("proj_set")) %>% 
                    helper(type = "markdown", 
                           content = "analysis_project"),
                hr(),
                input_data_ui(ns("input_data"))
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
                "FILL ME"
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
#' @param project_dir project directory as a `reactive`.
#' @param project_name project name as a `reactive`.
#' @param scenario_list list of raw scenarii as a `reactive`.
analysis_page_server <- function(input, output, session,
                                 project_dir = reactive({NULL}),
                                 project_name = reactive({NULL}),
                                 scenario_list = reactive({NULL})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        project_dir = NULL,
        project_name = NULL,
        scenario_list = NULL
    )
    # get input
    observe({
        local$project_dir = project_dir()
        local$project_name = project_name()
        local$scenario_list = scenario_list()
    })
    # init output
    out <- reactiveValues(
        setting = NULL,
        scenario = NULL
    )
    ## project setting
    setting <- callModule(new_proj_set_server, "proj_set")
    # update local
    observe({
        local$project_dir <- setting$project_dir
        local$project_name <- setting$project_name
    })
    # update output
    observe({
        out$setting <- setting
    })
    ## input data
    input_data <- callModule(input_data_server, "input_data")
    ## Training set sub-module
    training_set <- callModule(training_set_server, "train_set", 
                               project_dir = reactive(local$project_dir))
    # output
    return(out)
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
    out <- reactiveValues(data_file = NULL)
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
        print(local$data_info)
    })
    
    # show data info
    observeEvent(local$data_info, {
        req(local$data_info$msg)
        print(local$data_info$msg)
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
