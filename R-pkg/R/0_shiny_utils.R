#' Project module ui
#' @description
#' Wrapper to choose between creating a new project and opening an existing one 
#' @keywords internal
#' @author Ghislain Durif
project_module_ui <- function(id, label = "New project") {
    ns <- NS(id)
    tagList(
        actionGroupButtons(
            inputIds = c(ns("create_project"), 
                         ns("open_project")),
            labels = list("New project", "Existing project"),
            fullwidth = TRUE
        ),
        uiOutput(ns("new_project"))
    )
}

#' Project module server
#' @keywords internal
#' @author Ghislain Durif
project_module_server <- function(input, output, session) {
    # session
    ns <- session$ns
    # init local values
    local <- reactiveValues(ui = NULL)
    # init output values
    out <- reactiveValues(
        new_project = NULL,
        open_project = NULL
    )
    # create new project
    observeEvent(input$create_project, {
        local$ui <- create_project_module_ui(ns("creating_project"),
                                             label = "Project name")
        output$new_project <- renderUI({
            local$ui
        })
        project <- callModule(create_project_module_server,
                              "creating_project")
        # save output
        out$new_project <- project
    })
    # open existing analysis project
    observeEvent(input$open_project, {
        local$ui <- open_project_module_ui(ns("opening_project"),
                                           label = "Project directory")
        output$new_project <- renderUI({
            local$ui
        })
        project <- callModule(open_project_module_server,
                              "opening_project")
        # save output
        out$open_project <- project
    })
    # output
    return(out)
}

#' Create project module ui
#' @keywords internal
#' @author Ghislain Durif
create_project_module_ui <- function(id, label = "New project") {
    ns <- NS(id)
    tagList(
        tags$br(),
        textInput(ns("project_name"), label = label),
        actionButton(ns("create"), label = "Create")
    )
}

#' Create project module server
#' @keywords internal
#' @author Ghislain Durif
create_project_module_server <- function(input, output, session) {
    # init local values
    local <- reactiveValues()
    # check for click
    observe({
        local$project_name <- input$project_name
        local$create_button <- input$create
    })
    # output
    return(local)
}

#' Open project module ui
#' @keywords internal
#' @author Ghislain Durif
open_project_module_ui <- function(id, label = "Open project") {
    ns <- NS(id)
    tagList(
        tags$br(),
        dir_input_module_ui(ns("project_dir"), label = label, 
                            title = "Choose a directory"),
        actionButton(ns("open"), label = "Open"),
        uiOutput(ns("project_status"))
    )
}

#' Open project module server
#' @keywords internal
#' @author Ghislain Durif
#' @param project_type string, type of the project 'analysis' or 'simulation'
#' @param headerfile string, header file name 'header.txt' or 'headersim.txt'
#' @importFrom shinyjs toggleState disable
open_project_module_server <- function(input, output, session, 
                                       project_type = "analysis",
                                       headerfile = "header.txt") {
    # namespace
    ns <- session$ns
    # init local values
    local <- reactiveValues()
    # path
    tmp_dir <- callModule(dir_input_module_server, "project_dir")
    # check project
    observe({
        local$project_path <- tmp_dir$path
        local$project_status <- headerfile %in% list.files(tmp_dir$path)
        tmp_check <- project_path2name(tmp_dir$path)
        local$project_name <- tmp_check$project_name
        local$timestamp <- tmp_check$timestamp
    })
    # notification about project directory
    output$project_status <- renderUI({
        out <- NULL
        if(!local$project_status) {
            out <- tagList(
                tags$br(),
                tags$p(
                    icon("warning"), 
                    paste0("Chosen directory is not a DIYABC-RF ", 
                           project_type, " directory.")
                )
            )
            shinyjs::disable("open") # because of issue with toggleState
        }
        out
    })
    # click
    observe({
        shinyjs::toggleState("open", condition = local$project_status)
        local$open_button <- input$open
    })
    # output
    return(local)
}


#' Directory choice module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyFiles shinyDirButton
dir_input_module_ui <- function(id, label = "Directory", 
                                title = "Choose a directory") {
    ns <- NS(id)
    tagList(
        shinyDirButton(
            ns("dir"), label = label, title = title
        ),
        verbatimTextOutput(
            ns("dir_value"), placeholder = TRUE
        )
    )
}

#' Directory choice module server
#' @keywords internal
#' @author Ghislain Durif
dir_input_module_server <- function(input, output, session) {
    # directory chooser
    shinyDirChoose(
        input,
        id = "dir",
        roots = c(home = '~')
    )
    # default path
    local <- reactiveValues(path = getwd())
    dir <- reactive(input$dir)
    # print path
    output$dir_value <- renderText({
        local$path
    })
    # update path
    observeEvent(
        ignoreNULL = TRUE,
        eventExpr = { input$dir },
        handlerExpr = {
            req(is.list(input$dir))
            home <- normalizePath("~")
            local$path <- file.path(home, 
                                    paste(unlist(dir()$path[-1]), 
                                          collapse = .Platform$file.sep))
        }
    )
    # output
    return(local)
}

#' Data input file module ui
#' @keywords internal
#' @author Ghislain Durif
data_input_file_module_ui <- function(id, label = "Data input file") {
    ns <- NS(id)
    tagList(
        fileInput(ns("data_file"), label = label),
        "Data file info",
        verbatimTextOutput(ns("data_info"))
    )
}

#' Data input file module server
#' @keywords internal
#' @author Ghislain Durif
data_input_file_module_server <- function(input, output, session) {
    data_info <- reactive({ "FIXME" })
    output$data_info <- renderText({ data_info() })
}


#' Project module ui
#' @keywords internal
#' @author Ghislain Durif
project_input_module_ui <- function(id, label = "Project", default = "Your project") {
    ns <- NS(id)
    tagList(
        textInput(
            inputId = ns("project"), 
            label = label, 
            value = default
        ),
        checkboxInput(ns("timestamp"), label = "Timestamp directory", 
                      value = TRUE),
        verbatimTextOutput(
            ns("project_name"), 
            placeholder = TRUE
        )
    )
}

#' Project module server
#' @keywords internal
#' @importFrom lubridate today
#' @author Ghislain Durif
project_input_module_server <- function(input, output, session) {
    
    project_fullname <- reactive({
        if(input$timestamp) {
            paste0(input$project, "_", lubridate::today())
        } else {
            input$project
        }
    })
    
    output$project_name <- renderText({ 
        project_fullname()
    })
    
    return(
        list(
            name = project_fullname
        )
    )
}