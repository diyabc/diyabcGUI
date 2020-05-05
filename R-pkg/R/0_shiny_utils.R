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
project_input_module_ui <- function(id, label = "Project", 
                                    default = "project_name") {
    ns <- NS(id)
    tagList(
        textInput(
            inputId = ns("project_name"), 
            label = label, 
            value = default
        ),
        checkboxInput(
            ns("timestamp"), 
            label = "Timestamp directory", 
            value = FALSE
        ),
        verbatimTextOutput(
            ns("project_fullname"), 
            placeholder = TRUE
        )
    )
}

#' Project module server
#' @keywords internal
#' @importFrom lubridate today
#' @author Ghislain Durif
#' @importFrom lubridate today
#' @importFrom shinyjs disable
#' @importFrom stringr str_detect str_extract
project_input_module_server <- function(input, output, session, 
                                        project_name = NULL, existing = FALSE) {
    # init local reactive values
    local <- reactiveValues(new = TRUE,
                            timestamp = lubridate::today())
    # init output reactive values
    out <- reactiveValues(
        project_name = project_name,
        project_fullname = NULL
    )
    # deactivate timestamp if existing project
    observe({
        if(existing) {
            # deactivate timestamp button
            shinyjs::disable("timestamp")
        }
    })
    # update project name if initial value is given
    observe({
        if(local$new & !is.null(out$project_name)) {
            updateTextInput(session, "project_name", 
                            value = out$project_name)
        }
        local$new <- FALSE
    })
    # project name update
    observeEvent(input$project_name, {
        out$project_name <- input$project_name
    })
    # update timestamp depending on initial project name value
    observe({
        if(input$timestamp & !existing) {
            out$project_fullname <- paste0(out$project_name, "_", 
                                           local$timestamp)
        } else {
            out$project_fullname <- out$project_name
        }
    })
    # render project full name
    output$project_fullname <- renderText({ 
        out$project_fullname
    })
    # output
    return(out)
}