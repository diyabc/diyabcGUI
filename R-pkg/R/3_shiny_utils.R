#' Directory choice module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyFiles shinyDirButton
dir_choice_ui <- function(id, label = "Directory", 
                          title = "Choose a directory") {
    ns <- NS(id)
    tagList(
        shinyDirButton(
            ns("dir_choice"), label = label, title = title
        ),
        verbatimTextOutput(
            ns("dir_value"), placeholder = TRUE
        )
    )
}

#' Directory choice module server
#' @keywords internal
#' @author Ghislain Durif
#' @param default `reactiveValues` with attribute `dir`.
#' @importFrom shinyFiles shinyDirChoose
dir_choice_server <- function(input, output, session, 
                              default = reactiveValues(dir = NULL)) {
    # init output reactive values
    out <- reactiveValues(path = getwd())
    # check if default path is supplied
    observeEvent(default$dir, {
        req(default$dir)
        if(dir.exists(default$dir)) {
            out$path <- default$dir
        }
    })
    # directory chooser
    shinyDirChoose(
        input,
        id = "dir_choice",
        roots = c(home = normalizePath('~'))
    )
    # reactive directory
    dir <- reactive(input$dir_choice)
    # print path
    output$dir_value <- renderText({
        out$path
    })
    # update path
    observeEvent(input$dir_choice, {
        req(is.list(input$dir_choice))
        home <- normalizePath('~')
        out$path <- file.path(home, 
                              paste(unlist(dir()$path[-1]), 
                                    collapse = .Platform$file.sep))
    })
    # debugging
    observe({
        logging("current path = ", out$path)
    })
    # output
    return(out)
}

#' Project naming module ui
#' @keywords internal
#' @author Ghislain Durif
proj_name_ui <- function(id, label = "Project", default = "project_name") {
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

#' Project naming module server
#' @keywords internal
#' @author Ghislain Durif
#' @param project_name `reactiveValues` with attribute `name`.
#' @param existing boolean, existing project ?
#' @importFrom lubridate today
#' @importFrom shinyjs disable
#' @importFrom stringr str_detect str_extract
proj_name_server <- function(input, output, session, 
                                    project_name = reactiveValues(name = NULL), 
                                    existing = FALSE) {
    # init local reactive values
    local <- reactiveValues(timestamp = lubridate::today())
    # init output reactive values
    out <- reactiveValues()
    # deactivate timestamp if existing project
    observe({
        if(existing) {
            # deactivate timestamp button
            shinyjs::disable("timestamp")
        }
    })
    # update project name if initial value is given
    observeEvent(project_name$name, {
        req(project_name$name)
        updateTextInput(session, "project_name", value = project_name$name)
    })
    # project name update
    observeEvent(input$project_name, {
        out$name <- input$project_name
    })
    # update timestamp depending on initial project name value
    observe({
        if(input$timestamp & !existing) {
            out$fullname <- paste0(out$name, "_", local$timestamp)
        } else {
            out$fullname <- out$name
        }
    })
    # render project full name
    output$project_fullname <- renderText({ 
        out$fullname
    })
    # output
    return(out)
}