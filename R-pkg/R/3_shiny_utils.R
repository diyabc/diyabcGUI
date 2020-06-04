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
#' @param enabled boolean, enable input or not, as a `reactive`.
#' @importFrom fs path_home
#' @importFrom shinyFiles parseDirPath shinyDirChoose
#' @importFrom shinyjs disable enable
dir_choice_server <- function(input, output, session, 
                              enabled = reactive({TRUE})) {
    # init local
    local <- reactiveValues(enabled = NULL)
    # temp dir
    temp_dir <- tempfile("diyabc")
    dir.create(temp_dir, showWarnings = FALSE)
    # init output
    out <- reactiveValues(path = temp_dir)
    # local volumes
    volumes <- c(home = fs::path_home(), wd = temp_dir)
    # get input
    observe({
        local$enabled <- enabled()
    })
    # enable/disable ?
    observeEvent(local$enabled, {
        req(!is.null(local$enabled))
        if(local$enabled) {
            shinyjs::enable("dir_choice")
        } else {
            shinyjs::disable("dir_choice")
        }
    })
    # directory chooser
    shinyDirChoose(
        input,
        id = "dir_choice",
        roots = volumes,
        defaultRoot = "wd"
    )
    # print path
    output$dir_value <- renderText({
        out$path
    })
    # update path
    observeEvent(input$dir_choice, {
        req(input$dir_choice)
        out$path <- parseDirPath(volumes, input$directory)
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
#' @param enabled boolean, enable input or not, as a `reactive`.
#' @importFrom lubridate today
#' @importFrom shinyjs disable
#' @importFrom stringr str_detect str_extract
proj_name_server <- function(input, output, session,
                             enabled = reactive({TRUE})) {
    # init local reactive values
    local <- reactiveValues(enabled = NULL, timestamp = lubridate::today())
    # get input
    observe({
        local$enabled <- enabled()
    })
    # enable/disable ?
    observeEvent(local$enabled, {
        req(!is.null(local$enabled))
        if(local$enabled) {
            shinyjs::enable("project_name")
            shinyjs::enable("timestamp")
        } else {
            shinyjs::disable("project_name")
            shinyjs::disable("timestamp")
        }
    })
    # init output reactive values
    out <- reactiveValues(
        name = NULL,
        fullname = NULL
    )
    # project name update
    observeEvent(input$project_name, {
        out$name <- input$project_name
    })
    # update timestamp depending on initial project name value
    observe({
        req(!is.null(input$timestamp))
        if(input$timestamp) {
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