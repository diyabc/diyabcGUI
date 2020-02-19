#' Numeric module input function
#' @keywords internal
#' @author Ghislain Durif
numeric_input <- function(id, label = "Value", default = 0) {
    ns <- NS(id)
    tagList(
        numericInput(
            inputId = ns("num"), 
            label = label, 
            value = default
        )
    )
}

#' numeric module server function
#' @keywords internal
#' @author Ghislain Durif
numeric_module <- function(input, output, session) {}

#' Text area module input function
#' @keywords internal
#' @author Ghislain Durif
text_area_input <- function(id, label = "Text", default = "Your text", 
                            height = '200px') {
    ns <- NS(id)
    tagList(
        textAreaInput(
            inputId = ns("text"), 
            label = label, 
            value = default,
            height = height
        ),
        verbatimTextOutput(
            ns("text_value"), 
            placeholder = TRUE
        )
    )
}

#' Text area module server function
#' @keywords internal
#' @author Ghislain Durif
text_area_module <- function(input, output, session) {
    output$text_value <- renderText({ input$text })
}

#' Text module input function
#' @keywords internal
#' @author Ghislain Durif
text_input <- function(id, label = "Text", default = "Your text") {
    ns <- NS(id)
    tagList(
        textInput(
            inputId = ns("text"), 
            label = label, 
            value = default
        ),
        verbatimTextOutput(
            ns("text_value"), 
            placeholder = TRUE
        )
    )
}

#' Text module server function
#' @keywords internal
#' @author Ghislain Durif
text_module <- function(input, output, session) {
    output$text_value <- renderText({ input$text })
}

#' Directory choice module input function
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyFiles shinyDirButton
dir_input <- function(id, label = "Directory") {
    ns <- NS(id)
    tagList(
        shinyDirButton(
            ns("dir"), label = label, title = "Upload"
        ),
        verbatimTextOutput(
            ns("dir_value"), placeholder = TRUE
        )
    )
}

#' Directory choice module server function
#' @keywords internal
#' @author Ghislain Durif
dir_module <- function(input, output, session) {
    shinyDirChoose(
        input,
        id = "dir",
        roots = c(home = '~'),
        filetypes = c("")
    )
    
    global <- reactiveValues(datapath = getwd())
    
    dir <- reactive(input$dir)
    
    output$dir_value <- renderText({
        global$datapath
    })
    
    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                     input$dir
                 },
                 handlerExpr = {
                     req(is.list(input$dir))
                     home <- normalizePath("~")
                     global$datapath <-
                         file.path(home, paste(unlist(dir()$path[-1]), 
                                               collapse = .Platform$file.sep))
                 })
}


#' Project module input function
#' @keywords internal
#' @author Ghislain Durif
project_input <- function(id, label = "Project", default = "Your project") {
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

#' Project module server function
#' @keywords internal
#' @importFrom lubridate today
#' @author Ghislain Durif
project_module <- function(input, output, session) {
    
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
}