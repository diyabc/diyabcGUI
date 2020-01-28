#' Text module input function
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
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
#' @import shiny
text_module <- function(input, output, session) {
    output$text_value <- renderText({ input$text })
}

#' Directory choice module input function
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
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
#' @import shiny
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