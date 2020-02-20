#' Directory choice module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyFiles shinyDirButton
dir_input_module_ui <- function(id, label = "Directory") {
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

#' Directory choice module server
#' @keywords internal
#' @author Ghislain Durif
dir_input_module_server <- function(input, output, session) {
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
}