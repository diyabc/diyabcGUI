#' Project naming module ui
#' @keywords internal
#' @author Ghislain Durif
proj_name_ui <- function(id, label = "Project name") {
    ns <- NS(id)
    tagList(
        h3(label),
        fluidRow(
            column(
                width = 9,
                textInput(
                    ns("proj_name"), 
                    label = NULL, 
                    placeholder = "project name"
                )
            ),
            column(
                width = 3,
                actionButton(
                    ns("validate"),
                    label = "Validate",
                    icon = icon("check"),
                    width = '100%'
                )
            )
        ),
        uiOutput(ns("feedback"))
    )
}

#' Project naming module server
#' @keywords internal
#' @author Ghislain Durif
#' @param tag character string, type of project identified by `"ap"` 
#' (for diyabc-rf analysis project) or `"dp"` (data generation project).
proj_name_server <- function(input, output, session, tag = "ap") {
    
    # init local
    local <- reactiveValues(modified = TRUE)
    
    # new input
    observeEvent(input$proj_name, {
        # shinyjs::enable("validate")
        local$modified <- TRUE
    })
    
    # validate input
    observeEvent(input$validate, {
        req(input$validate)
        
        local$modified <- FALSE

        # check project name
        if(isTruthy(input$proj_name)) {
            # shinyjs::disable("validate")
            env[[tag]]$proj_name <<- input$proj_name
        }
    })
    
    # # debugging
    # observe({
    #     logging("proj name:", env[[tag]]$proj_name)
    # })
    
    # feedback on project name
    observe({
        feedbackWarning(
            "proj_name", 
            local$modified || !isTruthy(input$proj_name), 
            "Project name is missing or not validated."
        )
    })
}
