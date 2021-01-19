#' Project naming module ui
#' @keywords internal
#' @author Ghislain Durif
proj_name_ui <- function(id, label = "Project name", default = NULL) {
    ns <- NS(id)
    tagList(
        h3(label),
        fluidRow(
            column(
                width = 9,
                textInput(
                    ns("proj_name"), 
                    label = NULL, 
                    value = ifelse(
                        !is.null(default),
                        default,
                        ""
                    ),
                    placeholder = "project name"
                )
            ),
            column(
                width = 3,
                actionButton(
                    ns("validate_proj_name"),
                    label = "Validate",
                    icon = icon("check"),
                    width = '100%'
                )
            )
        ),
        uiOutput(ns("feedback_proj_name"))
    )
}

#' Project naming module server
#' @keywords internal
#' @author Ghislain Durif
proj_name_server <- function(input, output, session) {
    
    # init local
    local <- reactiveValues(
        modified_proj_name = FALSE,
        valid_proj_name = FALSE
    )
    
    # init output
    out <- reactiveValues(proj_name = NULL, valid_proj_name = FALSE)
    
    # new input
    observeEvent(input$proj_name, {
        shinyjs::enable("validate_proj_name")
        local$modified_proj_name <- TRUE
        
        # check project name
        local$valid_proj_name <- FALSE
        if(!is.null(input$proj_name)) {
            if(str_length(input$proj_name) > 0) {
                local$valid_proj_name <- TRUE
            }
        }
    })
    
    # validate input
    observeEvent(input$validate_proj_name, {
        req(input$validate_proj_name)
        
        local$modified_proj_name <- FALSE
        
        out$proj_name <- input$proj_name
        
        out$valid_proj_name <- local$valid_proj_name
        
        shinyjs::disable("validate_proj_name")
    })
    
    # valid proj name ?
    observeEvent(local$valid_proj_name, {
        req(!is.null(local$valid_proj_name))
        
        if(!local$valid_proj_name) {
            shinyjs::enable("validate_proj_name")
        }
    })
    
    # feedback on project name
    output$feedback_proj_name <- renderUI({
        feedback<- tagList()
        
        # check if modified proj name
        if(!is.null(local$modified_proj_name)) {
            if(local$modified_proj_name) {
                feedback <- tagAppendChild(
                    feedback,
                    helpText(
                        icon("warning"), 
                        "Project name is not validated."
                    )
                )
            }
        }
        
        # check proj name
        if(!is.null(local$valid_proj_name)) {
            if(!local$valid_proj_name) {
                feedback <- tagAppendChild(
                    feedback,
                    helpText(
                        icon("warning"), "Project name is missing."
                    )
                )
            }
        } else {
            
        }
        
        # feedback
        feedback
    })
    
    # output
    return(out)
}
