#' Project administration ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyWidgets downloadBttn
proj_admin_ui <- function(id) {
    ns <- NS(id)
    tagList(
        downloadBttn(
            ns("save"),
            label = "Save",
            style = "fill",
            color = "primary",
            block = TRUE
        ),
        helpText(
            icon("clock"), 
            "Preparing the zip file to save your project may take some time.",
            br(),  br(),
            icon("warning"), 
            "Depending on the configuration of your web browser,",
            "you may or may not be able to choose where the project zip file", 
            "will be saved on your computer. If you cannot choose, the file",
            "will be saved to a default location, like the",
            tags$code("Downloads"), "folder in your home directory."
        ),
        hr(),
        actionBttn(
            inputId = ns("reset"),
            label = tags$span(icon("refresh"), "Reset"),
            style = "fill",
            block = TRUE,
            color = "danger"
        )
    )
}

#' Project administration server
#' @keywords internal
#' @author Ghislain Durif
#' @param tag character string, type of project identified by `"ap"` 
#' (for diyabc-rf analysis project) or `"dp"` (data generation project).
proj_admin_server <- function(input, output, session, tag = NULL) {
    
    # init output
    out <- reactiveValues(reset = NULL)
    
    # check input
    if(isTruthy(tag)) {
        
        # save
        output$save <- downloadHandler(
            filename = function() {
                file_name <- "project_name.zip"
                if(isTruthy(env[[tag]]$proj_name)) {
                    file_name <- str_c(env[[tag]]$proj_name, ".zip")
                }
                return(file_name)
            },
            content = function(file) {
                req(env[[tag]]$proj_dir)
                wd <- getwd()
                on.exit(setwd(wd))
                setwd(env[[tag]]$proj_dir)
                cleanup_diyabc_run(env[[tag]]$proj_dir)
                cleanup_abcranger_run(env[[tag]]$proj_dir)
                zip::zip(file, list.files(env[[tag]]$proj_dir))
            }
        )
        
        ## reset
        observeEvent(input$reset, {
            ask_confirmation(
                inputId = "reset_confirmation",
                title = "Want to confirm ?"
            )
        })
        
        observeEvent(input$reset_confirmation, {
            req(input$reset_confirmation)
            out$reset <- ifelse(!is.null(out$reset), out$reset, 0) + 1
        })
    }
    
    ## output
    return(out)
}