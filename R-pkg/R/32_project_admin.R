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
proj_admin_server <- function(input, output, session,
                              proj_dir = reactive({NULL}),
                              proj_name = reactive({NULL})) {
    # init local
    local <- reactiveValues(
        proj_dir = NULL,
        proj_name = NULL
    )
    # get input
    observe({
        local$proj_dir <- proj_dir()
        local$proj_name <- proj_name()
    })
    # init output
    out <- reactiveValues(reset = NULL)
    # save
    output$save <- downloadHandler(
        filename = function() {
            file_name <- "project_name.zip"
            if(!is.null(local$proj_name)) {
                if(str_length(local$proj_name) > 0) {
                    file_name <- str_c(local$proj_name, ".zip")
                }
            }
            return(file_name)
        },
        content = function(file) {
            wd <- getwd()
            on.exit(setwd(wd))
            setwd(local$proj_dir)
            cleanup_diyabc_run(local$proj_dir)
            cleanup_abcranger_run(local$proj_dir)
            zip::zip(file, list.files(local$proj_dir))
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
        req(!is.null(input$reset_confirmation))
        if(isTRUE(input$reset_confirmation)) {
            out$reset <- ifelse(!is.null(out$reset), out$reset, 0) + 1
        }
    })
    
    ## output
    return(out)
}