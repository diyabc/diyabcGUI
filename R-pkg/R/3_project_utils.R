#' New project setting ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyWidgets actionGroupButtons
new_proj_set_ui <- function(id) {
    ns <- NS(id)
    tagList(
        proj_name_ui(ns("project_name"), label = "Project",
                     default = "project_name"),
        dir_choice_ui(ns("project_dir"), label = "Directory"),
        fluidRow(
            column(
                width = 6,
                actionGroupButtons(
                    inputIds = c(ns("validate"), 
                                 ns("reset")),
                    labels = list(
                        tags$span(icon("check"), "Validate"),
                        tags$span(icon("refresh"), "Reset")
                    ),
                    fullwidth = TRUE
                )
            ),
            column(
                width = 6,
                actionGroupButtons(
                    inputIds = c(ns("duplicate"), 
                                 ns("close")),
                    labels = list(
                        tags$span(icon("copy"), "Duplicate"),
                        tags$span(icon("window-close"), "Close")
                    ),
                    fullwidth = TRUE
                )
            )
        )
    )
}

#' New project setting server function
#' @keywords internal
#' @author Ghislain Durif
new_proj_set_server <- function(input, output, session) {
    # init local
    local <- reactiveValues(
        enabled = TRUE
    )
    # init output reactive values
    out <- reactiveValues(
        project_name = NULL,
        project_dir = NULL,
        validate = NULL,
        reset = NULL,
        duplicate = NULL,
        close = NULL
    )
    # project name server side
    proj_name <- callModule(proj_name_server, "project_name",
                            enabled = reactive(local$enabled))
    observeEvent(proj_name$name, {
        out$project_name <- proj_name$name
    })
    # parent folder server side
    parent_folder <- callModule(dir_choice_server, "project_dir",
                                enabled = reactive(local$enabled))
    ## project directory
    observe({
        out$project_dir <- file.path(parent_folder$path,
                                     proj_name$fullname)
    })
    ## actions
    observeEvent(input$validate, {
        local$enabled <- FALSE
        out$validate <- ifelse(is.null(out$validate), 0, out$validate) + 1
    })
    observeEvent(input$reset, {
        local$enabled <- TRUE
        out$reset <- ifelse(is.null(out$reset), 0, out$reset) + 1
    })
    observeEvent(input$duplicate, {
        out$duplicate <- ifelse(is.null(out$duplicate), 0, out$duplicate) + 1
    })
    observeEvent(input$close, {
        out$close <- ifelse(is.null(out$close), 0, out$close) + 1
    })
    # output
    return(out)
}