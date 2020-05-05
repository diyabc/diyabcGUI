#' Project module ui
#' @description
#' Wrapper to choose between creating a new project and opening an existing one 
#' @keywords internal
#' @author Ghislain Durif
project_module_ui <- function(id, label = "New project") {
    ns <- NS(id)
    tagList(
        actionGroupButtons(
            inputIds = c(ns("create_project"), 
                         ns("open_project")),
            labels = list("New project", "Existing project"),
            fullwidth = TRUE
        ),
        uiOutput(ns("new_project"))
    )
}

#' Project module server
#' @keywords internal
#' @author Ghislain Durif
project_module_server <- function(input, output, session) {
    # session
    ns <- session$ns
    # init local values
    local <- reactiveValues(ui = NULL)
    # init output values
    out <- reactiveValues(
        new_project = NULL,
        open_project = NULL
    )
    # create new project
    observeEvent(input$create_project, {
        local$ui <- create_project_module_ui(ns("creating_project"),
                                             label = "Project name")
        output$new_project <- renderUI({
            local$ui
        })
        project <- callModule(create_project_module_server,
                              "creating_project")
        # save output
        out$new_project <- project
    })
    # open existing analysis project
    observeEvent(input$open_project, {
        local$ui <- open_project_module_ui(ns("opening_project"),
                                           label = "Project directory")
        output$new_project <- renderUI({
            local$ui
        })
        project <- callModule(open_project_module_server,
                              "opening_project")
        # save output
        out$open_project <- project
    })
    # output
    return(out)
}

#' Create project module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyjs disabled
create_project_module_ui <- function(id, label = "New project") {
    ns <- NS(id)
    tagList(
        tags$br(),
        textInput(ns("project_name"), label = label),
        shinyjs::disabled(
            actionButton(ns("create"), label = "Create")
        )
    )
}

#' Create project module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyjs disable enable
#' @importFrom stringr str_length
create_project_module_server <- function(input, output, session) {
    # init local values
    local <- reactiveValues()
    # disable button if empty input name
    observe({
        if(is.null(input$project_name)) {
            shinyjs::disable("create")
        } else if(str_length(input$project_name) > 0) {
            shinyjs::enable("create")
        } else {
            shinyjs::disable("create")
        }
    })
    # check for click
    observe({
        local$project_name <- input$project_name
        local$create_button <- input$create
    })
    # output
    return(local)
}

#' Open project module ui
#' @keywords internal
#' @author Ghislain Durif
open_project_module_ui <- function(id, label = "Open project") {
    ns <- NS(id)
    tagList(
        tags$br(),
        dir_input_module_ui(ns("project_dir"), label = label, 
                            title = "Choose a directory"),
        actionButton(ns("open"), label = "Open"),
        uiOutput(ns("project_status"))
    )
}

#' Open project module server
#' @keywords internal
#' @author Ghislain Durif
#' @param project_type string, type of the project 'analysis' or 'simulation'
#' @param headerfile string, header file name 'header.txt' or 'headersim.txt'
#' @importFrom shinyjs toggleState disable
open_project_module_server <- function(input, output, session, 
                                       project_type = "analysis",
                                       headerfile = "header.txt") {
    # namespace
    ns <- session$ns
    # init local values
    local <- reactiveValues()
    # path
    tmp_dir <- callModule(dir_input_module_server, "project_dir")
    # check project
    observe({
        local$project_path <- tmp_dir$path
        local$project_status <- headerfile %in% list.files(tmp_dir$path)
        tmp_check <- project_path2name(tmp_dir$path)
        local$project_name <- tmp_check$project_name
        local$timestamp <- tmp_check$timestamp
    })
    # notification about project directory
    output$project_status <- renderUI({
        out <- NULL
        if(!local$project_status) {
            out <- tagList(
                tags$br(),
                tags$p(
                    icon("warning"), 
                    paste0("Chosen directory is not a DIYABC-RF ", 
                           project_type, " directory.")
                )
            )
            shinyjs::disable("open") # because of issue with toggleState
        }
        out
    })
    # click
    observe({
        shinyjs::toggleState("open", condition = local$project_status)
        local$open_button <- input$open
    })
    # output
    return(local)
}
