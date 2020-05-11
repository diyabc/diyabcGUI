#' Training set sub-module ui
#' @keywords internal
#' @author Ghislain Durif
training_set_ui <- function(id) {
    ns <- NS(id)
    tagList(
        hist_model_panel_ui(ns("hist_model_panel"))
    )
}

#' Training set sub-module server
#' @keywords internal
#' @author Ghislain Durif
#' @param project_dir project directory as a `reactive`.
#' @param project_name project name as a `reactive`.
#' @param scenario_list list of raw scenarii as a `reactive`.
training_set_server <- function(input, output, session,
                                project_dir = reactive({NULL}),
                                project_name = reactive({NULL}),
                                scenario_list = reactive({NULL})) {
    # init local
    local <- reactiveValues(
        project_dir = NULL,
        project_name = NULL,
        scenario_list = NULL
    )
    # get input
    observe({
        local$project_dir = project_dir()
        local$project_name = project_name()
        local$scenario_list = scenario_list()
    })
    # init output
    out <- reactiveValues()
    # historic model panel
    callModule(hist_model_panel_server, "hist_model_panel",
               project_dir = reactive(local$project_dir))
    # output
    return(out)
}

#' Historical model input panel module ui
#' @keywords internal
#' @author Ghislain Durif
hist_model_panel_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(
                width = 6,
                actionButton(
                    ns("add"), 
                    label = tags$span(icon("plus"), "Add")
                )
            ),
            column(
                width = 6,
                uiOutput(ns("scenario_nb"))
            )
        ),
        br(),
        tabsetPanel(id = ns("scenario_tabs"))
    )
}

#' Historical model input panel module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyjs onclick
#' @importFrom shinyWidgets actionBttn
#' @importFrom stringr str_c
hist_model_panel_server <- function(input, output, session,
                                    project_dir = reactive({NULL}), 
                                    raw_scenario_list = reactive({NULL})) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        count = NULL,
        project_dir = NULL,
        raw_scenario_list = NULL,
        scenario_list = list()
    )
    # get input
    observe({
        local$project_dir <- project_dir()
        # FIXME raw_scenario_list
    })
    # init output
    out <- reactiveValues(scenario_list = NULL)
    
    # add scenario
    observeEvent(input$add, {
        # increment count
        local$count <- ifelse(is.null(local$count), 0, local$count) + 1
        # id
        id <- local$count
        # add new tab
        appendTab(
            inputId = "scenario_tabs",
            tabPanel(
                title = closable_tab_title(id, 
                                           label = str_c("Scenario ", id),
                                           ns = ns),
                value = id,
                tags$br(),
                hist_model_ui(ns(str_c("model", id)))
            ),
            select = TRUE
        )
        # observe closing
        observe({
            shinyjs::onclick(id = str_c("close", id), {
                removeTab(inputId = "scenario_tabs", target = str_c(id))
                local$scenario_list[[ id ]] <<- NULL
            })
        })
        # module function
        local$scenario_list[[ id ]] <<- callModule(
            hist_model_server, str_c("model", id),
            project_dir = reactive(local$project_dir), 
            raw_scenario = reactive(local$raw_scenario)
        )
    })
    
    # update output
    observe({
        out$scenario_list <- local$scenario_list
        
        if(length(local$scenario_list) > 0) {
            print(local$scenario_list[[1]]$raw)
            print(local$scenario_list[[1]]$param)
        }
    })
    
    ## output
    return(out)
}

#' Return closable panel title
#' @keywords internal
#' @author Ghislain Durif
closable_tab_title <- function(id, label, ns) {
    tags$span(
        label,
        HTML("&nbsp;"),
        tags$span(
            actionButton(
                ns(str_c("close", id)), 
                label = NULL,
                icon = icon("close")
            )
        )
    )
}
