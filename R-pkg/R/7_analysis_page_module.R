#' Analysis page ui
#' @keywords internal
#' @author Ghislain Durif
analysis_page_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "Project settings",
                width = 12,
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                new_proj_set_ui(ns("proj_set")) %>% 
                    helper(type = "markdown", 
                           content = "simulation_project")
            )
        ),
        fluidRow(
            box(
                title = "Training set simulations",
                width = 12,
                status = "info", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                training_set_ui(ns("train_set"))
            )
        ),
        fluidRow(
            box(
                title = "Random Forest Analyses",
                width = 12,
                status = "warning", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                "FILL ME"
            )
        )
        ,
        fluidRow(
            box(
                title = "Project action",
                width = 12,
                status = "danger", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE,
                "FILLME"
            )
        )
    )
}

#' Analysis page server
#' @keywords internal
#' @author Ghislain Durif
#' @param project_dir project directory as a `reactive`.
#' @param project_name project name as a `reactive`.
#' @param scenario_list list of raw scenarii as a `reactive`.
analysis_page_server <- function(input, output, session,
                                 project_dir = reactive({NULL}),
                                 project_name = reactive({NULL}),
                                 scenario_list = reactive({NULL})) {
    # namespace
    ns <- session$ns
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
    out <- reactiveValues(
        setting = NULL,
        scenario = NULL
    )
    ## project setting
    setting <- callModule(new_proj_set_server, "proj_set")
    # update local
    observe({
        local$project_dir <- setting$project_dir
        local$project_name <- setting$project_name
    })
    # update output
    observe({
        out$setting <- setting
    })
    ## Training set sub-module
    training_set <- callModule(training_set_server, "train_set")
    # output
    return(out)
}
