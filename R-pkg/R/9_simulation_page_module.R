#' Simulation page ui
#' @keywords internal
#' @author Ghislain Durif
simu_page_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "Project settings",
                width = 12,
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                simu_proj_set_ui(ns("proj_set")) %>% 
                    helper(type = "markdown", 
                           content = "simulation_project")
            )
        ),
        fluidRow(
            box(
                title = "Historical model",
                width = 12,
                status = "info", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                simu_hist_model_ui(ns("hist_model"))
            )
        ),
        fluidRow(
            box(
                title = "Genetic data",
                width = 12,
                status = "warning", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                "FILLME"
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

#' Simulation page server
#' @keywords internal
#' @author Ghislain Durif
#' @param project_dir project directory as a `reactive`.
#' @param project_name project name as a `reactive`.
#' @param raw_scenario raw scenario as a `reactive`.
simu_page_server <- function(input, output, session,
                             project_dir = reactive({NULL}),
                             project_name = reactive({NULL}),
                             raw_scenario = reactive({NULL})) {
    # init local
    local <- reactiveValues(
        project_dir = NULL,
        project_name = NULL,
        raw_scenario = NULL
    )
    # get input
    observe({
        local$project_dir = project_dir()
        local$project_name = project_name()
        local$raw_scenario = raw_scenario()
    })
    # init output
    out <- reactiveValues(
        setting = NULL,
        scenario = NULL
    )
    ## project setting
    setting <- callModule(simu_proj_set_server, "proj_set")
    # update local
    observe({
        local$project_dir <- setting$project_dir
        local$project_name <- setting$project_name
    })
    # update output
    observe({
        out$setting <- setting
    })
    ## historical model
    scenario <- callModule(simu_hist_model_server, "hist_model",
                           project_dir = reactive(local$project_dir),
                           project_name = reactive(local$name),
                           raw_scenario = reactive(local$raw_scenario))
    # update local
    observe({
        local$raw_scenario <- scenario$raw_scenario
    })
    # update output
    observe({
        out$scenario <- scenario
    })
    # output
    return(out)
}

#' Simulation project setting ui
#' @keywords internal
#' @author Ghislain Durif
simu_proj_set_ui <- function(id) {
    ns <- NS(id)
    tagList(
        proj_name_ui(ns("project_name"), label = "Project",
                     default = "project_name"),
        dir_choice_ui(ns("project_dir"), label = "Directory")
    )
}

#' Simulation project setting server function
#' @keywords internal
#' @author Ghislain Durif
#' 
#' @importFrom shinyjs disable enable
simu_proj_set_server <- function(input, output, session) {
    # init output reactive values
    out <- reactiveValues(
        project_name = NULL,
        project_dir = NULL
    )
    # project name server side
    proj_name <- callModule(proj_name_server, "project_name")
    observeEvent(proj_name$name, {
        out$project_name <- proj_name$name
    })
    # parent folder server side
    parent_folder <- callModule(dir_choice_server, "project_dir")
    # project directory
    observe({
        out$project_dir <- file.path(parent_folder$path,
                                     proj_name$fullname)
    })
    # output
    return(out)
}

#' Simulation historical model ui
#' @keywords internal
#' @author Ghislain Durif
simu_hist_model_ui <- function(id) {
    ns <- NS(id)
    tagList(
        hist_model_ui(ns("hist_model"))
    )
}

#' Simulation historical model server
#' @keywords internal
#' @author Ghislain Durif
#' @param project_dir project directory as a `reactive`.
#' @param project_name project name as a `reactive`.
#' @param raw_scenario raw scenario as a `reactive`.
#' @importFrom shinyjs disable enable
simu_hist_model_server <- function(input, output, session,
                                   project_dir = reactive({NULL}),
                                   project_name = reactive({NULL}),
                                   raw_scenario = reactive({NULL})) {
    # init local
    local <- reactiveValues(
        project_dir = NULL,
        project_name = NULL,
        raw_scenario = NULL
    )
    # get input
    observe({
        local$project_dir = project_dir()
        local$project_name = project_name()
        local$raw_scenario = raw_scenario()
    })
    # init output
    out <- reactiveValues(
        raw_scenario = NULL,
        scenario_param = NULL
    )
    # define model
    scenario <- callModule(hist_model_server, "hist_model",
                           project_dir = reactive(local$project_dir), 
                           raw_scenario = reactive(local$raw_scenario))
    # update output
    observe({
        out$raw_scenario <- scenario$raw
        out$scenario_param <- scenario$param
    })
    # output
    return(out)
}
