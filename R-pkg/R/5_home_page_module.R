#' Simplified home page ui
#' @keywords internal
#' @author Ghislain Durif
simplified_home_page_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "DIYABC-RF main pipeline",
                width = 12, 
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                tags$div(
                    tags$p("Including the two modules of DIYABC Random Forest:"),
                    tags$ul(
                        tags$li("training set simulation"),
                        tags$li("random forest analyses")
                    )
                ) %>% 
                    helper(type = "markdown", 
                           content = "data_analysis"),
                actionButton(
                    ns("new_analysis_project"),
                    label = "New project",
                    icon = icon("folder-open"),
                    width = "100%"
                )
            )
        ),
        fluidRow(
            box(
                title = "Synthetic data file generation",
                width = 12, 
                status = "info", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                tags$div(
                    tags$p(
                        "Direct use of DIYABC-RF simulation engine",
                        "to generate pseudo-oberved datasets."
                    ) %>% 
                        helper(type = "markdown", 
                               content = "data_simulation")
                ),
                actionButton(
                    ns("new_simu_project"),
                    label = "New project",
                    icon = icon("folder-open"),
                    width = "100%"
                )
            )
        )
    )
}

#' Simlified home page server
#' @keywords internal
#' @author Ghislain Durif
simplified_home_page_server <- function(input, output, session) {
    # init output
    out <- reactiveValues(
        new_analysis_project = NULL,
        new_simu_project = NULL
    )
    # analysis project reactivity
    observeEvent(input$new_analysis_project, {
        out$new_analysis_project <- ifelse(
            !is.null(out$new_analysis_project),
            out$new_analysis_project,
            0
        ) + 1
    })
    # simu project reactivity
    observeEvent(input$new_simu_project, {
        out$new_simu_project <- ifelse(
            !is.null(out$new_simu_project),
            out$new_simu_project,
            0
        ) + 1
    })
    # output
    return(out)
}

#' Home page ui
#' @keywords internal
#' @author Ghislain Durif
home_page_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                title = "Data analysis",
                width = 12, 
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                tags$div(
                    tags$p("Including the two modules of DIYABC Random Forest:"),
                    tags$ul(
                        tags$li("training set simulation"),
                        tags$li("random forest analyses")
                    )
                ) %>% 
                    helper(type = "markdown", 
                           content = "data_analysis"),
                open_project_ui(ns("analysis_project"))
            )
        ),
        fluidRow(
            box(
                title = "Pseudo-observed dataset simulation",
                width = 12, 
                status = "info", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                tags$div(
                    tags$p("Direct use of DIYABC-RF simulation engine.") %>% 
                        helper(type = "markdown", 
                               content = "data_simulation")
                ),
                open_project_ui(ns("simu_project"))
            )
        )
    )
}

#' Home page server
#' @keywords internal
#' @author Ghislain Durif
home_page_server <- function(input, output, session) {
    # init output
    out <- reactiveValues(
        new_analysis_project = NULL,
        existing_analysis_project = NULL,
        new_simu_project = NULL,
        existing_simu_project = NULL
    )
    # input
    analysis_project <- callModule(open_project_server, "analysis_project")
    simu_project <- callModule(open_project_server, "simu_project")
    # analysis project reactivity
    observeEvent(analysis_project$new, {
        out$new_analysis_project <- analysis_project$new
    })
    observeEvent(analysis_project$existing, {
        out$existing_analysis_project <- analysis_project$existing
    })
    # simu project reactivity
    observeEvent(simu_project$new, {
        out$new_simu_project <- simu_project$new
    })
    observeEvent(simu_project$existing, {
        out$existing_simu_project <- simu_project$existing
    })
    # output
    return(out)
}

#' Open project module ui
#' @description
#' Wrapper to choose between creating a new project and opening an existing one 
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyWidgets actionGroupButtons
open_project_ui <- function(id, label = "New project") {
    ns <- NS(id)
    tagList(
        actionGroupButtons(
            inputIds = c(ns("new_project"), 
                         ns("existing_project")),
            labels = list(
                tags$span(icon("plus"), "New project"),
                tags$span(icon("folder-open"), "Existing project")
            ),
            fullwidth = TRUE
        )
    )
}

#' Open project module server
#' @keywords internal
#' @author Ghislain Durif
open_project_server <- function(input, output, session) {
    # init output
    out <- reactiveValues(new = NULL, existing = NULL)
    # react to new project request
    observeEvent(input$new_project, {
        out$new <- ifelse(is.null(out$new), 0, out$new) + 1
    })
    # react to open existing project request
    observeEvent(input$existing_project, {
        out$existing <- ifelse(is.null(out$existing), 0, out$existing) + 1
    })
    # output
    return(out)
}
