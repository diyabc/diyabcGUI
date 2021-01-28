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
                status = "warning", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE,
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
                    label = "Start",
                    icon = icon("play-circle"),
                    width = "100%"
                )
            ),
            box(
                title = "Synthetic data file generation",
                width = 12, 
                status = "info", solidHeader = TRUE,
                collapsible = TRUE, collapsed = FALSE,
                tags$div(
                    tags$p(
                        "Direct use of DIYABC-RF simulation engine",
                        "to generate pseudo-oberved datasets."
                    ) %>% 
                        helper(type = "markdown", 
                               content = "data_simulation")
                ),
                actionButton(
                    ns("new_datagen_project"),
                    label = "Start",
                    icon = icon("play-circle"),
                    width = "100%"
                )
            ),
            box(
                title = tags$div(icon("info-circle"), "Help center"),
                width = 12,
                collapsible = FALSE,
                tagList(
                    tags$p(
                        "Check the documentation at",
                        tags$a(
                            "DIYABC-RF GUI official website", 
                            href="https://diyabc.github.io/"
                        ),
                        "."
                    ),
                    tags$p(
                        "If you encounter any issue, please visit",
                        tags$a(
                            "DIYABC-RF GUI issue tracker", 
                            href="https://github.com/diyabc/diyabcGUI/issues"
                        ),
                        "."
                    ),
                    hr(),
                    tags$p(
                        "Help inside the application:", br(),
                        tags$ul(
                            tags$li(
                                "Specific help pop-ups relative to some panels",
                                "are directly available inside",
                                "the application by clicking on",
                                "the close-by", icon("question-circle"), "."
                            ),
                            br(),
                            tags$li(
                                "Reactive feedback is also",
                                "provided by the different panels",
                                "of the application.",
                                tags$i("Warning"), "messages are idenfied by a", 
                                icon("warning"), "and",
                                tags$i("informative"), "messages by a", 
                                icon("comment"), "."
                            )
                        )
                    )
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
        new_datagen_project = NULL
    )
    # analysis project reactivity
    observeEvent(input$new_analysis_project, {
        out$new_analysis_project <- ifelse(
            !is.null(out$new_analysis_project),
            out$new_analysis_project,
            0
        ) + 1
    })
    # datagen project reactivity
    observeEvent(input$new_datagen_project, {
        out$new_datagen_project <- ifelse(
            !is.null(out$new_datagen_project),
            out$new_datagen_project,
            0
        ) + 1
    })
    # output
    return(out)
}

#' Home page ui
#' @keywords internal
#' @author Ghislain Durif
#' @description deprecated
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
#' @description deprecated
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
