#' Simplified home page ui
#' @keywords internal
#' @author Ghislain Durif
home_page_ui <- function(id) {
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
home_page_server <- function(input, output, session) {
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
