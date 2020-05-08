#' Graph display module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard box
graph_display_ui <- function(id) {
    ns <- NS(id)
    fluidRow(
        column(
            width = 6,
            verticalLayout(
                plotOutput(
                    ns("display")
                ),
                actionButton(ns("save"), label = "Save")  %>% 
                    helper(
                        type = "inline", 
                        content = paste0(
                            "Check 'Image settings' box to change saved ",
                            "image parameters.")
                    ),
                br()
            ),
        ),
        box(
            title = "Image settings",
            width = 6,
            collapsible = TRUE,
            collapsed = TRUE,
            textInput(
                ns("filename"),
                value = "Rplot.eps",
                label = "Filename"
            ) %>% 
                helper(
                    type = "inline", 
                    content = paste0(
                        "Figures are saved in 'fig' sub-folder in ",
                        "project directory. ", 
                        "Possible extensions are: ", 
                        "'eps', 'ps', 'tex', 'pdf', 'jpeg', ", 
                        "'tiff', 'png', 'bmp', 'svg'")
                ),
            numericInput(
                ns("graph_scale"), 
                label = "Graph scale", 
                value = 1, min = 1
            ) %>% 
                helper(
                    type = "inline", 
                    content = paste0(
                        "Multiplicative scaling factor.")
                ),
            numericInput(
                ns("graph_width"), 
                label = "Graph width", 
                value = 50, min = 1
            ) %>% 
                helper(
                    type = "inline", 
                    content = paste0(
                        "Output figure width in 'units'.")
                ),
            numericInput(
                ns("graph_height"), 
                label = "Graph height", 
                value = 50, min = 1
            ) %>% 
                helper(
                    type = "inline", 
                    content = paste0(
                        "Output figure height in 'units'.")
                ),
            selectInput(
                ns("size_unit"), 
                label = "Unit", 
                choices = list("mm" = "mm", 
                               "cm" = "cm", 
                               "in" = "in"), 
                selected = "mm"
            ) %>% 
                helper(
                    type = "inline", 
                    content = paste0(
                        "Size units.")
                ), 
            numericInput(
                ns("graph_dpi"), 
                label = "Graph resolution", 
                value = 320, min = 1
            ) %>% 
                helper(
                    type = "inline", 
                    content = "Image resolution."
                )
        )
    )
}


#' Graph display module server
#' @keywords internal
#' @author Ghislain Durif
#' @param graph a `ggplot2` graph.
#' @param project_dir `reactiveValues` with `path` attribute.
#' @importFrom shiny showNotification
#' @importFrom shinyjs disable enable
graph_display_server <- function(input, output, session, graph = NULL, 
                                 project_dir = reactiveValues(path = NULL)) {
    # namespace
    ns <- session$ns
    # init local values
    local <- reactiveValues(
        filename = NULL,
        check_filename = TRUE
    )
    # debugging
    observe({
        logging("dir to save figure = ", project_dir$path)
    })
    # graph plot
    output$display <- renderPlot({
        req(graph)
        graph
    })
    # update and check graph filename
    observe({
        req(input$filename)
        local$filename <- input$filename
        local$check_filename <- check_graph_filename(input$filename)
    })
    # consequence of graph filename check
    observe({
        if(!local$check_filename) {
            shinyjs::disable("save")
            showNotification(
                id = "graph_filename_issue", 
                duration = 5, 
                closeButton = TRUE,
                type = "warning", 
                tagList(
                    tags$p(
                        icon("warning"), 
                        paste0("Image filename is not ok, possible extensions are: ", 
                               "'eps', 'ps', 'tex', 'pdf', 'jpeg', 'tiff', 'png', ", 
                               "'bmp', 'svg'")
                    )
                )
            )
        } else {
            shinyjs::enable("save")
        }
    })
    # graph saving
    observeEvent(input$save, {
        req(project_dir$path)
        req(graph)
        ret <- tryCatch(
            save_fig(
                graph = graph, dirname = project_dir$path, 
                filename = local$filename, 
                scale = input$graph_scale, 
                width = input$graph_width, 
                height = input$graph_height, 
                units = input$size_unit, 
                dpi = input$graph_dpi),
            error = function(e) return(e))
        if(!is.null(ret)) {
            showNotification(
                id = "saving_graph", 
                duration = 5, 
                closeButton = TRUE,
                type = "error", 
                tagList(
                    tags$p(
                        icon("warning"), 
                        paste0("Image was saved. ", ret)
                    )
                )
            )
        } else {
            showNotification(
                id = "saving_graph", 
                duration = 5, 
                closeButton = TRUE,
                type = "message", 
                tagList(
                    tags$p(
                        icon("check"), 
                        paste0("Image was saved.")
                    )
                )
            ) 
        }
    })
}

#' Check file name
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_c str_detect
check_graph_filename <- function(filename) {
    file_ext <- c("eps", "ps", "tex", "pdf", "jpeg", 
                  "tiff", "png", "bmp", "svg")
    out <- str_detect(string = filename, 
                      pattern = str_c(c("(", 
                                        str_c(file_ext, 
                                              collapse = "|"), 
                                        ")$"), 
                                      collapse = ""))
    return(out)
}

#' Save a ggplot2 graph in a file
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom ggplot2 ggsave
save_fig <- function(graph, dirname, filename, scale, 
                     width, height, units, dpi) {
    if(!dir.exists(dirname)) {
        dir.create(dirname, recursive = TRUE)
    }
    logging("save fig : file =", filename, "dir =", dirname)
    logging("fig spec :", width, "x", height, units, "scale =", 
            scale, "dpi =", dpi)
    ggsave(filename = filename, plot = graph, path = dirname, scale = scale, 
           width = width, height = height, units = units)
}