#' Graph display module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyWidgets dropdownButton
graph_display_module_ui <- function(id) {
    ns <- NS(id)
    verticalLayout(
        plotOutput(ns("display"), height = "200px"),
        uiOutput(ns("saving"))
    )
}


#' Graph display module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shiny showNotification
#' @importFrom shinyjs disable enable
#' @importFrom shinyWidgets dropdownButton
graph_display_module_server <- function(input, output, session, graph = NULL, 
                                        project_path = NULL) {
    # namespace
    ns <- session$ns
    # init local values
    local <- reactiveValues(
        filename = "Rplot.jpg",
        check_filename = TRUE
    )
    # graph plot
    output$display <- renderPlot({ 
        graph
    })
    # render ui
    observe({
        output$saving <- renderUI({
            fluidRow(
                column(
                    2, 
                    dropdownButton(
                        h4("Graph parameters"),
                        textInput(
                            ns("graph_filename"),
                            value = local$filename,
                            label = "Filename"
                        ) %>% 
                            helper(
                                type = "inline", 
                                content = paste0(
                                    "Figures are saved 'fig' sub-folder in ",
                                    "project directory. ", 
                                    "Possible extensions are: ", 
                                    "'eps', 'ps', 'tex', 'pdf', 'jpeg', ", 
                                    "'tiff', 'png', 'bmp', 'svg'")),
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
                            value = 100, min = 1
                        ) %>% 
                            helper(
                                type = "inline", 
                                content = paste0(
                                    "Output figure width in 'units'.")
                            ),
                        numericInput(
                            ns("graph_height"), 
                            label = "Graph height", 
                            value = 100, min = 1
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
                                content = paste0(
                                    "Image resolution.")
                            ),
                        icon = icon("cog"), size = "sm", right = TRUE, inline = TRUE
                    )
                ),
                column(
                    4, 
                    actionButton(ns("save"), label = "Save")
                )
            )
        })
    })
    # update and check graph filename
    observe({
        req(input$graph_filename)
        local$filename <- input$graph_filename
        local$check_filename <- check_graph_filename(input$graph_filename)
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
                        paste0("Graph filename is not ok, possible extensions are: ", 
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
        if(!is.null(project_path)) {
            save_fig(graph = graph, dirname = path, 
                     filename = local$graph_filename, 
                     scale = input$graph_scale, 
                     width = input$graph_width, 
                     height = input$graph_weight, 
                     units = input$size_units, 
                     dpi = input$graph_dpi)
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
    ggsave(filename = filename, plot = graph, path = dirname, scale = scale, 
           width = width, height = height, units = units)
}