#' Home module ui
#' @keywords internal
#' @author Ghislain Durif
home_module_ui <- function(id, label = "home") {
    ns <- NS(id)
    tagList(
        h3("Data analysis"),
        tags$div(
            tags$p("Including the two modules of DIYABC Random Forest:"),
            tags$ul(
                tags$li("training set simulation"),
                tags$li("random forest analyses")
            )
        ),
        fluidPage(
            actionButton(ns("new_analysis_project"), 
                         label = "New project", width = '20%')
        ),
        tags$br(),
        fluidPage(
            actionButton(ns("open_analysis_project"), 
                         label = "Open project", width = '20%'),
            dir_input_module_ui(ns("analysis_project_directory")),
            uiOutput(ns("open_analysis_project_status"))
        )
        ,
        hr(),
        h4("Pseudo-observed dataset simulation") %>% 
            helper(type = "markdown", 
                   content = "data_simulation"),
        fluidPage(
            actionButton(ns("new_simu_project"), 
                         label = "New project", width = '20%')
        ),
        tags$br(),
        fluidPage(
            actionButton(ns("open_simu_project"), 
                         label = "Open project", width = '20%'),
            dir_input_module_ui(ns("simu_project_directory")),
            uiOutput(ns("open_simu_project_status"))
        )
    )
}

#' Home module server
#' @keywords internal
#' @author Ghislain Durif
home_module_server <- function(input, output, session) {
    analysis_dir <- callModule(dir_input_module_server, 
                               "analysis_project_directory")
    simu_dir <- callModule(dir_input_module_server,
                           "simu_project_directory")
    
    # check analysis project directory
    output$open_analysis_project_status <- renderUI({
        out <- NULL
        if(! "header.txt" %in% list.files(analysis_dir$datapath))
            out <- tagList(tags$p(
                    icon("warning"), 
                    "Chosen directory is not a DIYABC RF analysis directory."))
        out
    })
    
    # check analysis project directory
    output$open_simu_project_status <- renderUI({
        out <- NULL
        if(! "headersim.txt" %in% list.files(simu_dir$datapath))
            out <- tagList(tags$p(
                icon("warning"), 
                "Chosen directory is not a DIYABC RF analysis directory."))
        out
    })
}
