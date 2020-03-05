#' Simulation module ui
#' @keywords internal
#' @author Ghislain Durif
simu_module_ui <- function(id, label = "simu") {
    ns <- NS(id)
    tagList(
        sidebarPanel(
            project_input_module_ui(ns("project"), label = "Project", 
                          default = "project_name") %>% 
                helper(type = "markdown", 
                       content = "simulation_project"),
            dir_input_module_ui(ns("directory"), label = "Folder"),
            radioButtons(ns("type"), label = "Simulation type",
                         choices = list("MicroSAT/sequence" = "mss", 
                                        "SNP" = "snp", 
                                        "PoolSeq" = "poolseq"), 
                         selected = "mss"),
            actionButton(ns("simulate"), label = "Simulate")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Historical model",
                    hist_model_module_ui(ns("hist_model_simu"))
                ),
                tabPanel(
                    "Genetic data",
                    genetic_data_module_ui(ns("genetic_data_simu"))
                )
            )
        )
    )
}

#' Simulation module server
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
simu_module_server <- function(input, output, session) {
    
    data_info <- reactiveValues()

    observe({
        data_info$type <- input$type
    })
    
    callModule(project_input_module_server, "project")
    callModule(dir_input_module_server, "directory")
    
    callModule(hist_model_module_server, "hist_model_simu")
    callModule(genetic_data_module, "genetic_data_simu", data_info = data_info)
}
