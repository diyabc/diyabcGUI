#' Simulation module input function
#' @keywords internal
#' @author Ghislain Durif
simu_ui <- function(id, label = "simu") {
    ns <- NS(id)
    tagList(
        sidebarPanel(
            text_input(ns("project"), label = "Project", 
                       default = "project_name") %>% 
                helper(type = "markdown", 
                       content = "simulation_project"),
            dir_input(ns("directory"), label = "Directory"),
            checkboxInput(ns("timestamp"), label = "Timestamp directory", 
                          value = TRUE),
            radioButtons(ns("type"), label = "Simulation type",
                         choices = list("MicroSAT/sequence" = 1, 
                                        "SNP" = 2, 
                                        "PoolSeq" = 3), 
                         selected = 1),
            actionButton(ns("simulate"), label = "Simulate")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Historical model",
                    hist_model_ui(ns("hist_model_simu"))
                ),
                tabPanel(
                    "Genetic data",
                    genetic_data_ui(ns("genetic_data_simu"))
                )
            )
        )
    )
}

#' Simulation module server function
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
simu_module <- function(input, output, session) {
    callModule(text_module, "project")
    callModule(dir_module, "directory")
    
    callModule(hist_model_module, "hist_model_simu")
    callModule(genetic_data_module, "genetic_data_simu")
}