#' Training set simulation sub-module ui
#' @keywords internal
#' @author Ghislain Durif
trainset_simu_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        verticalLayout(
            h4("Historical model"),
            hist_model_add_module_ui(ns("hist_model_def"))
        )
    )
}

#' Training set simulation sub-module server
#' @keywords internal
#' @author Ghislain Durif
trainset_simu_module_server <- function(input, output, session, 
                                        project_dir = NULL) {
    # init local reactive values
    local <- reactiveValues()
    # init ouput reactive values
    out <- reactiveValues()
    # server
    callModule(hist_model_add_module_server, "hist_model_def")
}

#' Random Forest analysis sub-module ui
#' @keywords internal
#' @author Ghislain Durif
rf_module_ui <- function(id) {
    ns <- NS(id)
    "Random Forest analysis sub-module"
}

#' Random Forest analysis sub-module server
#' @keywords internal
#' @author Ghislain Durif
rf_module_server <- function(input, output, session) {}