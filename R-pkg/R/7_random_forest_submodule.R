#' Randon forest submodule ui
#' @keywords internal
#' @author Ghislain Durif
rf_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h4("Settings"),
        radioButtons(
            ns("mode"), 
            label = "Mode",
            choices = list("Model choice" = "mod_choice", 
                           "Parameter estimation" = "param_estim"), 
            selected = "mod_choice"
        ),
        actionBttn(
            inputId = ns("run"),
            label = "Run",
            style = "fill",
            block = TRUE
        )
    )
}