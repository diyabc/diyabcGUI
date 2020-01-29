#' Genetic data type ui
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
genetic_data_type_ui <- function(id) {
    ns <- NS(id)
    verticalLayout(
        numericInput(ns("auto_dip"), label = "Autosomal diploid (A)", value = 0),
        numericInput(ns("auto_hap"), label = "Autosomal diploid (H)", value = 0),
        numericInput(ns("x_linked"), label = "X-linked (X)", value = 0),
        numericInput(ns("y_linked"), label = "X-linked (Y)", value = 0),
        numericInput(ns("mito"), label = "Mitochondrial (M)", value = 0)
    )
}

#' Genetic data type module
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
genetic_data_type_ui <- function(input, output, session) {}

#' Genetic data ui
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
genetic_data_ui <- function(id) {
    ns <- NS(id)
    genetic_data_type_ui(ns("data_type"))
}

#' Genetic data module
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
genetic_data_module <- function(input, output, session) {}