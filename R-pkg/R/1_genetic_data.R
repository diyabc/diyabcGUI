#' Genetic data type ui
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
genetic_data_type_ui <- function() {
    verticalLayout(
        numericInput("auto_dip", label = "Autosomal diploid (A)", value = 0),
        numericInput("auto_hap", label = "Autosomal diploid (H)", value = 0),
        numericInput("x_linked", label = "X-linked (X)", value = 0),
        numericInput("y_linked", label = "X-linked (Y)", value = 0),
        numericInput("mito", label = "Mitochondrial (M)", value = 0)
    )
}

#' Genetic data ui
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
genetic_data_ui <- function() {
    genetic_data_type_ui()
}

#' Genetic data server
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
genetic_data_server <- function(input, output, session) {}