#' Shiny app server function
#' @keywords internal
#' @description
#' FIXME
#' @details
#' FIXME
#' @author Ghislain Durif
#' @param input app input.
#' @param output app output.
#' @param session shiny session.
#' @importFrom shinyhelper observe_helpers
#' @return None
diyabc_server <- function(input, output, session) {
    # index module
    callModule(index_module_server, "index")
}