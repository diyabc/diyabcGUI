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
    observe_helpers(session, help_dir = help_dir(), withMathJax = TRUE)
    callModule(index_module, "index")
}