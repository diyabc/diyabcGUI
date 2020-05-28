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
#' @export
diyabc_server <- function(input, output, session) {
    # help
    observe_helpers(session, help_dir = help_dir(), withMathJax = TRUE)
    # index server function
    index_server(input, output, session)
    # simplified_index_server(input, output, session)
}