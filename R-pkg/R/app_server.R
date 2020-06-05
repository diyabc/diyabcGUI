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
    ## help
    observe_helpers(session, help_dir = help_dir(), withMathJax = TRUE)
    ##  temp dir
    tmp_dir <- tempfile("diyabc")
    dir.create(tmp_dir, showWarnings = FALSE)
    diyabc_options <- lst(tmp_dir)
    options("diyabc" = diyabc_options)
    ## index server function
    # simplified_index_server(input, output, session)
    index_server(input, output, session)
}