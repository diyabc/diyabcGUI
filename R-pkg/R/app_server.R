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
    # local reactive values
    local = reactiveValues(
        analysis_project_list = list(),
        simu_project_list = list()
    )
    # help
    observe_helpers(session, help_dir = help_dir(), withMathJax = TRUE)
    # sidebar
    app_sidebar_update(input, output, session,
                       analysis_project_list = local$analysis_project_list,
                       simu_project_list = local$simu_project_list)
    # body
    app_body_update(input, output, session)
    # index module
    callModule(index_module_server, "index")
}