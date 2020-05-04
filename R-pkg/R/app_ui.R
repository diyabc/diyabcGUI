#' Shiny app ui function
#' @keywords internal
#' @description
#' FIXME
#' @details
#' FIXME
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardPage
#' @return Shiny ui
# diyabc_ui <- dashboardPage(
#     app_header,
#     app_sidebar,
#     app_body,
#     skin = "black"
# )
diyabc_ui <- fluidPage(
    index_module_ui("index")
)
