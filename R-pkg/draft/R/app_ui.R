#' Shiny app ui function
#' @keywords internal
#' @description
#' FIXME
#' @details
#' FIXME
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardPage
#' @return Shiny ui
diyabc_ui <- dashboardPage(
    app_header,
    app_simplified_sidebar,
    app_simplified_body,
    skin = "black"
)
