#' App dashboard header
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardHeader
app_header <- function() {
    dashboardHeader(title = "DIYABC-RF")
}

#' Shiny app ui function
#' @keywords internal
#' @description
#' FIXME
#' @details
#' FIXME
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardPage
#' @return Shiny ui
#' @export
diyabc_ui <- function() {
    dashboardPage(
        app_header(),
        app_sidebar(),
        app_body(),
        skin = "black"
    )
}