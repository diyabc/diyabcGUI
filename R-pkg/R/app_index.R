#' App dashboard header
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardHeader
app_header <- dashboardHeader(title = "DIYABC-RF")

#' App dashboard simplified sidebar
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardSidebar menuItem sidebarMenu
app_simplified_sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(
            "Home", 
            tabName = "home_tab", 
            icon = icon("home")
        ),
        menuItem(
            "Data analysis", 
            tabName = "analysis_tab", 
            icon = icon("gear")
        ),
        menuItem(
            "Data simulation", 
            tabName = "simu_tab", 
            icon = icon("dna")
        )
    )
)

#' App dashboard simplified body
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardBody tabItems tabItem
#' @importFrom shinyjs useShinyjs
app_simplified_body <- dashboardBody(
    useShinyjs(),
    tabItems(
        tabItem(tabName = "home_tab",
                home_page_ui("home_page")
        ),
        tabItem(tabName = "analysis_tab",
                analysis_page_ui("analysis_page")
        ),
        tabItem(tabName = "simu_tab",
                simu_page_ui("simu_page")
        )
    )
)

#' App simplified dashboard server function
#' @keywords internal
#' @author Ghislain Durif
simplified_index_server <- function(input, output, session) {
    callModule(home_page_server, "home_page")
    callModule(analysis_page_server, "analysis_page")
    callModule(simu_page_server, "simu_page")
}
