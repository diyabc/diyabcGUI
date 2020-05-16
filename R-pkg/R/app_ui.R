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

#' App dashboard sidebar
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardSidebar menuItem sidebarMenu
app_sidebar <- dashboardSidebar(
    sidebarMenuOutput("app_menu")
)

#' App dashboard body
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardBody tabItems tabItem
#' @importFrom shinyjs useShinyjs
app_body <- dashboardBody(
    useShinyjs(),
    uiOutput("app_tabs")
)


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
    app_sidebar,
    app_body,
    skin = "black"
)
