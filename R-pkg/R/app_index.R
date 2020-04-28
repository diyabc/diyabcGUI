#' App dashboard header
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardHeader
app_header <- dashboardHeader(title = "DIYABC-RF")

#' App dashboard sidebar
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardSidebar menuItem sidebarMenu sidebarMenuOutput
app_sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home"))
    ),
    hr(),
    h3("Data analysis"),
    sidebarMenuOutput("analysis_menu"),
    hr(),
    h4("Data simulation"),
    sidebarMenuOutput("simu_menu")
)

#' App dashboard sidebar update
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard renderMenu sidebarMenu
app_sidebar_update <- function(input, output, session) {
    output$analysis_menu <- renderMenu({
        sidebarMenu(
            menuItem("Empty", icon = icon("warning"))
        )
    })
    output$simu_menu <- renderMenu({
        sidebarMenu(
            menuItem("Empty", icon = icon("warning"))
        )
    })
}

#' App dashboard body
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardBody tabItems tabItem
#' @importFrom shinyjs useShinyjs
app_body <- dashboardBody(
    useShinyjs(),
    tabItems(
        tabItem(
            tabName = "home",
            home_module_ui("home_page")    
        )
    )
)

#' App dashboard body update
#' @keywords internal
#' @author Ghislain Durif
app_body_update <- function(input, output, session) {
    callModule(home_module_server, "home_page")
}


#' Index module ui
#' @keywords internal
#' @author Ghislain Durif
index_module_ui <- function(id, label = "index") {
    ns <- NS(id)
    navbarPage(
        "DIYABC-RF", id = ns("navbar_index"),
        tabPanel(
            "Home",
            home_module_ui(ns("home"))
        ),
        tabPanel(
            "Data simulations",
            simu_module_ui(ns("simu"))
        ),
        tabPanel(
            "Data analysis",
            analysis_module_ui(ns("analysis"))
        )
    )
}

#' Index module server
#' @keywords internal
#' @author Ghislain Durif
index_module_server <- function(input, output, session) {
    
    output$data_analysis <- renderMenu({
        menuItem("Data analysis", icon = icon("gear"))
    })
    
    callModule(home_module_server, "home")
    callModule(simu_module_server, "simu")
    callModule(analysis_module_server, "analysis")
}