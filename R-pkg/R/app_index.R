#' App dashboard header
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardHeader
app_header <- dashboardHeader(title = "DIYABC-RF")

#' App dashboard sidebar
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardSidebar
app_sidebar <- dashboardSidebar(
    uiOutput("sidebar")
)

#' App dashboard sidebar update
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard menuItem sidebarMenu
app_sidebar_update <- function(input, output, session) {
    # to select home tab at start
    observeEvent(session, {
        updateTabItems(session, "main_menu", selected = "home")
    })
    # render sidebar
    output$sidebar <- renderUI({
        tagList(
            sidebarMenu(
                id = "main_menu",
                menuItem(
                    "Home", 
                    tabName = "home", 
                    icon = icon("home"), 
                    selected = TRUE
                )
            ),
            hr(),
            sidebarMenu(
                id = "analysis_menu",
                menuItem(
                    "Data analysis", 
                    tabName = "analysis_tab", 
                    icon = icon("gear"), 
                    startExpanded = TRUE,
                    list(
                        menuSubItem("Empty", icon = icon("warning"))
                    )
                )
            ),
            hr(),
            sidebarMenu(
                id = "simu_menu",
                menuItem(
                    "Data simulation", 
                    tabName = "simu_tab", 
                    icon = icon("dna"), 
                    startExpanded = FALSE,
                    list(
                        menuSubItem("Empty", icon = icon("warning"))
                    )
                )
            )
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
    uiOutput("tabs"),
)

#' App dashboard body update
#' @keywords internal
#' @author Ghislain Durif
app_body_update <- function(input, output, session) {
    
    observe({
        output$tabs <- renderUI({
            # home tab
            tab_list <- list(
                tabItem(
                    tabName = "home",
                    home_module_ui("home_page")    
                )
            )
            # process
            do.call(tabItems, tab_list)
        })
        
        callModule(home_module_server, "home_page")
    })
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