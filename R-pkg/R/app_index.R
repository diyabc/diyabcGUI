#' App dashboard simplified sidebar
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardSidebar menuItem sidebarMenu
app_sidebar <- function() {
    dashboardSidebar(
        sidebarMenu(
            id = "app_menu",
            menuItem(
                "Home", 
                tabName = "home_tab", 
                icon = icon("home")
            ),
            menuItem(
                "DIYABC-RF main pipeline", 
                tabName = "analysis_tab", 
                icon = icon("flask")
            ),
            menuItem(
                "Synthetic data file generation", 
                tabName = "datagen_tab", 
                icon = icon("dna")
            ),
            menuItem(
                "Preferences", 
                tabName = "pref_tab", 
                icon = icon("gear")
            ),
            menuItem(
                "Quit", 
                tabName = "quit",
                icon = icon("circle-o-notch")
            )
        )
    )
}

#' App dashboard simplified body
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardBody tabItems tabItem
#' @importFrom shinyjs useShinyjs
app_body <- function() {
    dashboardBody(
        useShinyjs(),
        add_busy_spinner(spin = "fading-circle", margins = c(0, 10)),
        tabItems(
            tabItem(
                tabName = "home_tab",
                simplified_home_page_ui("home_page")
            ),
            tabItem(
                tabName = "analysis_tab",
                analysis_page_ui("analysis_page")
            ),
            tabItem(
                tabName = "datagen_tab",
                datagen_page_ui("datagen_page")
            ),
            tabItem(
                tabName = "pref_tab",
                pref_page_ui("pref_page")
            ),
            tabItem(
                tabName = "quit",
                h1("Confirmation needed ?"),
                actionBttn(
                    "quitting",
                    label = "Quit",
                    icon = icon("check"),
                    style = "fill", 
                    color = "danger"
                )
            )
        )
    )
}

#' App simplified dashboard server function
#' @keywords internal
#' @author Ghislain Durif
index_server <- function(input, output, session) {
    
    # home page
    home_page <- callModule(simplified_home_page_server, "home_page")
    
    ## new analysis project
    observeEvent(home_page$new_analysis_project, {
        req(home_page$new_analysis_project)
        updateTabItems(session, "app_menu", selected = "analysis_tab")
        init_diyabcrf_env()
    })
    
    ## new data generation project
    observeEvent(home_page$new_datagen_project, {
        req(home_page$new_datagen_project)
        updateTabItems(session, "app_menu", selected = "datagen_tab")
        init_datagen_env()
    })
    
    ## analysis page
    analysis_page <- callModule(analysis_page_server, "analysis_page")
    # # reset
    # observeEvent(analysis_page$reset, {
    #     req(analysis_page$reset)
    #     session$reload()
    #     updateTabItems(session, "app_menu", selected = "analysis_tab")
    # })
    
    ## datagen page
    datagen_page <- callModule(datagen_page_server, "datagen_page")
    # # reset
    # observeEvent(datagen_page$reset, {
    #     req(datagen_page$reset)
    #     session$reload()
    #     updateTabItems(session, "app_menu", selected = "datagen_tab")
    # })
    
    ## preferences
    callModule(pref_page_server, "pref_page")
    
    ## quit
    observeEvent(input$quitting, {
        stopApp()
    })
}
