#' App dashboard simplified sidebar
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardSidebar menuItem sidebarMenu 
#' sidebarMenuOutput
app_sidebar <- function() {
    dashboardSidebar(
        sidebarMenu(
            id = "app_menu",
            menuItem(
                "Home", 
                tabName = "home_tab", 
                icon = icon("home")
            ),
            sidebarMenuOutput("menu_tabs"),
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
                home_page_ui("home_page")
            ),
            tabItem(
                tabName = "analysis_tab",
                # analysis_module_ui("analysis_module")
            ),
            tabItem(
                tabName = "datagen_tab",
                # datagen_module_ui("datagen_module")
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
#' @importFrom shinydashboard renderMenu
index_server <- function(input, output, session) {
    
    # home page
    home_page <- callModule(home_page_server, "home_page")
    
    ## new analysis project
    observeEvent(home_page$new_analysis_project, {
        req(home_page$new_analysis_project)
        # rendering sidebar menu
        output$menu_tabs <- renderMenu({
            sidebarMenu(
                id = "dynamic_tabs",
                menuItem(
                    "DIYABC-RF main pipeline", 
                    tabName = "analysis_tab", 
                    icon = icon("flask")
                )
            )
        })
        # update tab item
        updateTabItems(session, "app_menu", selected = "analysis_tab")
        # init env
        init_diyabcrf_env()
    })
    
    ## new data generation project
    observeEvent(home_page$new_datagen_project, {
        req(home_page$new_datagen_project)
        # rendering sidebar menu
        output$menu_tabs <- renderMenu({
            sidebarMenu(
                id = "dynamic_tabs",
                menuItem(
                    "Synthetic data file generation", 
                    tabName = "datagen_tab", 
                    icon = icon("dna")
                )
            )
        })
        # update tab item
        updateTabItems(session, "app_menu", selected = "datagen_tab")
        # init env
        init_datagen_env()
    })
    
    ## analysis page
    # analysis_module <- callModule(analysis_module_server, "analysis_module")
    
    ## datagen page
    # datagen_module <- callModule(datagen_module_server, "datagen_module")
    
    ## preferences
    callModule(pref_page_server, "pref_page")
    
    ## quit
    observeEvent(input$quitting, {
        stopApp()
    })
}
