#' App dashboard simplified sidebar
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardSidebar menuItem sidebarMenu 
#' sidebarMenuOutput
app_sidebar <- function() {
    dashboardSidebar(
        sidebarMenu(
            id = "app_menu",
            style = "position:fixed;width:inherit;",
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
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom shinyjs useShinyjs
app_body <- function() {
    dashboardBody(
        useShinyjs(),
        useShinyFeedback(),
        add_busy_spinner(spin = "fading-circle", margins = c(0, 10)),
        tabItems(
            tabItem(
                tabName = "home_tab",
                home_page_ui("home_page")
            ),
            tabItem(
                tabName = "analysis_setup_tab",
                analysis_setup_module_ui("analysis_setup_module")
            ),
            tabItem(
                tabName = "analysis_ts_tab",
                analysis_ts_module_ui("analysis_ts_module")
            ),
            tabItem(
                tabName = "analysis_rf_tab",
                analysis_rf_module_ui("analysis_rf_module")
            ),
            tabItem(
                tabName = "analysis_admin_tab",
                analysis_admin_module_ui("analysis_admin_module")
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
#' @importFrom shinydashboard sidebarMenu renderMenu menuItem menuSubItem
index_server <- function(input, output, session) {
    
    # home page
    home_page <- callModule(home_page_server, "home_page")
    
    ## check for binary prog diyabc and abcranger
    observe({
        # check if binary files are available
        diyabc_bin <- tryCatch(
            find_bin("diyabc"),
            error = function(e) return(e)
        )
        abcranger_bin <- tryCatch(
            find_bin("abcranger"),
            error = function(e) return(e)
        )
        if("error" %in% class(diyabc_bin) | "error" %in% class(abcranger_bin)) {
            show_alert(
                title = "Error !",
                text = tagList(
                    icon("warning"),
                    "DIYABC-RF internal engine is missing.", 
                    br(), br(),
                    "Please navigate to the", tags$b("Preferences"), 
                    "tab (see left sidebar) and click on", 
                    tags$b("Update DIYABC-RF internal engine"), ".", 
                    br(), br(),
                    "If the issue persists, please contact DIYABC-RF support."
                ),
                type = "error",
                html = TRUE
            )
        }
    })
    
    ## new analysis project
    observeEvent(home_page$new_analysis_project, {
        req(home_page$new_analysis_project)
        
        # rendering sidebar menu
        output$menu_tabs <- renderMenu({
            sidebarMenu(
                id = "dynamic_tabs",
                menuItem(
                    "DIYABC-RF main pipeline", 
                    # tabName = "analysis_tab", 
                    icon = icon("flask"),
                    startExpanded = TRUE,
                    menuSubItem(
                        "Project settings",
                        tabName = "analysis_setup_tab"
                    ),
                    menuSubItem(
                        "Training set simulation",
                        tabName = "analysis_ts_tab"
                    ),
                    menuSubItem(
                        "Random Forest Analysis",
                        tabName = "analysis_rf_tab"
                    ),
                    menuSubItem(
                        "Project administration",
                        tabName = "analysis_admin_tab"
                    )
                )
            )
        })
        
        # update tab item
        updateTabItems(session, "app_menu", selected = "analysis_setup_tab")
        
        # reset env
        reset_diyabcrf_env()
        
        # verbosity
        observe({
            logging("analysis project directory:", env$ap$proj_dir)
        })
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
        
        # reset env
        reset_datagen_env()
        
        # verbosity
        observe({
            logging("data generation project directory:", env$dp$proj_dir)
        })
    })
    
    ## analysis page
    analysis_setup_module <- callModule(
        analysis_setup_module_server, "analysis_setup_module", parent = session
    )
    analysis_ts_module <- callModule(
        analysis_ts_module_server, "analysis_ts_module", parent = session
    )
    analysis_rf_module <- callModule(
        analysis_rf_module_server, "analysis_rf_module", parent = session
    )
    analysis_admin_module <- callModule(
        analysis_admin_module_server, "analysis_admin_module", parent = session
    )
    
    ## datagen page
    # datagen_module <- callModule(datagen_module_server, "datagen_module")
    
    ## preferences
    callModule(pref_page_server, "pref_page")
    
    ## quit
    observeEvent(input$quitting, {
        stopApp()
    })
}
