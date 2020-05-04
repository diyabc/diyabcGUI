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

#' Render menu item
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard menuSubItem
render_sidebar_menu <- function(project_list, tag) {
    if(length(project_list) > 0) {
        tab_id <- paste0(tag, "_", item$project_name)
        out <- lapply(
            project_list,
            function(item) {
                menuSubItem(
                    tabName = tab_id,
                    icon = icon("folder")
                )
            }
        )
    } else {
        out <- list(menuSubItem("Empty", icon = icon("warning")))
    }
    return(out)
}


#' App dashboard sidebar update
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard menuItem sidebarMenu
app_sidebar_update <- function(input, output, session,
                               analysis_project_list = list(),
                               simu_project_list = list()) {
    # to select home tab at start
    observeEvent(session, {
        updateTabItems(session, "main_menu", selected = "home")
    })
    # init item lists
    local <- reactiveValues(
        analysis_item_list = list(),
        analysis_menu_list = list()
    )
    # render sidebar
    observe({
        # check if any current analysis project
        local$analysis_item_list <- render_sidebar_menu(analysis_project_list,
                                                        tag = "analysis")
        # check if any current simulation project
        local$simu_item_list <- render_sidebar_menu(simu_project_list, 
                                                    tag = "simu")
        # render
        output$sidebar <- renderUI({
            tagList(
                sidebarMenu(
                    id = "main_menu",
                    menuItem(
                        "Home", 
                        tabName = "home", 
                        icon = icon("home"), 
                        selected = TRUE
                    ), 
                    menuItem(
                        "Data analysis", 
                        tabName = "analysis_tab", 
                        icon = icon("gear"), 
                        startExpanded = TRUE,
                        local$analysis_item_list
                    ),
                    menuItem(
                        "Data simulation", 
                        tabName = "simu_tab", 
                        icon = icon("dna"), 
                        startExpanded = FALSE,
                        local$simu_item_list
                    )
                )
            )
        })
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

#' List tab properties
#' @keywords internal
#' @author Ghislain Durif
tab_prop <- function(tab_name, tab_ui_id, module_ui) {
    return(
        list(
            tab_name = tab_id,
            tab_ui_id = tab_ui_id,
            tab_ui = module_ui(tab_ui_id)
        )
    )
}

#' Render menu item
#' @keywords internal
#' @author Ghislain Durif
render_body <- function(project_list, module_ui, tag) {
    if(length(project_list) > 0) {
        out <- lapply(
            project_list,
            function(item) {
                tab_name <- paste0(tag, "_", item$project_name)
                tab_ui_id <- paste0("mod_", tab_name)
                return(tab_prop(tab_name, tab_ui_id, tab_ui, module_ui))
            }
        )
    } else {
        out <- NULL
    }
    return(out)
}

#' App dashboard body update
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard tabItem
app_body_update <- function(input, output, session,
                            analysis_project_list = list(),
                            simu_project_list = list()) {
    
    # init local value
    local <- reactiveValues(index_tab = NULL,
                            analysis_tab_list = NULL,
                            simu_tab_list = NULL)
    
    ## dynamically update tab list
    # home page
    observe({
        local$index_tab <- tab_prop(tab_name = "home", 
                                    tab_ui_id = "home_page", 
                                    module_ui = home_module_ui)
    })
    # analysis items
    observe({
        local$analysis_tab_list <- render_body(analysis_project_list, 
                                               analysis_module_ui, 
                                               tag = "analysis")
    })
    # simu items
    observe({
        local$simu_tab_list <- render_body(simu_project_list, 
                                           simu_module_ui, 
                                           tag = "simu")
    })
    
    ## render output
    observe({
        output$tabs <- renderUI({
            tab_list <- lapply(
                            c(list(local$index_tab), 
                              local$analysis_tab_list, 
                              local$simu_tab_list), 
                            function(tab) {
                                tabItem(tabName = tab$tab_name, 
                                        tab$tab_ui)
                            })
            # process
            do.call(tabItems, local$tab_list)
        })
        
        ## call modules
        # home
        lapply(local$index_tab, 
               function(tab) {
                   callModule(home_module_server, "home_page")
               })
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