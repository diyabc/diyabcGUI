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

#' App dashboard server function
#' @keywords internal
#' @author Ghislain Durif
index_server <- function(input, output, session) {
    
    # init local
    local <- reactiveValues()
    
    # init project list
    analysis_setting <- reactiveValues(
        menu_subitems = NULL,
        count = NULL,
        proj_list = list(),
        tab_list = list()
    )
    simu_setting <- reactiveValues(
        menu_subitems = NULL,
        count = NULL,
        proj_list = list(),
        tab_list = list()
    )
    
    # update analysis menu sub item list
    observeEvent(analysis_setting$proj_list, {
        
        print(analysis_setting$proj_list)
        
        analysis_setting$menu_subitems <- render_menu_subitem_list(
            analysis_setting$proj_list, 
            tag = "analysis_project")
    })
    
    # update simu menu sub item list
    observeEvent(simu_setting$proj_list, {
        simu_setting$menu_subitems <- render_menu_subitem_list(
            simu_setting$proj_list, 
            tag = "simu_project")
    })
    
    # render sidebar menu
    observe({
        output$app_menu <- renderMenu({
            sidebarMenu(
                menuItem(
                    "Home", 
                    tabName = "home_tab", 
                    icon = icon("home")
                ),
                menuItem(
                    "Data analysis", 
                    tabName = "analysis_tab", 
                    icon = icon("gear"), 
                    startExpanded = TRUE,
                    analysis_setting$menu_subitems
                ),
                menuItem(
                    "Data simulation", 
                    tabName = "simu_tab", 
                    icon = icon("dna"),
                    startExpanded = TRUE,
                    simu_setting$menu_subitems
                )
            )
        })
    })
    
    # FIXME render body
    output$app_tabs <- renderUI({
        ## static tabs
        static_tabs <- list(
            tabItem(tabName = "home_tab",
                    home_page_ui("home_page")
            )
            # tabItem(tabName = "analysis_tab",
            #         # analysis_page_ui("analysis_page")
            # ),
            # tabItem(tabName = "simu_tab",
            #         simu_page_ui("simu_page")
            # )
        )
        ## combine static and dynamic tabs
        current_tabs <- c(static_tabs,
                          analysis_setting$tab_list,
                          simu_setting$tab_list)
        ## render
        do.call(tabItems, current_tabs)
    })
    
    # home page server side
    home_page <- callModule(home_page_server, "home_page")
    
    # open new analysis project
    observeEvent(home_page$new_analysis_project, {
        # update project count
        analysis_setting$count <- ifelse(is.null(analysis_setting$count), 
                                                 0, analysis_setting$count) + 1
        # project id
        proj_id <- str_c("analysis_proj", analysis_setting$count)
        # update analysis project list
        analysis_setting$proj_list[[ proj_id ]] <- list(name = "project_name", 
                                                        id = proj_id)
        # select corresponding item
        updateTabItems(session, "app_tabs", selected = proj_id)
    })
    
    # callModule(analysis_page_server, "analysis_page")
    # callModule(simu_page_server, "simu_page")
}

#' Render menu sub item list function
#' @keywords internal
#' @author Ghislain Durif
#' @param proj_list list of projects with `$name` (string) and `$id` (unique 
#' integer).
#' @param tag string, tag to identify 'analysis' or 'simulation' projects.
#' @importFrom shinydashboard menuSubItem
render_menu_subitem_list <- function(proj_list, tag = "proj") {
    out <- NULL
    if(length(proj_list) > 0) {
        out <- lapply(proj_list, function(proj) {
            menuSubItem(
                closable_menu_title(id = proj$id, label = proj$name), 
                tabName = str_c(tag, proj$id),
                icon = icon("folder")
            )
        })
    } else {
        out <- list(menuSubItem("Empty", icon = icon("warning")))
    }
    return(out)
}

#' Return closable menu title
#' @keywords internal
#' @author Ghislain Durif
closable_menu_title <- function(id, label) {
    tags$span(
        label,
        HTML("&nbsp;"),
        actionBttn(
            str_c("close_", id), 
            label = NULL,
            icon = icon("close"),
            style = "minimal",
            size = "xs"
        )
    )
}

