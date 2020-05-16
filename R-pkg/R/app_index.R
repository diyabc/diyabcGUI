#' App simplified dashboard server function
#' @keywords internal
#' @author Ghislain Durif
simplified_index_server <- function(input, output, session) {
    callModule(home_page_server, "home_page")
    callModule(analysis_page_server, "analysis_page")
    callModule(simu_page_server, "simu_page")
}

#' App dashboard server function
#' @keywords internal
#' @author Ghislain Durif
#' @importFRom shinydashboard tabItem tabItems
index_server <- function(input, output, session) {
    
    # init local
    local <- reactiveValues()
    
    # init project list
    analysis_setting <- reactiveValues(
        count = NULL,
        content_list = list(),
        proj_list = list()
    )
    simu_setting <- reactiveValues(
        count = NULL,
        content_list = list(),
        proj_list = list()
    )
    
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
                    render_menu_subitem_list(analysis_setting$proj_list)
                ),
                menuItem(
                    "Data simulation", 
                    tabName = "simu_tab", 
                    icon = icon("dna"),
                    startExpanded = TRUE,
                    render_menu_subitem_list(simu_setting$proj_list)
                )
            )
        })
    })
    
    # dynamic tab update
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
        # dynamic analysis tab
        analysis_tabs <- lapply(analysis_setting$proj_list, function(proj) {
            tabItem(
                tabName = proj$id, 
                analysis_page_ui(str_c("mod_" , proj$id))
            )
        })
        # dynamic simu tab
        simu_tabs <- lapply(simu_setting$proj_list, function(proj) {
            tabItem(
                tabName = proj$id, 
                simu_page_ui(str_c("mod_" , proj$id))
            )
        })
        ## combine static and dynamic tabs
        current_tabs <- c(static_tabs,
                          unname(analysis_tabs),
                          unname(simu_tabs))
        
        ## render
        do.call(tabItems, current_tabs)
    })
    
    # home page server side
    home_page <- callModule(home_page_server, "home_page")
    
    # # dynamic content in the dynamic tabs
    # observe({ 
    #     lapply(analysis_setting$proj_list, function(proj) {
    #         
    #         analysis_setting$content_list[[ proj$id ]] <<- callModule(
    #             analysis_page_server, str_c("mod_" , proj$id)
    #         )
    #     })
    # })
    
    ## open analysis project
    # update analysis project list
    observeEvent(home_page$new_analysis_project, {
        # update project count
        analysis_setting$count <- ifelse(is.null(analysis_setting$count), 
                                                 0, analysis_setting$count) + 1
        # project id
        proj_id <- str_c("analysis_proj", analysis_setting$count)
        # update analysis project list
        analysis_setting$proj_list[[ proj_id ]] <- list(
            # name = analysis_setting$content_list[[ proj_id ]]$setting$project_name,
            name = "test",
            id = proj_id
        )

        # # server side call
        # analysis_setting$content_list[[ proj_id ]] <- callModule(
        #     analysis_page_server, proj_id
        # )
        # select corresponding item
        # updateTabItems(session, "app_tabs", selected = proj_id)
        # updateTabItems(session, "app_tabs", selected = "home_page")
    })
    
    # update analysis menu sub item list
    # observeEvent(analysis_setting$proj_list, {
    #     print(analysis_setting$proj_list)
    #     analysis_setting$menu_subitems <- render_menu_subitem_list(
    #         analysis_setting$proj_list
    #     )
    #     print(analysis_setting$menu_subitems)
    # })
    
    # update simu menu sub item list
    observeEvent(simu_setting$proj_list, {
        simu_setting$menu_subitems <- render_menu_subitem_list(
            simu_setting$proj_list
        )
    })
    
    # callModule(analysis_page_server, "analysis_page")
    # callModule(simu_page_server, "simu_page")
}

#' Render menu sub item list function
#' @keywords internal
#' @author Ghislain Durif
#' @param proj_list list of projects with `$name` (string) and `$id` (unique 
#' integer).
#' @importFrom shinydashboard menuSubItem
render_menu_subitem_list <- function(proj_list) {
    out <- NULL
    if(length(proj_list) > 0) {
        out <- lapply(proj_list, function(proj) {
            menuSubItem(
                closable_menu_title(id = proj$id, label = proj$name), 
                tabName = proj$id,
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

