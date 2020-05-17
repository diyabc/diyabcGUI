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
    local <- reactiveValues(
        current_tab = "home_page"
    )
    
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
        # FIXME
        updateTabItems(session, "app_menu", selected = local$current_tab)
    })
    
    # dynamic tab update
    output$app_tabs <- renderUI({
        ## static tabs
        static_tabs <- list(
            tabItem(tabName = "home_tab",
                    home_page_ui("home_page")
            )
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
    
    # FIXME update selected tab
    observeEvent(local$current_tab, {
        print(local$current_tab)
        updateTabItems(session, "app_menu", selected = local$current_tab)
    })
    
    # home page server side
    home_page <- callModule(home_page_server, "home_page")
    
    # update analysis project list
    observeEvent(home_page$new_analysis_project, {
        # update project count
        analysis_setting$count <- ifelse(is.null(analysis_setting$count), 
                                                 0, analysis_setting$count) + 1
        # project id
        proj_id <- str_c("analysis_proj", analysis_setting$count)
        # update analysis project list
        analysis_setting$proj_list[[ proj_id ]] <- reactiveValues(
            name = proj_id, #"project_name",
            id = proj_id
        )
        # server function
        analysis_setting$content_list[[ proj_id ]] <<- callModule(
            analysis_page_server, 
            str_c("mod_" , proj_id),
            project_name = reactive(analysis_setting$proj_list[[ proj_id ]]$name)
        )
        
        # FIXME close project
        
        # # FIXME update tab Name
        # observeEvent(analysis_setting$content_list[[ proj_id ]]$setting$validate, {
        #     req(analysis_setting$content_list[[ proj_id ]]$setting)
        #     req(analysis_setting$content_list[[ proj_id ]]$setting$project_name)
        #     analysis_setting$proj_list[[ proj_id ]]$name <- 
        #         analysis_setting$content_list[[ proj_id ]]$setting$project_name
        #     updateTabItems(session, "app_menu", selected = proj_id)
        # })
        
        # update current tab
        local$current_tab <- proj_id
    })
    
    # update simu project list
    observeEvent(home_page$new_simu_project, {
        # update project count
        simu_setting$count <- ifelse(is.null(simu_setting$count), 
                                         0, simu_setting$count) + 1
        # project id
        proj_id <- str_c("simu_proj", simu_setting$count)
        # update simu project list
        simu_setting$proj_list[[ proj_id ]] <- reactiveValues(
            name = proj_id, #"project_name",
            id = proj_id
        )
        # server function
        simu_setting$content_list[[ proj_id ]] <<- callModule(
            simu_page_server, 
            str_c("mod_" , proj_id),
            project_name = reactive(simu_setting$proj_list[[ proj_id ]]$name)
        )
        
        # FIXME close project
        
        # # FIXME update tab Name
        # observeEvent(simu_setting$content_list[[ proj_id ]]$setting$validate, {
        #     req(simu_setting$content_list[[ proj_id ]]$setting)
        #     req(simu_setting$content_list[[ proj_id ]]$setting$project_name)
        #     simu_setting$proj_list[[ proj_id ]]$name <- 
        #         simu_setting$content_list[[ proj_id ]]$setting$project_name
        #     updateTabItems(session, "app_menu", selected = proj_id)
        # })
        
        # update current tab
        local$current_tab <- proj_id
    })
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

