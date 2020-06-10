#' App dashboard simplified sidebar
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardSidebar menuItem sidebarMenu
app_simplified_sidebar <- function() {
    dashboardSidebar(
        sidebarMenu(
            id = "app_menu",
            menuItem(
                "Home", 
                tabName = "home_tab", 
                icon = icon("home")
            ),
            menuItem(
                "Data analysis", 
                tabName = "analysis_tab", 
                icon = icon("flask")
            ),
            menuItem(
                "Data simulation", 
                tabName = "simu_tab", 
                icon = icon("dna")
            ),
            menuItem(
                "Preferences", 
                tabName = "pref_tab", 
                icon = icon("gear")
            )
        )
    )
}

#' App dashboard simplified body
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardBody tabItems tabItem
#' @importFrom shinyjs useShinyjs
app_simplified_body <- function() {
    dashboardBody(
        useShinyjs(),
        add_busy_spinner(spin = "fading-circle"),
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
                tabName = "simu_tab",
                simu_page_ui("simu_page")
            ),
            tabItem(
                tabName = "pref_tab",
                pref_page_ui("pref_page")
            )
        )
    )
}

#' App simplified dashboard server function
#' @keywords internal
#' @author Ghislain Durif
simplified_index_server <- function(input, output, session) {
    home_page <- callModule(simplified_home_page_server, "home_page")
    
    ## new analysis project
    observeEvent(home_page$new_analysis_project, {
        req(home_page$new_analysis_project)
        updateTabItems(session, "app_menu", selected = "analysis_tab")
    })
    
    ## new simu project
    observeEvent(home_page$new_simu_project, {
        req(home_page$new_simu_project)
        updateTabItems(session, "app_menu", selected = "simu_tab")
    })
    
    ## analysis page
    analysis_page <- callModule(analysis_page_server, "analysis_page")
    # reset
    observeEvent(analysis_page$reset, {
        req(analysis_page$reset)
        session$reload()
        updateTabItems(session, "app_menu", selected = "analysis_tab")
    })
    
    ## simu page
    callModule(simu_page_server, "simu_page")
    
    ## preferences
    callModule(pref_page_server, "pref_page")
}

#' App dashboard sidebar
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardSidebar sidebarMenuOutput
app_sidebar <- function() {
    dashboardSidebar(
        sidebarMenuOutput("app_dynamic_menu")
    )
}

#' App dashboard body
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard dashboardBody tabItems tabItem
#' @importFrom shinyjs useShinyjs
app_body <- function() {
    dashboardBody(
        useShinyjs(),
        tab_items_ui()
    )
}

#' App dashboard server function
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard renderMenu tabItem tabItems updateTabItems
index_server <- function(input, output, session) {
    
    # init local
    local <- reactiveValues(
        current_tab = "home_page"
    )
    
    # init project list setting
    analysis_setting <- reactiveValues(
        empty_tabs = as.list(1:max_proj()),
        current_tabs = list(),
        content_list = list(),
        proj_list = list()
    )
    simu_setting <- reactiveValues(
        empty_tabs = as.list(1:max_proj()),
        current_tabs = list(),
        content_list = list(),
        proj_list = list()
    )
    
    # dynamic sidebar menu update
    observe({
        render_sidebar_menu(input, output, session, 
                            analysis_setting, simu_setting, 
                            local)
    })
    
    # dynamic tab title update
    # FIXME
    # observe({
    #     # analysis projects
    #     lapply(analysis_setting$proj_list, function(proj) {
    #         output[[ str_c("tab_", proj$id, "_title") ]] <- renderUI({
    #             tags$span(
    #                 proj$name,
    #                 HTML("&nbsp;"),
    #                 actionBttn(
    #                     str_c("close_", id),
    #                     label = NULL,
    #                     icon = icon("close"),
    #                     style = "minimal",
    #                     size = "xs"
    #                 )
    #             )
    #         })
    #     })
    #     # simu projects
    #     lapply(simu_setting$proj_list, function(proj) {
    #         output[[ str_c("tab_", proj$id, "_title") ]] <- renderUI({
    #             tags$span(
    #                 proj$name,
    #                 HTML("&nbsp;"),
    #                 actionBttn(
    #                     str_c("close_", id),
    #                     label = NULL,
    #                     icon = icon("close"),
    #                     style = "minimal",
    #                     size = "xs"
    #                 )
    #             )
    #         })
    #     })
    # })
    
    # update selected tab
    observeEvent(local$current_tab, {
        updateTabItems(session, "app_menu", selected = local$current_tab)
    })
    
    # home page server side
    home_page <- callModule(home_page_server, "home_page")
    
    # debugging
    observe({
        print(paste0("analysis current tabs = ", 
                     paste0(unlist(analysis_setting$current_tabs), collapse = " ")))
        print(paste0("analysis empty tabs = ", 
                     paste0(unlist(analysis_setting$empty_tabs), collapse = " ")))
    })
    
    # update analysis project list
    observeEvent(home_page$new_analysis_project, {
        # possible to create new project ?
        req(length(analysis_setting$empty_tabs) > 0)
        # tab id
        tab_id <- min(unlist(analysis_setting$empty_tabs))
        tab_name <- str_c("analysis_proj", tab_id)
        # update empty/current tab lists
        analysis_setting$current_tabs <- append(analysis_setting$current_tabs, 
                                              tab_id)
        analysis_setting$empty_tabs <- 
            analysis_setting$empty_tabs[-which(analysis_setting$empty_tabs 
                                                 == tab_id)]
        # update unique project count
        analysis_setting$count <- ifelse(is.null(analysis_setting$count), 
                                         0, analysis_setting$count) + 1
        # project unique id
        proj_id <- str_c("analysis_proj", analysis_setting$count)
        # update analysis project list
        # FIXME list?
        analysis_setting$proj_list[[ proj_id ]] <- reactiveValues(
            proj_id = proj_id,
            tab_id = tab_id,
            tab_name = tab_name
        )
        # server function
        analysis_setting$content_list[[ proj_id ]] <<- callModule(
            analysis_page_server, 
            str_c("mod_" , tab_name)
        )
        
        # FIXME close project
        
        # # FIXME update sidebar subitem name
        # observeEvent(analysis_setting$content_list[[ proj_id ]]$setting, {
        #     req(analysis_setting$content_list[[ proj_id ]]$setting)
        #     req(analysis_setting$content_list[[ proj_id ]]$setting$project_name)
        #     analysis_setting$proj_list[[ proj_id ]]$name <-
        #         analysis_setting$content_list[[ proj_id ]]$setting$project_name
        #     # updateTabItems(session, "app_menu", selected = proj_id)
        # })
        
        # dynamic sidebar subitem title update
        observeEvent(analysis_setting$content_list[[ proj_id ]]$setting, {
            tmp_project <- analysis_setting$content_list[[ proj_id ]]
            req(tmp_project$setting)
            req(!is.null(tmp_project$setting$project_name))
            output[[ str_c("tab_", tab_name, "_title") ]] <- renderUI({
                closable_title(id = tab_name, 
                               label = tmp_project$setting$project_name)
            })
        })
        
        # update current tab
        local$current_tab <- tab_name
    })
    
    # # update simu project list
    # observeEvent(home_page$new_simu_project, {
    #     # update project count
    #     simu_setting$count <- ifelse(is.null(simu_setting$count), 
    #                                      0, simu_setting$count) + 1
    #     # project id
    #     proj_id <- str_c("simu_proj", simu_setting$count)
    #     # update simu project list
    #     simu_setting$proj_list[[ proj_id ]] <- reactiveValues(
    #         name = proj_id, #"project_name",
    #         id = proj_id
    #     )
    #     # server function
    #     simu_setting$content_list[[ proj_id ]] <<- callModule(
    #         simu_page_server, 
    #         str_c("mod_" , proj_id),
    #         project_name = reactive(simu_setting$proj_list[[ proj_id ]]$name)
    #     )
    #     
    #     # FIXME close project
    #     
    #     # FIXME update tab Name
    #     observeEvent(simu_setting$content_list[[ proj_id ]]$setting$validate, {
    #         req(simu_setting$content_list[[ proj_id ]]$setting)
    #         req(simu_setting$content_list[[ proj_id ]]$setting$project_name)
    #         simu_setting$proj_list[[ proj_id ]]$name <-
    #             simu_setting$content_list[[ proj_id ]]$setting$project_name
    #         # updateTabItems(session, "app_menu", selected = proj_id)
    #     })
    #     
    #     # update current tab
    #     local$current_tab <- proj_id
    # })
}

#' Defines maximum number of project per module
#' @keywords internal
#' @author Ghislain Durif
max_proj <- function() {
    return(5)
}

#' Render sidebar menu function
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinydashboard renderMenu sidebarMenu updateTabItems
render_sidebar_menu <- function(input, output, session, 
                                analysis_setting, simu_setting, 
                                local) {
    # render sidebar
    output$app_dynamic_menu <- renderMenu({
        sidebarMenu(
            id = "app_menu",
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
    # select current tab
    updateTabItems(session, "app_menu", selected = local$current_tab)
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
                text = subitem_menu_title(id = proj$tab_name), 
                tabName = proj$tab_name,
                icon = icon("folder")
            )
        })
    } else {
        out <- list(menuSubItem("Empty", icon = icon("warning")))
    }
    return(out)
}

#' Return a dynamic subitem menu title
#' @keywords internal
#' @author Ghislain Durif
subitem_menu_title <- function(id) {
    uiOutput(str_c("tab_", id, "_title"))
}

#' Return a closable title
#' @keywords internal
#' @author Ghislain Durif
closable_title <- function(id, label) {
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

#' Static (with pseudo-dynamic behavior) tabItems ui
#' @keywords internal
#' @description Fix numbers of static tabs, available or not in sidebarMenu)
tab_items_ui <- function() {
    ##  home page
    static_tabs <- list(
        tabItem(tabName = "home_tab",
                home_page_ui("home_page")
        )
    )
    ## analysis tabs
    analysis_proj_list <- str_c("analysis_proj", 1:max_proj())
    analysis_tabs <- lapply(analysis_proj_list, function(proj) {
        tabItem(
            tabName = proj, 
            analysis_page_ui(str_c("mod_" , proj))
        )
    })
    ## simu tabs
    simu_proj_list <- str_c("simu_proj", 1:max_proj())
    simu_tabs <- lapply(simu_proj_list, function(proj) {
        tabItem(
            tabName = proj, 
            simu_page_ui(str_c("mod_" , proj))
        )
    })
    ## combine static and dynamic tabs
    current_tabs <- c(static_tabs,
                      unname(analysis_tabs),
                      unname(simu_tabs))
    
    ## render
    do.call(tabItems, current_tabs)
}

