#' Training set sub-module ui
#' @keywords internal
#' @author Ghislain Durif
training_set_ui <- function(id) {
    ns <- NS(id)
    tagList(
        hist_model_panel_ui(ns("hist_model_panel"))
    )
}

#' Training set sub-module server
#' @keywords internal
#' @author Ghislain Durif
training_set_server <- function(input, output, session) {
    # init output
    out <- reactiveValues()
    # historic model panel
    callModule(hist_model_panel_server, "hist_model_panel")
    # output
    return(out)
}

#' Historical model input panel module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyWidgets actionGroupButtons
hist_model_panel_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(
                width = 6,
                actionGroupButtons(
                    inputIds = c(ns("add"), 
                                 ns("remove")),
                    labels = list(
                        tags$span(icon("plus"), "Add"),
                        tags$span(icon("minus"), "Remove")
                    ),
                    fullwidth = TRUE
                )
            ),
            column(
                width = 6,
                uiOutput(ns("scenario_nb"))
            )
        ),
        uiOutput(ns("scenario_tabs"))
    )
}

#' Historical model input panel module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_c
hist_model_panel_server <- function(input, output, session) {
    # namespace
    ns <- session$ns
    # init local
    local <- reactiveValues(
        current_scenario = 1,
        current_tab = NULL,
        scenario_id_list = 1,
        scenario_list = list(s1 = new_scenario(1)),
        scenario_nb = 1
    )
    # init output
    out <- reactiveValues()
    ## add scenario
    observeEvent(input$add, {
        if(local$scenario_nb < 20) {
            # new scenario id
            new_id <- max(local$scenario_id_list) + 1
            # increment number of scenarii
            local$scenario_nb <- local$scenario_nb + 1
            # new current scenario
            local$current_scenario <- local$scenario_nb
            # update list of scenario ids
            local$scenario_id_list <- c(local$scenario_id_list, 
                                        new_id)
            # update list of scenarii
            local$scenario_list[[ scenario_key(new_id) ]] <- new_scenario(new_id)
        }
    })
    ## remove scenario
    observeEvent(input$remove, {
        if(local$scenario_nb > 1) {
            # find index of current scenario
            current_id <- local$scenario_list_id[local$current_scenario]
            # update list of scenario ids
            local$scenario_id_list <- local$scenario_id_list[-local$current_scenario]
            # update list of scenarii
            local$scenario_list[[ scenario_key(current_id)]] <- NULL
            local$current_scenario <- ifelse(local$current_scenario > 1, 
                                             local$current_scenario - 1,
                                             local$current_scenario)
            local$scenario_nb <- local$scenario_nb - 1
        }
    })
    ## output scenario number
    observeEvent(local$scenario_nb, {
        logging("scenario nb = ", local$scenario_nb)
        output$scenario_nb <- renderUI({
            helpText(
                str_c("Current number of scenario = ", local$scenario_nb)
            )
        })
    })
    ## update scenario tabs
    observeEvent(local$scenario_nb, {
        output$scenario_tabs <- renderUI({
            tabs <- lapply(
                1:local$current_scenario,
                function(ind) {
                    tabPanel(
                        title = str_c("Scenario ", ind),
                        value = as.character(ind),
                        str_c("scenario key = ", 
                              scenario_key(local$scenario_id_list[ind]))
                    )
                })
            tabs <- c(tabs, list(id = ns("scenario_tabs")))
            do.call(tabsetPanel, tabs)
        })
        # update current tab
        updateTabsetPanel(session, "scenario_tabs",
                          selected = as.character(local$current_scenario))
    })
    # ## tab switch
    # FIXME
    # observeEvent(input$scenario_tabs, {
    #     req(input$scenario_tabs)
    #     local$current_tab <- input$scenario_tabs
    # })
    # observeEvent(local$current_tab, {
    #     logging("current tab = ", local$current_tab)
    #     local$current_scenario <- as.numeric(local$current_tab)
    # })
    ## debugging
    observeEvent(local$current_scenario, {
        logging("current scenario = ", local$current_scenario)
    })
}

#' New scenario function
#' @keywords internal
#' @author Ghislain Durif
new_scenario <- function(id) {
    return(list(id = id, raw_scenario = NULL))
}

#' Scenario id key
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_c
scenario_key <- function(id)
    return(str_c("s", id))
