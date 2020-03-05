#' Historical model choice module ui
#' @keywords internal
#' @author Ghislain Durif
hist_model_choice_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        flowLayout(
            selectInput(
                ns("select"), label = NULL, 
                choices = list("Scenario 1" = 1), 
                selected = 1
            ),
            actionGroupButtons(
                inputIds = c(ns("add"), ns("remove")),
                labels = list("Add", "Remove"),
                fullwidth = TRUE
            )
        ),
        "scenarii/parameters"
    )
}

#' Historical model choice module context init
#' @keywords internal
#' @author Ghislain Durif
hist_model_choice_module_context_init <- function() {
    out <- reactiveValues(
        candidate_scenarii = as.vector(1),
        choice_length = 1,
        current_scenario = 1)
    return(out)
}

#' Historical model choice module server
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_c
hist_model_choice_module_server <- function(input, output, session, context) {
    
    # choose scenario
    observeEvent(input$select, {
        context$scenario_choice$current_scenario <- as.numeric(input$select)
        
        print("select scenario (current/length/possible)")
        print(context$scenario_choice$current_scenario)
        print(context$scenario_choice$choice_length)
        print(context$scenario_choice$candidate_scenarii)
    })
    
    # add scenario
    observeEvent(input$add, {
        if(context$scenario_choice$choice_length < 100) {
            # update candidate scenarii id
            context$scenario_choice$candidate_scenarii <- 
                c(context$scenario_choice$candidate_scenarii,
                  max(context$scenario_choice$candidate_scenarii) + 1)
            # update candidate scenarii length
            context$scenario_choice$choice_length <- 
                context$scenario_choice$choice_length + 1
            # update current scenario
            context$scenario_choice$current_scenario <- 
                tail(context$scenario_choice$candidate_scenarii, 1)
            # update selector
            updateSelectInput(
                session, "select",
                choices = setNames(
                    as.list(context$scenario_choice$candidate_scenarii), 
                    str_c("Scenario", 
                          1:(context$scenario_choice$choice_length),
                          sep = " ")
                ),
                selected = context$scenario_choice$current_scenario
            )
        }
    })
    
    # remove scenario
    observeEvent(input$remove, {
        if(context$scenario_choice$choice_length > 1) {
            # find index of current scenario
            ind <- which(context$scenario_choice$candidate_scenarii == 
                             context$scenario_choice$current_scenario)
            # new current scenario = preceding scenario
            new_ind <- ifelse(ind > 1, ind-1, ind)
            # update candidate scenarii id
            context$scenario_choice$candidate_scenarii <- 
                context$scenario_choice$candidate_scenarii[-ind]
            # update candidate scenarii length
            context$scenario_choice$choice_length <- 
                context$scenario_choice$choice_length - 1
            # update current scenario
            context$scenario_choice$current_scenario <- 
                context$scenario_choice$candidate_scenarii[new_ind]
            # update selector
            updateSelectInput(
                session, "select",
                choices = setNames(
                    as.list(context$scenario_choice$candidate_scenarii), 
                    str_c("Scenario", 
                          1:(context$scenario_choice$choice_length),
                          sep = " ")
                ),
                selected = context$scenario_choice$current_scenario
            )
        }
    })
}

#' Analysis module ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom shinyWidgets actionGroupButtons
analysis_module_ui <- function(id, label = "analysis") {
    ns <- NS(id)
    tagList(
        sidebarPanel(
            project_input_module_ui(ns("project"), label = "Project", 
                                    default = "project_name") %>% 
                helper(type = "markdown", 
                       content = "analysis_project"),
            dir_input_module_ui(ns("directory"), label = "Folder"),
            hr(),
            data_input_file_module_ui(ns("data_file"), 
                                      label = "Data input file")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Historical model",
                    hist_model_choice_module_ui(ns("hist_model_choice"))
                ),
                tabPanel(
                    "Summary statistics",
                    "TODO"
                )
            )
        )
    )
}

#' Analysis module server
#' @keywords internal
#' @author Ghislain Durif
analysis_module_server <- function(input, output, session) {
    
    context <- reactiveValues(
        scenario_choice = hist_model_choice_module_context_init()
    )

    callModule(project_input_module_server, "project")
    callModule(dir_input_module_server, "directory")
    callModule(data_input_file_module_server, "data_file")
    
    callModule(hist_model_choice_module_server, "hist_model_choice", 
               context = context)
    
}