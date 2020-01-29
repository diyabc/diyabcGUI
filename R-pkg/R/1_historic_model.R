#' Historical model ui
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
hist_model_ui <- function(add=FALSE) {
    tagList(
        verticalLayout(
            flowLayout(
                textAreaInput(
                    "scenario", 
                    label = "Describe your scenario", 
                    value = "N1 N2\n0 sample 1\n0 sample 2\nt sample 1\nt2 merge 1 2", rows = 10),
                plotOutput("scenario_graph", height = "200px")
            ),
            verticalLayout(
                h3("Parameters"),
                flowLayout(
                    uiOutput("param_value"),
                    uiOutput("sample_param"),
                    numericInput(
                        "nsimu", 
                        label = "Number of repetitions", 
                        value = 1
                    )
                )
            )
        )
    )
}

#' Historical model server
#' @keywords internal
#' @author Ghislain Durif
#' @import shiny
hist_model_server <- function(input, output, session) {
    
    # tmp graph
    # FIXME
    output$scenario_graph <- renderPlot({ plot( (1:3), c(0,1,0), 
                                                type = "l", 
                                                axes = FALSE,
                                                xlab = "",
                                                ylab = "")})
    
    # scenario
    scenario_param <- reactive({
        parse_scenario(input$scenario)
    })
    
    # parameters
    output$param_value <- renderUI({
        
        param_list <- scenario_param()$parameters
        print(paste0(param_list))
        
        # numeric input for each parameters
        numeric_input_list <- list()
        if(length(param_list) > 0) {
            numeric_input_list <- lapply(param_list, function(param) {
                numericInput(param, label = param, value = 100)
            })
        }
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, numeric_input_list)
    })
}


#' Parse scenario
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom stringr str_count str_extract_all str_split
#' @importFrom tibble lst
parse_scenario <- function(text) {
    if(is.null(text)) return(NULL)
    keywords <- c("sample", "varNe", "merge", "split")
    # parameters
    parameters <- unique(
                    unlist(
                        str_extract_all(
                            text, 
                            pattern = paste0(c("[a-zA-Z][a-zA-Z0-9]*(?= |\n)", 
                                               "[a-zA-Z][a-zA-Z0-9]*$"), 
                                             collapse = "|"))))
    parameters <- parameters[ ! parameters %in% keywords ]
    # number of populations
    tmp <- str_split(text, pattern = "\n", simplify = TRUE)
    npop <- str_count(tmp[1], pattern = " ") + 1
    # out
    return(lst(parameters, npop))
}
