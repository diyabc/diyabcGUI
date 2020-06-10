#' Randon forest submodule ui
#' @keywords internal
#' @author Ghislain Durif
rf_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h4("Settings"),
        radioButtons(
            ns("mode"), 
            label = "Mode",
            choices = list("Model choice" = "mod_choice", 
                           "Parameter estimation" = "param_estim"), 
            selected = "mod_choice"
        ),
        hr(),
        numericInput(
            ns("min_node_size"),
            label = "Minimal node size",
            min = 0,
            value = 0
        ),
        helpText(
            "0 means 1 for classification or 5 for regression."
        ),
        numericInput(
            ns("n_tree"),
            label = "Number of trees",
            min = 1,
            value = 500
        ),
        numericInput(
            ns("noise_columns"),
            label = "Number of noise columns",
            min = 0,
            value = 5
        ),
        h5(tags$b("Linear framework")),
        checkboxInput(
            ns("linear"),
            label = tags$span(
                "Enable/Disable LDA for model choice or PLS for", 
                "parameter estimation"
            ),
            value = TRUE
        ),
        # if parameter estimation
        conditionalPanel(
            condition = "input.mode == 'param_estim'",
            ns = ns,
            numericInput(
                ns("pls_max_var"),
                label = "PLS explained variance threshold",
                min = 0.001,
                max = 0.999,
                value = 0.9,
                step = 0.001
            ),
            helpText(
                "Percentage of maximum explained Y-variance", 
                "for retaining pls axis"
            ),
            numericInput(
                ns("chosen_scenario"),
                label = "Chosen scenario",
                value = 1,
                min = 1
            ),
            textInput(
                ns("parameter"),
                label = "Parameter to estimate"
            ),
            uiOutput(
                ns("possible_parameters")
            ),
            numericInput(
                ns("noobs"),
                label = "Number of oob testing samples",
                value = 10,
                min = 1
            ) %>% 
                helper(type = "markdown", 
                       content = "noob_parameter"),
        ),
        hr(),
        actionBttn(
            inputId = ns("run"),
            label = "Run",
            style = "fill",
            block = TRUE
        )
    )
}


#' Randon forest submodule server
#' @keywords internal
#' @author Ghislain Durif
rf_module_server <- function(input, output, session, 
                             locus_type = reactive({NULL}),
                             proj_dir = reactive({NULL}),
                             # proj_file_list = reactive({NULL}),
                             valid_proj = reactive({FALSE})) {
    # init local
    local <- reactiveValues(
        locus_type = NULL,
        proj_dir = NULL,
        proj_header_file = NULL,
        proj_file_list = NULL,
        proj_header = NULL,
        valid_proj = NULL
    )
    
    # get input
    observe({
        local$locus_type <- locus_type()
        local$proj_dir <- proj_dir()
        # local$proj_file_list <- proj_file_list()
        local$valid_proj <- valid_proj()
    })
    
    # check project directory
    observe({
        proj_file_list <- reactivePoll(
            1000, session,
            checkFunc = function() {
                if(!is.null(local$proj_dir)) {
                    if(dir.exists(local$proj_dir))
                        list.files(local$proj_dir)
                    else
                        list()
                } else {
                    list()
                }
            },
            valueFunc = function() {
                if(!is.null(local$proj_dir)) {
                    if(dir.exists(local$proj_dir))
                        list.files(local$proj_dir)
                    else
                        list()
                } else {
                    list()
                }
            }
        )
        
        local$proj_file_list <- proj_file_list()
    })
    
    # # debugging
    # observe({
    #     print("proj_file_list")
    #     print(local$proj_file_list)
    # })
    
    # enable run
    observeEvent(local$proj_file_list, {
        # FIXME (for click-button training set def)
        req(!is.null(local$proj_dir))
        req(dir.exists(local$proj_dir))
        req(!is.null(local$proj_file_list))

        # # debugging
        # print("project files")
        # print(local$proj_file_list)

        mandatory_files <- c("headerRF.txt", "statobsRF.txt", "reftableRF.bin")
        if(!local$valid_proj & 
           !all(mandatory_files %in% local$proj_file_list)) {
            shinyjs::disable("run")
        } else {
            shinyjs::enable("run")
        }
    })

    # check headerRF.txt file
    observe({
        req(!is.null(local$proj_dir))

        proj_header_file <- reactiveFileReader(
            1000, session,
            file.path(local$proj_dir, "headerRF.txt"),
            function(file) {
                if(file.exists(file))
                    readLines(file)
                else
                    list()
            }
        )

        local$proj_header_file <- proj_header_file()
    })
    
    # # debugging
    # observe({
    #     print("proj_header_file")
    #     print(local$proj_header_file)
    # })
    
    # possible scenario and possible parameters
    observeEvent(local$proj_header_file, {
        req(local$proj_header_file)
        
        file_check <- parse_diyabc_header(
            file_name = file.path(local$proj_dir, "headerRF.txt"),
            file_type = "text/plain",
            data_type = local$locus_type
        )
        
        # # debugging
        # logging("number of scenario = ", length(file_check$raw_scenario_list))
        
        # update corresponding input
        updateNumericInput(
            session, "chosen_scenario", 
            max = length(file_check$raw_scenario_list)
        )
        
        # possible parameters
        output$possible_parameters <- renderUI({
            
            param_list <- lapply(
                file_check$raw_prior_list,
                function(item) {
                    return(
                        tags$li(
                            str_extract(
                                item,
                                single_param_regex()
                            )
                        )
                    )
                }
            )
            
            helpText(
                "You can use one of the following parameter",
                "or an arithmetic combination of them.",
                tags$div(
                    style = "column-count:2;",
                    do.call(tags$ul, param_list)
                )
            )
        })
    })
    
    # possible parameters
    # TODO
    
    # run
    observeEvent(input$run, {
        abcranger_run(
            proj_dir, mode, nref, 
            min_mode_size, n_tree, noisecolumns, no_linear, 
            pls_max_var, chosen_scen, noob, parameter, 
            groups = NULL
        )
    })
}