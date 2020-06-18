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
                ns("missing_parameter")
            ),
            uiOutput(
                ns("possible_parameters")
            ),
            numericInput(
                ns("noob"),
                label = "Number of oob testing samples",
                value = 10,
                min = 1
            ) %>% 
                helper(type = "markdown", 
                       content = "noob_parameter"),
        ),
        # textInput(
        #     ns("group"),
        #     label = "Model group"
        # ),
        # HelpText(
        #     "You may 'group' your models in several splitted groups.",
        #     "For example if you have six models, labeled from 1 to 6,", 
        #     "you can specify '1,2,3;4,5,6'."
        # ),
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
    # namespace
    ns <- session$ns
    
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
            locus_type = local$locus_type
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
                "or an arithmetic combination of them, such",
                "as division, addition or multiplication of",
                "two existing parameters. like 't/N' or 'T1+T2'.",
                tags$div(
                    style = "column-count:2;",
                    do.call(tags$ul, param_list)
                )
            )
        })
    })
    
    # check parameter input
    output$missing_parameter <- renderUI({
        if(is.null(input$paramter)) {
            if(str_length(input$parameter) == 0) {
                helpText(
                    icon("warning"), "Missing parameter."
                )
            } else {
                NULL
            }
        } else {
            NULL
        }
    })
    
    # deactivate run if no parameter in param_estim mode
    observeEvent(input$mode, {
        req(input$mode)
        if(input$mode == "param_estim") {
            if(is.null(input$parameter)) {
                shinyjs::disable("run")
            } else if(str_length(input$parameter) == 0) {
                shinyjs::disable("run")
            } else {
                shinyjs::enable("run")
            }
        }
    })
    
    # run
    observeEvent(input$run, {
        
        # print("abcranger args")
        # print("proj_dir =")
        # print(local$proj_dir)
        # print("mode =")
        # print(input$mode)
        
        # print("min_node_size =")
        # print(input$min_node_size)
        # print("noise_columns =")
        # print(input$noise_columns)
        # print("linear =")
        # print(input$linear)
        # print("pls_max_var")
        # print(input$pls_max_var)
        # print("n_tree")
        # print(input$n_tree)
        # 
        # if(input$mode == "param_estim") {
        #     print("chosen_scen =")
        #     print(input$chosen_scenario)
        #     print("noob =")
        #     print(input$noob)
        #     print("parameter =")
        #     print(input$parameter)
        # }
        
        req(!is.null(local$proj_dir))
        req(!is.null(input$mode))
        req(!is.null(input$min_node_size))
        req(!is.null(input$noise_columns))
        req(!is.null(input$linear))
        req(!is.null(input$pls_max_var))
        req(!is.null(input$n_tree))
        
        if(input$mode == "param_estim") {
            req(!is.null(input$chosen_scenario))
            req(!is.null(input$noob))
            req(!is.null(input$parameter))
            req(str_length(input$parameter) > 0)
        }
        
        logging("running abcranger")
        
        n_ref <- 0
        cmd_out <- abcranger_run(
            local$proj_dir, input$mode, n_ref, 
            input$min_node_size, input$n_tree, input$noise_columns, 
            !input$linear, 
            input$pls_max_var, input$chosen_scenario, input$noob, 
            input$parameter, 
            groups = NULL
        )
        
        # check run
        if(cmd_out$check == 0) {
            showNotification(
                id = ns("run_ok"),
                duration = 5,
                closeButton = TRUE,
                type = "message",
                tagList(
                    tags$p(
                        icon("check"),
                        "abcranger run is done."
                    )
                )
            )
        } else {
            showNotification(
                id = ns("run_not_ok"),
                duration = 5,
                closeButton = TRUE,
                type = "error",
                tagList(
                    tags$p(
                        icon("warning"),
                        "A problem during abcranger run."
                    )
                )
            )
        }
        
    })
}