#' Preference page ui
#' @keywords internal
#' @author Ghislain Durif
#' @importFrom parallel detectCores
pref_page_ui <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            title = "Preferences",
            width = 12, 
            status = "primary", solidHeader = TRUE,
            collapsible = FALSE,
            h5(tags$b("Monitoring and log")),
            tags$p("App log file: ", uiOutput(ns("log_file"), inline = TRUE)),
            selectInput(
                ns("log_level"), label = "Log level",
                choices = c("info", "debug", "trace"), selected = "info"
            ),
            hr(),
            numericInput(
                ns("upload_file_size"), 
                label = "Maximum upload file size (in Mb)",
                value = 100,
                min = 10,
                max = 10000
            ),
            helpText(
                icon("exclamation-triangle"), "By default, data files larger than 10 Gb",
                "will not be accepted.", 
                "Please increase this limit if needed."
            ),
            hr(),
            numericInput(
                ns("ncore"), 
                label = "Number of cores for multi-threading",
                value = getOption("diyabcGUI")$ncore,
                min = 1,
                max = parallel::detectCores()
            ),
            helpText(
                icon("exclamation-triangle"), 
                "By default, 3/4th of all available cores are used."
            ),
            hr(),
            numericInput(
                ns("simu_loop_size"), 
                label = "Number of particles simulated in a single batch (loop-size)",
                value = as.integer(10 * getOption("diyabcGUI")$ncore),
                min = 10,
                max = 10000
            ),
            helpText(
                "DIYABC simulation engine generates data by batch.",
                "If the batch size is 50, it means that", 
                "simulating 1000 individuals will be done in",
                "20 batches of 50 individuals.",
                "Here you can set up the batch size,", 
                "by default it is", tags$code("5 * n_core"), 
                "(so that each core will generate 5 individuals", 
                "of each batch)."
            ),
            hr(),
            actionButton(
                ns("update_bin"), 
                label = "Update DIYABC-RF internal engine",
                icon = icon("download"), width = "100%"
            ),
            helpText(icon("clock"), "Downloading files may take some time."),
            uiOutput(ns("feedback_update")),
            helpText(
                icon("comment"),
                "DIYABC-RF internal engine is based on", tags$code("diyabc"),
                "and", tags$code("abcranger"), "softwares.", 
                "See", tags$a(
                    "DIYABC-RF GUI official website", 
                    href="https://diyabc.github.io/"
                ),
                "for more details.",
                "It is recommended to update the internal engine", 
                "from time to time (to get the latest bug fixes)."
            )
        )
    )
}

#' Preference page server
#' @keywords internal
#' @author Ghislain Durif
pref_page_server <- function(input, output, session) {
    
    ## log file
    output$log_file <- renderUI({tags$code(log_file())})
    
    ## setup log level
    observeEvent(input$log_level, {
        req(input$log_level)
        logging_level(input$log_level)
    })
    
    ## set diyabcGUI options
    observe({
        req(is.numeric(input$ncore))
        req(is.numeric(input$simu_loop_size))
        set_diyabcGUI_options(
            ncore = as.integer(input$ncore),
            simu_loop_size = as.integer(input$simu_loop_size)
        )
    })
    
    ## set maximum upload size
    observeEvent(input$upload_file_size, {
        req(is.numeric(input$upload_file_size))
        options(
            shiny.maxRequestSize = as.integer(input$upload_file_size) * 1024^2
        )
    })
    
    ## update internal engine
    observeEvent(input$update_bin, {
        tmp_check <- execute_safely(dl_all_latest_bin(), session = session)
        
        output$feedback_update <- renderUI({
            req(tmp_check$check_diyabc)
            req(tmp_check$check_abcranger)
            
            tag_list1 <- switch(
                as.character(tmp_check$check_diyabc),
                "0" = tags$li(
                    "Download of", tags$code("diyabc"), "succeeded."
                ),
                "1" = tags$li(
                    icon("exclamation-triangle"),
                    "Download of", tags$code("diyabc"), "failed.",
                    "If the issue persists, please contact DIYABC-RF support."
                ),
                "-1" = tags$li(
                    tags$code("diyabc"), "was already the latest version."
                ),
                NULL
            )
            
            tag_list2 <- switch(
                as.character(tmp_check$check_abcranger),
                "0" = tags$li(
                    "Download of", tags$code("abcranger"), "succeeded."
                ),
                "1" = tags$li(
                    icon("exclamation-triangle"),
                    "Download of", tags$code("abcranger"), "failed.",
                    "If issue persists, please contact abcranger-RF support."
                ),
                "-1" = tags$li(
                    tags$code("abcranger"), "was already the latest version."
                ),
                NULL
            )
            
            
            if(!is.null(tag_list1) || !is.null(tag_list2)) {
                
                tmp_icon <- icon("info-circle")
                
                if(any(
                    c(tmp_check$check_diyabc, tmp_check$check_abcranger) != 0
                )) {
                    tmp_icon <- icon("exclamation-triangle")
                }
                
                tagList(helpText(tags$p(
                    tmp_icon, "Download status:",
                    tags$ul(tag_list1, tag_list2)
                )))
                
            } else {
                NULL
            }
        })
    })
}
