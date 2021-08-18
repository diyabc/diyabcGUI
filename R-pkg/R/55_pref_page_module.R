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
            numericInput(
                ns("upload_file_size"), 
                label = "Maximum upload file size (in Mb)",
                value = 100,
                min = 10,
                max = 10000
            ),
            helpText(
                icon("warning"), "By default, data files larger than 10 Gb",
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
                icon("warning"), 
                "By default, 3/4th of all available cores are used."
            ),
            hr(),
            numericInput(
                ns("simu_loop_size"), 
                label = "Number of particles simulated in a single batch (loop-size).",
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
                "by default it is", tags$code("10 * n_core"), 
                "(so that each core will generate 10 individuals", 
                "of each batch)."
            )
        )
    )
}

#' Preference page server
#' @keywords internal
#' @author Ghislain Durif
pref_page_server <- function(input, output, session) {
    
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
}
