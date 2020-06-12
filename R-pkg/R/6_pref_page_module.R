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
                ns("ncore"), 
                label = "Number of cores for multi-threading",
                value = getOption("diyabcGUI")$ncore,
                min = 1,
                max = parallel::detectCores()
            ),
            helpText(
                "By default, all available cores are used."
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
                "By default, data files larger than 100 Mb",
                "will not be accepted."
            )
        )
    )
}

#' Preference page server
#' @keywords internal
#' @author Ghislain Durif
pref_page_server <- function(input, output, session) {
    
    ## set ncore
    observeEvent(input$ncore, {
        req(is.numeric(input$ncore))
        set_diyabcGUI_options(ncore = as.integer(input$ncore))
    })
    
    
    ## set maximum upload size
    observeEvent(input$upload_file_size, {
        req(is.numeric(input$upload_file_size))
        options(
            shiny.maxRequestSize = as.integer(input$upload_file_size) * 1024^2
        )
    })
}
