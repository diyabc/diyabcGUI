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
                value = as.integer(parallel::detectCores()/2),
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
#' @importFrom parallel detectCores
pref_page_server <- function(input, output, session) {
    
    # # init diyabcGUI options
    # diyabcGUI_options <- list(
    #     ncore = parallel::detectCores()
    # )
    # options("diyabcGUI" = diyabcGUI_options)
    
    # # init maximum upload size
    # options(shiny.maxRequestSize = 100 * 1024^2)
    
    ## set ncore
    observeEvent(input$ncore, {
        req(is.numeric(input$ncore))
        diyabcGUI_options <- list(
            ncore = as.integer(input$ncore)
        )
        options("diyabcGUI" = diyabcGUI_options)
    })
    
    
    ## set maximum upload size
    observeEvent(input$upload_file_size, {
        req(is.numeric(input$upload_file_size))
        options(
            shiny.maxRequestSize = as.integer(input$upload_file_size) * 1024^2
        )
    })
}
