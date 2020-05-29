library(shiny)
server <- function(input, output, session) {
    diyabcGUI::diyabc_server(input, output, session)
}