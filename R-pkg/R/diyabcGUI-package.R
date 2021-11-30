#' @name diyabcGUI-package
#' @aliases diyabcGUI
#' @docType package
#' @title diyabcGUI
#' 
#' diyabcGUI package
#' 
#' @author Ghislain Durif
#' @importFrom fs path_home dir_copy dir_delete file_copy file_delete
#' @importFrom ggplot2 ggplot ggtitle geom_point geom_segment geom_label 
#' geom_text geom_vline geom_density aes aes_string
#' theme_void theme element_text element_blank 
#' margin scale_x_continuous scale_y_continuous 
#' unit xlim ylim
#' theme_bw xlab ylab
#' @importFrom logger log_errors log_eval log_messages log_shiny_input_changes
#' log_threshold 
#' log_fatal log_error log_warn log_success log_info log_debug log_trace
#' @importFrom magrittr %>%
#' @importFrom parallel detectCores
#' @importFrom processx process
#' @import shiny
#' @importFrom shinybusy add_busy_spinner
#' @importFrom shinydashboard box updateTabItems
#' @importFrom shinyFeedback feedbackWarning
#' @importFrom shinyhelper helper
#' @importFrom shinyjs disable enable disabled hidden hide reset show
#' @importFrom shinyWidgets actionBttn actionGroupButtons ask_confirmation progressBar 
#' radioGroupButtons show_alert updateProgressBar execute_safely
#' @importFrom stringr str_c str_detect str_extract str_extract_all str_length 
#' str_match str_pad str_replace str_replace_all str_split str_to_upper str_to_lower str_trim
#' @importFrom tibble lst
#' @importFrom tidyr drop_na
#' @importFrom tools file_ext
#' @importFrom utils download.file head read.csv read.table tail unzip
#' @importFrom zip zip
"_PACKAGE"