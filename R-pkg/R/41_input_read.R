#' Internal function to read header file
#' #' @keywords internal
#' #' @author Ghislain Durif
#' #' @description
#' #' Content: see doc
#' #' @param file_name string, (server-side) path to a headersim file.
#' #' @param file_type string, MIME file type.
#' #' @importFrom readr read_file
#' header_reader <- function(file_name) {
#'     
#'     ## Headers parsing
#'     header <- read_file(file_name) # whole file in one string
#'     
#'     ### First part, scenarios
#'     
#'     ## extraction of scenarios descriptions
#'     regex_scen <- "\\bscenario\\s+(\\d+)\\s+.*\\n((?:(?!(?:scenario|\\n)).*\\n)+)"
#'     
#'     # split by scenario
#'     scen_prematch <- str_extract_all(header,regex_scen)[[1]]
#'     
#'     # number of scenarios
#'     n_scen <- length(scen_prematch)
#'     
#'     # scenario match
#'     scen_match <- str_match_all(scen_prematch,rescen)
#'     
#'     ## vector of scenario order
#'     scen_ord <- unlist(lapply(
#'         1:n_scen, function(ind) return(as.integer(scen_match[[ind]][2]))
#'     ))
#'     
#'     ## vector of scenario descriptions
#'     scen_desc <- unlist(lapply(
#'         1:n_scen, function(ind) return(scen_match[[i]][3])
#'     ))
#'     scen_desc <- scen_desc[scen_ord]
#'     
#'     ### Second part, hist params
#'     
#'     
#'     ## extraction of number of parameters
#'     regex_paramtot <- "historical parameters priors \\((\\d+)\\D"
#'     ## total number of hist params in header
#'     n_paramtot <- as.integer(str_match(
#'         str_extract(header, regex_paramtot)[[1]],
#'         regex_paramtot
#'     )[2])
#'     
#'     ## extraction of parameters names (and the corresponding law too,
#'     ## in order to check if it is a dirac base, aka constant)
#'     regex_paramlist <- paste0(
#'         "\\bhistorical parameters priors.*\\n((?:\\w+\\W[^\\n]*\\n){",
#'         n_paramtot, "})"
#'     )
#'     paramlistmatch <- str_match(str_extract_all(header,reparamlist),reparamlist)[2]
#'     ## regexp to extract : param name, mini and maximum of the law
#'     reparam <- "(\\w+)\\W+\\w\\W+\\w\\w\\[([^,\\]]+),([^,\\]]+)[,\\]][^\\n]*\\n"
#'     paramsh <- str_match_all(str_extract_all(paramlistmatch,reparam)[[1]],reparam)
#'     paramsdesc <- list()
#'     reali <- 1;
#'     ## loop on all hist parameters to "filter" all constants
#'     ## parameters
#'     for(i in 1:nparamtoth)
#'     {
#'         mini <- as.numeric(paramsh[[i]][3])
#'         maxi <- as.numeric(paramsh[[i]][4])
#'         if(maxi != 0.0)
#'             if ((maxi-mini)/maxi > 0.000001)
#'             {
#'                 paramsdesc[paramsh[[i]][2]] <- reali
#'                 reali <- reali + 1
#'             }
#'     }
#'     realparamtot <- reali - 1 # this is the real number of non constant
#'     # historical parameters
#'     
#'     ### Third part, extract parameters for each scenario
#'     # parambyscenh stores the indices of used parameters for each
#'     # scenario
#'     parambyscenh <- vector(mode="numeric",length=nscenh)
#'     # Loop for all scenarios
#'     for(i in 1:nscenh)
#'     {
#'         templist <- list()
#'         # list of terms used in scenario descriptions
#'         listterms <- strsplit(scendesc[i],"\\W")[[1]]
#'         m <- 1
#'         for(j in 1:length(listterms)) 
#'         {
#'             if (!is.null(paramsdesc[listterms[j]][[1]]))
#'             {
#'                 templist[m] <- paramsdesc[listterms[j]][[1]]
#'                 m <- m + 1
#'             }
#'         }
#'         parambyscenh[i] <- list((unique(unlist(templist))))
#'     }
#'     
#'     # Get all names
#'     restatsname <- "\\n\\nscenario\\s+.*"
#'     allcolspre <- tail(strsplit(str_extract(header,restatsname),"\\s+")[[1]],-2)
#'     
#' }


#' Read and parse headerRF.txt and header.txt file
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Content: see doc
#' @param file_name string, (server-side) path to a headersim file.
#' @param file_type string, MIME file type.
#' @importFrom readr read_file
read_header <- function(file_name, file_type) {
    
    # init output
    out <- list(
        msg = list(), valid = TRUE,
        data_file = NULL, loci_desc = NULL, 
        n_param = NULL, n_prior = NULL, n_sumstat = NULL, 
        cond_list = NULL, prior_list = NULL, group_prior_list = NULL, 
        n_scenario = NULL, scenario_list = NULL, 
        simu_mode = NULL
    )
    
    current_line <- 0
    
    # check file_name
    tmp <- check_file_name(file_name)
    if(!tmp) {
        out$valid <- FALSE
        msg <- tagList("Invalid file file.")
        out$msg <- append(out$msg, list(msg))
    }
    
    # check file_type
    if(file_type != "text/plain") {
        out$valid <- FALSE
        msg <- tagList("Wrong file type.")
        out$msg <- append(out$msg, list(msg))
    }
    
    # continue ?
    if(!out$valid) {
        return(out)
    }
    
    ## HEADER FILE CONTENT
    # read whole file in one string and split it by new line
    header <- str_split(read_file(file_name), "\n", simplify = TRUE)
    
    ## data file
    out$data_file <- header[1]
    header <- header[-1]
    current_line <- current_line + 1
    
    ## number of parameters and statistics
    strng <- header[1]
    header <- header[-1]
    current_line <- current_line + 1
    pttrn <- "^[0-9]+ parameters and [0-9]+ summary statistics$"
    if(!str_detect(strng, pttrn)) {
        out$valid <- FALSE
        msg <- tagList(
            "Issue with line", tags$b(as.character(current_line)), 
            "that should match the pattern", tags$code(pttrn), "."
        )
        out$msg <- append(out$msg, list(msg))
    } else {
        pttrn <- "[0-9]+(?= parameters)"
        out$n_param <- as.integer(str_extract(strng, pttrn))
        pttrn <- "[0-9]+(?= summary statistics)"
        out$n_sumstat <- as.integer(str_extract(strng, pttrn))
    }
    
    ## empty line
    strng <- header[1]
    header <- header[-1]
    current_line <- current_line + 1
    if(strng != "") {
        out$valid <- FALSE
        msg <- tagList(
            "Issue with line", tags$b(as.character(current_line)), 
            "that should be empty."
        )
        out$msg <- append(out$msg, list(msg))
    }
    
    ## scenarios
    pttrn <- "^[0-9]+ scenarios:( [0-9]+)+ ?$"
    strng <- header[1]
    header <- header[-1]
    current_line <- current_line + 1
    # find section
    if(!str_detect(strng, pttrn)) {
        out$valid <- FALSE
        msg <- tagList(
            "Issue with line", tags$b(as.character(current_line)), 
            "that should match the pattern", tags$code(pttrn), "."
        )
        out$msg <- append(out$msg, list(msg))
    } else {
        pttrn <- "[0-9]+(?= scenarios:)"
        out$n_scenario <- as.integer(str_extract(strng, pttrn))
        pttrn <- "(?<= )[0-9]+"
        nrow_per_scenario <- as.integer(unlist(str_extract_all(strng, pttrn)))
        # extract scenarii
        row_seq <- cumsum(c(1, nrow_per_scenario+1))
        scenario_list <- lapply(
            split(
                raw_content[(min(line_seq):(max(line_seq)-1)) 
                            + next_sec_line], 
                rep(seq(line_seq), diff(c(line_seq, max(line_seq))))
            ), 
            function(content) {
                parse_diyabc_header_scenarii(content)
            })
        # check extracted scnenarii
        valid <- all(unlist(lapply(scenario_list, 
                                   function(item) item$valid)))
        # extract raw scenario list
        raw_scenario_list <- unlist(unname(
            lapply(
                scenario_list, 
                function(item) item$raw_scenario
            )
        ))
        # next section
        next_sec_line <- next_sec_line + max(line_seq)
    }
    
    
    # output
    return(out)
}


#' Parse scenario in header file
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Content: see doc
#' @param content string vector, scenario description
parse_scenario_header <- function(content) {
    
    # init output
    out <- list(
        valid = TRUE,
        id = NULL, n_param = NULL,
        prior = NULL, scenario = NULL
    )
    
    # description (1st line)
    strng <- content[1]
    pttrn <- str_c("^scenario [0-9]+ \\[", num_regex(), "\\] \\([0-9]+\\)$")
    if(!str_detect(strng, pttrn)) {
        out$valid <- FALSE
    } else {
        # scenario id
        pttrn <- "(?<=^scenario )[0-9]+"
        out$id <- as.integer(str_extract(strng, pttrn))
        # scenario prior
        pttrn <- str_c("(?<= \\[)", num_regex(), "(?=\\] )")
        out$prior <- as.numeric(str_extract(strng, pttrn))
        # number of parameters in scenario
        pttrn <- "(?<= \\()[0-9]+(?=\\)$)"
        out$n_param <- as.integer(str_extract(strng, pttrn))
        ## scenario
        out$scenario <- str_c(content[-1], collapse = "\n")
    }
    ## output
    return(out)
}



#' Read and parse statobsRF.txt file
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Content: see doc
#' @param file_name string, (server-side) path to a headersim file.
#' @param file_type string, MIME file type.
#' @param n_stat integer, number of summary statistics in reftable.
read_statobs <- function(file_name, file_type, n_stat) {
    
    # init output
    out <- list(msg = list(), valid = TRUE)
    
    # check file_name
    tmp <- check_file_name(file_name)
    if(!tmp) {
        out$valid <- FALSE
        msg <- tagList("Invalid file file.")
        out$msg <- append(out$msg, list(msg))
    }
    
    # check file_type
    if(file_type != "text/plain") {
        out$valid <- FALSE
        msg <- tagList("Wrong file type.")
        out$msg <- append(out$msg, list(msg))
    }
    
    # continue ?
    if(!out$valid) {
        return(out)
    }
    
    # try reading it
    tmp <- tryCatch(
        read.table(file_name), 
        error = function(e) return(NULL)
    )
    
    if(is.null(tmp)) {
        out$valid <- FALSE
        msg <- tagList("Issue with statobs file format.")
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # check number of rows and columns
    if(nrow(tmp) != 2) {
        out$valid <- FALSE
        msg <- tagList(
            "The statobs file is supposed to contain two lines:",
            "one with the names of the summary statistics,",
            "and a second with the corresponding values",
            "computed on the dataset."
        )
        out$msg <- append(out$msg, list(msg))
    }
    
    if(ncol(tmp) != n_stat) {
        out$valid <- FALSE
        msg <- tagList(
            "The number of summary statistics in the statobs file",
            "does not correspond to the number of summary statistics",
            "in the reftable file."
        )
        out$msg <- append(out$msg, list(msg))
    }
    
    # output
    return(out)
}

#' Read and parse reftableRF.bin file
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Content: see doc
#' @param file_name string, (server-side) path to a headersim file.
#' @param file_type string, MIME file type.
read_reftable <- function(file_name, file_type) {
    
    # init output
    out <- list(msg = list(), valid = TRUE)
    
    # check file_name
    tmp <- check_file_name(file_name)
    if(!tmp) {
        out$valid <- FALSE
        msg <- tagList("Invalid file file.")
        out$msg <- append(out$msg, list(msg))
    }
    
    # check file_type
    if(file_type != "application/octet-stream") {
        out$valid <- FALSE
        msg <- tagList("Wrong file type.")
        out$msg <- append(out$msg, list(msg))
    }
    
    # continue ?
    if(!out$valid) {
        return(out)
    }
    
    # output
    return(out)
}