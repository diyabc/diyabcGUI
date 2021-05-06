#' Read and parse IndSeq SNP data file
#' @keywords internal
#' @author Ghislain Durif
#' @param data_file string, data file name.
#' @param data_dir string, path to directory where data file is stored.
#' @importFrom tools file_ext
#' @importFrom parallel makeCluster stopCluster
#' @importFrom pbapply pblapply
read_indseq_snp_data <- function(data_file, data_dir) {
    
    # init output
    out <- list(
        msg = list(), valid = TRUE,
        data_file = NULL, n_loci = NULL, locus_count = NULL, locus_desc = NULL, 
        n_pop = NULL, n_indiv = NULL,
        sex_ratio = NULL, maf = NULL
    )
    
    current_line <- 0
    
    # full path
    file_name <- file.path(data_dir, data_file)
    
    # check file_name
    tmp <- check_file_name(file_name)
    if(!tmp) {
        out$valid <- FALSE
        msg <- tagList("Invalid data file name.")
        out$msg <- append(out$msg, list(msg))
    }
    
    # check file content
    if(file.info(file_name)$size == 0) {
        out$valid <- FALSE
        msg <- tagList("Data file is empty.")
        out$msg <- append(out$msg, list(msg))
    }
    
    # check file_type
    if(tools::file_ext(file_name) != "snp") {
        out$valid <- FALSE
        msg <- tagList(
            "IndSeq SNP files should have an extension",
            tags$code(".snp"), "."
        )
        out$msg <- append(out$msg, list(msg))
    }
    
    # continue ?
    if(!out$valid) {
        return(out)
    }
    
    # data file name
    out$data_file <- data_file
    
    ## DATA FILE CONTENT
    
    ## HEADER 1
    header1 <- readLines(file_name, n = 1, warn = FALSE)
    
    # sex ratio
    pttrn <- "(?i)NM=[0-9]+\\.?[0-9]*NF(?-i)"
    if(!str_detect(header1, pttrn)) {
        out$valid <- FALSE
        msg <- tagList(
            "Missing", tags$b("sex ratio"), "in header1 first line."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    out$sex_ratio <- str_extract(header1, pttrn)
    
    # MAF
    pttrn <- "(?i)(?<=MAF=)([0-9]+\\.?[0-9]*|hudson)(?-i)"
    if(!str_detect(header1, pttrn)) {
        out$valid <- FALSE
        msg <- tagList(
            "Missing", tags$b("Minimum Allele Frequency"), 
            "(MAF) in header1 first line.",
            "MAF should be a real number between 0 and 1", 
            "or the keyword", tags$code("hudson"),
            ", see manual."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    out$maf <- str_extract(header1, pttrn)
    
    if(str_detect(out$maf, "^[0-9]+\\.?[0-9]*$")) {
        out$maf <- as.numeric(out$maf)
        if(out$maf < 0 || out$maf > 1) {
            out$valid <- FALSE
            msg <- tagList(
                tags$b("Minimum Allele Frequency"), 
                "(MAF) should be a real number between 0 and 1", 
                "or the keyword", tags$code("hudson"),
                ", see manual."
            )
            out$msg <- append(out$msg, list(msg))
            return(out)
        }
    }
    
    ## HEADER 2
    header2 <- tryCatch(
        unname(unlist(read.table(file = file_name, skip = 1, nrows = 1))), 
        error = function(e) return(e)
    )
    if("error" %in% class(header2)) {
        out$valid <- FALSE
        msg <- tagList(
            "Formatting issue with second line header, ",
            "impossible to read it, see manual."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # upper case
    header2 <- str_to_upper(header2)
    
    # header 2 content
    if(header2 != "IND" && header2[2] != "SEX" && header2[3] != "POP" &&
       !all(header2[-(1:3)] %in% c("A", "H", "X", "Y", "M"))) {
        out$valid <- FALSE
        msg <- tagList(
            "Formatting issue with header second line, see manual.",
            "Required column titles are", tags$code("IND SEX POP"), 
            "followed by a letter indicator among", 
            tags$code("A"), tags$code("H"), tags$code("X"), 
            tags$code("Y"), tags$code("M"),
            "for each SNP locus in the file (see manual)."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # nb of locus
    out$n_loci <- length(header2) - 3
    
    # locus type
    candidate_locus <- c("A", "H", "X", "Y", "M")
    locus_encoding <- str_c(header2[-(1:3)], collapse = " ")
    out$locus_count <- Reduce("rbind", lapply(
        candidate_locus, 
        function(pttrn) {
            count <- str_count(locus_encoding, pttrn)
            return(data.frame(
                count = count,
                type = pttrn,
                stringsAsFactors = FALSE
            ))
        }
    ))
    
    # save snp type for filtering
    snp_type <- header2[-(1:3)]
    
    ## DATA FILE CONTENT
    content <- tryCatch(
        read.table(file_name, skip = 2), error = function(e) return(e)
    )
    if("error" %in% class(content)) {
        out$valid <- FALSE
        msg <- tagList(
            "Formatting issue with data, ",
            "impossible to read them, see manual."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # check for SEX column with only F (interpreted as FALSE)
    if(is.logical(content[,2])) {
        content[,2] <- ifelse(content[,2], "T", "F")
    }
    
    # check number of locus
    if(out$n_loci != (ncol(content) - 3)) {
        out$valid <- FALSE
        msg <- tagList(
            "Number of loci not consistent between",
            "file header and file content."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # check sex content
    if(!all(str_to_upper(as.character(content[,2])) %in%
            c("F", "M", "9"))) {
        out$valid <- FALSE
        msg <- tagList(
            tags$code("SEX"), "column should only contain",
            tags$code("F"), "for female,",
            tags$code("M"), "for male or",
            tags$code("9"), "for missing values (see manual)."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # number of pop
    out$n_pop <- length(unique(content[,3]))
    
    # number of individuals
    out$n_indiv <- nrow(content)
    
    ## REFORMAT DATA (to speed up the checks)
    
    # individual information
    indiv_info <- content[,1:3]
    colnames(indiv_info) <- c("IND", "SEX", "POP")
    
    # check for any missing values
    if(any(is.na(indiv_info))) {
        out$valid <- FALSE
        msg <- tagList(
            tags$code("NA"), "values were found",
            "in one (or more) of the columns",
            tags$code("IND"), tags$code("SEX"), "or", tags$code("POP"), "."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # SNP info
    content <- t(as.matrix(content[,-(1:3)]))
    
    # check for any missing values
    if(any(is.na(content))) {
        out$valid <- FALSE
        msg <- tagList(
            tags$code("NA"), "values were found",
            "in one (or more) of the columns encoding the SNPs."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # check data type
    if(!is.integer(content)) {
        out$valid <- FALSE
        msg <- tagList(
            "SNP encoding should be only contain integer values."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # check data encoding
    if(!all(content %in% c(0,1,2,9))) {
        out$valid <- FALSE
        msg <- tagList(
            "SNP encoding should only be",
            tags$code("0"), tags$code("1"), tags$code("2"),
            "or", tags$code("9"), "for missing data (see manual)."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    
    
    
    
    
    
    
    #     ## filtering locus
    #     if(valid) {
    #         filtered_locus <- filter_snp_indseq(
    #             content, indiv_info, snp_type, locus_details, maf
    #         )
    #         if(is.null(filtered_locus)) {
    #             err <- append(
    #                 err,
    #                 str_c(
    #                     "Issue with IndSeq SNP file data content:",
    #                     "error during SNP filtering, ",
    #                     "probable issue with SNP data encoding (see manual).",
    #                     sep = " "
    #                 )
    #             )
    #             valid <- FALSE
    #         } else {
    #             locus <- unname(unlist(lapply(
    #                 split(filtered_locus, seq(nrow(filtered_locus))),
    #                 function(item) {
    #                     if(item$count > 0)
    #                         return(str_c(item$count - item$filter,
    #                                      " <", item$type, ">"))
    #                     else
    #                         return(NULL)
    #                 }
    #             )))
    #             locus_msg <- unname(unlist(lapply(
    #                 split(filtered_locus, seq(nrow(filtered_locus))),
    #                 function(item) {
    #                     if(item$count > 0) {
    #                         item_type <- str_c("<", item$type, ">")
    #                         txt <- str_c(
    #                             item$count - item$filter, item_type, sep = " "
    #                         )
    #                         if(item$filter > 0) {
    #                             txt <- str_c(
    #                                 item$count - item$filter, item_type,
    #                                 "(note:", item$filter, item_type,
    #                                 "loci are filtered out",
    #                                 "based on MAF criterion)", sep = " "
    #                             )
    #                         }
    #                         if(item$mono > 0) {
    #                             str_c(
    #                                 item$count - item$filter, item_type,
    #                                 "(note:", item$filter, item_type,
    #                                 "loci, including ",
    #                                 item$mono, item_type,
    #                                 "monomorphic loci, are filtered out",
    #                                 "based on MAF criterion)", sep = " "
    #                             )
    #                         }
    #                         return(txt)
    #                     } else
    #                         return(NULL)
    #                 }
    #             )))
    #             msg <- append(
    #                 msg,
    #                 str_c("Total number of loci =", n_loci, sep = " ")
    #             )
    #             msg <- append(
    #                 msg,
    #                 str_c("Available loci:", str_c(locus_msg, collapse =  "; "),
    #                       sep = " ")
    #             )
    #         }
    #     }
    #     ## output
    #     spec <- lst(locus, n_indiv, n_loci, n_pop)
    # }

    ## output
    return(out)
}

#' Processing individual locus specific in IndSeq SNP data file based on MAF
#' @keywords internal
#' @author Ghislain Durif
#' @param snp_data integer vector encoding for each individual the
#' number of ancestral allele for the loci, i.e. `0`, `1` and `2` for 
#' autosome (`A`) and X-chromosome (`X`) in female, or `0` and `1` for 
#' haploid locus (`H`), Y-chromosome (`Y`) in male and 
#' mitochondrial locus (`M`). 
#' Note : missing values are encoded by a `9`.
#' @param sex_data character vector encoding for each individual the sex, i.e. 
#' `"F"` for female and `"M"` for male.
#' Note : missing values are encoded by a `"9"`.
#' @param pop_data character vector encoding each individual population.
#' @param snp_type character encoding the type of the locus 
#' (among `A`, `H`, `X`, `Y`, `M`).
#' @param maf double between 0 and 1
process_indseq_locus <- function(snp_data, sex_data, pop_data, snp_type, maf) {
    
    # init local
    local <- list(
        valid = TRUE, filt = FALSE, mono = FALSE,
        missing_pop = NA, issue_X = NA, issue_Y = NA, af = 0,
        hudson = FALSE
    )
    
    ## identify excessive missing data for a single pop
    is_missing_pop <- tapply(
        snp_data, as.factor(pop_data), function(x) return(x == 9)
    )
    check_missing_pop <- unlist(lapply(
        1:length(is_missing_pop),
        function(ind)
            return(sum(is_missing_pop[[ind]]) == length(is_missing_pop[[ind]]))
    ))
    if(any(check_missing_pop)) {
        local$valid <- FALSE
        local$missing_pop <- str_c(
            names(is_missing_pop)[check_missing_pop], collapse = ","
        )
    }
    
    ## identify missing data and compute data size without missing data
    non_missing_data <- (snp_data != 9)
    true_data_size <- sum(non_missing_data)
    ## identify female, male, and missing sex
    female_ind <- (sex_data == "F")
    male_ind <- (sex_data == "M")
    missing_sex <- (sex_data == "9")
    
    if(snp_type == "A") {
        ## autosome
        if(true_data_size > 0) {
            # reference allele frequence
            local$af <- sum(snp_data[non_missing_data]) / (2 * true_data_size)
        }
    } else if(snp_type %in% c("H", "M")) {
        ## haploid & mitochondrial
        if(true_data_size > 0) {
            # reference allele frequence
            local$af <- sum(snp_data[non_missing_data]) / true_data_size
        }
    } else if(snp_type == "X") {
        ## X-chromosome
        check_X <- (missing_sex & non_missing_data) | 
            (male_ind & (snp_data == 2))
        if(any(check_X)) {
            local$valid <- FALSE
            local$issue_X <- str_c(which(check_X), collapse = ",")
        }
        
        specific_ind <- non_missing_data & !missing_sex
        specific_data_size <- sum(specific_ind)
        
        if(specific_data_size > 0) {
            weighted_data_size <- 2 * sum(non_missing_data & female_ind) + 
                sum(non_missing_data & male_ind)
            # reference allele frequence
            local$af <- sum(snp_data[specific_ind]) / weighted_data_size
        }
    } else if(snp_type == "Y") {
        ## Y-chromosome
        check_Y <- (missing_sex & non_missing_data) |
            (male_ind & (snp_data == 2)) | (female_ind & (snp_data != 9))
        if(any(check_Y)) {
            local$valid <- FALSE
            local$issue_Y <- str_c(which(check_Y), collapse = ",")
        }
        
        specific_ind <- non_missing_data & male_ind
        specific_data_size <- sum(specific_ind)
        
        if(specific_data_size > 0) {
            # reference allele frequence
            local$af <- sum(snp_data[specific_ind]) / specific_data_size
        }
    }
    
    # filtering
    if(maf == "hudson") {
        maf <- 0
        local$hudson <- TRUE
    }
    # MAF filter
    local$filter <- (local$af < maf) || (1 - local$af < maf)
    # mono
    local$mono <- (local$af == 0) || (1 - local$af == 0)
    
    if(local$hudson) {
        local$filter <- local$mono
    }
    
    # filter
    return(data.frame(
        valid = local$valid, filter = local$filter, mono = local$mono,
        missing_pop = local$missing_pop, 
        issue_X = local$issue_X, issue_Y = local$issue_Y, af = local$af,
        maf = maf, hudson = local$hudson,
        stringsAsFactors = FALSE
    ))
}

#' Check (for missing values) and filter (based on MAF) IndSeq SNP data
#' @keywords internal
#' @author Ghislain Durif
#' @param content data.frame with columns corresponding to SNPs 
#' where each entry encode the number of ancestral allele for the loci, 
#' i.e. `0`, `1` and `2` for autosome (`A`) and X-chromosome (`X`) in female, 
#' or `0` and `1` for  haploid locus (`H`), Y-chromosome (`Y`) in male and 
#' mitochondrial locus (`M`). 
#' Note : missing values are encoded by a `9`.
#' @param indiv_info data.frame with columns `IND` (individual id), 
#' `SEX` (female or male), `POP` (population id).
#' Note : missing values are encoded by a `9`.
#' @param snp_type vector of locus type (among `A`, `H`, `X`, `Y`, `M`).
#' @param locus_cout data.frame with two columns, `count` being the number 
#' of locus for each type in the data, and `type` being the corresponding locus 
#' type (among `A`, `H`, `X`, `Y`, `M`).
#' @importFrom dplyr bind_rows
check_snp_indseq <- function(content, indiv_info, snp_type, locus_count, 
                             maf=0.05) {
    
    # init output
    out <- list(
        valid = TRUE, locus_count = NULL, msg = list()
    )
    
    ncore <- getOption("diyabcGUI")$ncore
    
    snp_list <- pblapply(
        1:nrow(content),
        function(ind) {
            out <- process_indseq_locus(
                snp_data = content[ind,], sex_data = indiv_info$SEX,
                pop_data = indiv_info$POP, snp_type = snp_type[ind],
                maf = maf
            )
        },
        cl = ncore
    )
    
    seek_error <- unlist(lapply(
        snp_list, function(item) "try-error" %in% class(item)
    ))
    if(any(seek_error)) {
        error_msg <- attributes(
            snp_list[[ which(seek_error)[1] ]]
        )$condition$message
        err <- str_c(
            "Issue when checking IndSeq SNP data file content:",
            error_msg, sep = " "
        )
        out$valid <- FALSE
        msg <- tagList("Error when checking data file content.")
        out$msg <- append(out$msg, list(msg))
        pprint(err)
        return(out)
    }
    
    # no error
    snp_tab <- Reduce("bind_rows", snp_list)
    rm("snp_list")
    
    # check for unvalid locus
    if(any(!snp_tab$valid)) {
        # missing pop
        is_missing_pop <- !is.na(snp_tab$missing_pop)
        if(any(is_missing_pop)) {
            out$valid <- FALSE
            missing_pop <- snp_tab$missing_pop[is_missing_pop]
            snp_issue <- which(is_missing_pop)
            msg <- tagList(
                "Issue with missing data for entire population(s)",
                "regarding SNP:",
                tags$div(
                    do.call(
                        tags$ul, 
                        lapply(
                            1:length(snp_issue),
                            function(ind) {
                                tags$li(
                                    tags$b(snp_issue[ind]),
                                    "(for population(s)", 
                                    tags$code(missing_pop[ind]), ")"
                                )
                            }
                        )
                    ),
                    style = "column-count:2;"
                )
            )
            out$msg <- append(out$msg, list(msg))
        }
        
        # X chromosome
        is_issue_X <- !is.na(snp_tab$issue_X)
        if(any(is_issue_X)) {
            out$valid <- FALSE
            issue_X <- snp_tab$issue_X[is_issue_X]
            snp_issue <- which(is_issue_X)
            msg <- tagList(
                "Issue with data for SNP on X chromosome (see manual)",
                "regarding SNP:",
                tags$div(
                    do.call(
                        tags$ul, 
                        lapply(
                            1:length(snp_issue),
                            function(ind) {
                                tags$li(
                                    tags$b(snp_issue[ind]),
                                    "(for individuals", 
                                    tags$code(issue_X[ind]), ")"
                                )
                            }
                        )
                    )
                )
            )
            out$msg <- append(out$msg, list(msg))
        }
        # Y chromosome
        is_issue_Y <- !is.na(snp_tab$issue_Y)
        if(any(is_issue_Y)) {
            out$valid <- FALSE
            issue_Y <- snp_tab$issue_Y[is_issue_Y]
            snp_issue <- which(is_issue_Y)
            msg <- tagList(
                "Issue with data for SNP on Y chromosome (see manual)",
                "regarding SNP:",
                tags$div(
                    do.call(
                        tags$ul, 
                        lapply(
                            1:length(snp_issue),
                            function(ind) {
                                tags$li(
                                    tags$b(snp_issue[ind]),
                                    "(for individuals", 
                                    tags$code(issue_Y[ind]), ")"
                                )
                            }
                        )
                    )
                )
            )
            out$msg <- append(out$msg, list(msg))
        }
    }
    
    # continue ?
    if(!out$valid) {
        return(out)
    }
    
    # extract number of filtered loci by type
    tmp_filter <- tapply(
        snp_tab$filter, snp_type, sum
    )
    tmp_filter <- data.frame(
        filt=tmp_filter, type=names(tmp_filter), 
        row.names=NULL, stringsAsFactors = FALSE
    )
    
    # extract number of monomorphic loci by type
    tmp_mono <- tapply(
        snp_tab$mono, snp_type, sum
    )
    tmp_mono <- data.frame(
        mono=tmp_mono, type=names(tmp_mono), 
        row.names=NULL, stringsAsFactors = FALSE
    )
    
    # merge all results into locos_details table
    out$locus_count <- merge(locus_count, tmp_filter)
    out$locus_count <- merge(out$locus_count, tmp_mono)
    
    # output
    return(out)
}