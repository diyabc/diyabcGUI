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
        data_file = NULL, n_loci = NULL, locus_count = NULL, 
        n_pop = NULL, n_indiv = NULL,
        sex_ratio = NULL, maf = NULL
    )
    
    # full path
    file_name <- file.path(data_dir, data_file)
    
    # check file_name
    tmp <- check_file_name(file_name)
    if(!tmp) {
        out$valid <- FALSE
        msg <- tagList("Invalid data file name")
        out$msg <- append(out$msg, list(msg))
    }
    
    # check file content
    if(file.info(file_name)$size == 0) {
        out$valid <- FALSE
        msg <- tagList("Data file is empty")
        out$msg <- append(out$msg, list(msg))
    }
    
    # check file_type
    if(tools::file_ext(file_name) != "snp") {
        out$valid <- FALSE
        msg <- tagList(
            "IndSeq SNP files should have an extension", tags$code(".snp")
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
            "Missing", tags$b("sex ratio"), "in first line header"
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
            "(MAF) in first line header.",
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
            "impossible to read it, see manual"
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
            "Formatting issue with header second line.",
            "Required column titles are", tags$code("IND"), 
            tags$code("SEX"), tags$code("POP"), 
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
    locus_count <- Reduce("rbind", lapply(
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
            "impossible to read the file, see manual"
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
            "file header and file content"
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
            tags$code("9"), "for missing values (see manual)"
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
            tags$code("IND"), tags$code("SEX"), "or", tags$code("POP")
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
            "in one (or more) of the columns encoding the SNPs"
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # check data type
    if(!is.integer(content)) {
        out$valid <- FALSE
        msg <- tagList(
            "SNP encoding should be only contain integer values"
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
            "or", tags$code("9"), "for missing data (see manual)"
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    ## LOCUS FILTERING
    # run
    snp_check <- check_snp_indseq(
        content, indiv_info, snp_type, locus_count, out$maf
    )
    # check
    if(!snp_check$valid) {
        out$valid <- FALSE
        out$msg <- append(out$msg, snp_check$msg)
        return(out)
    }
    # results
    out$locus_count <- snp_check$locus_count

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
        valid = TRUE, filter = FALSE, mono = FALSE,
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
#' @param locus_count data.frame with two columns, `count` being the number 
#' of locus for each type in the data, and `type` being the corresponding locus 
#' type (among `A`, `H`, `X`, `Y`, `M`).
#' @importFrom dplyr bind_rows
check_snp_indseq <- function(content, indiv_info, snp_type, locus_count, 
                             maf=0.05) {
    
    # init output
    out <- list(
        valid = TRUE, locus_count = NULL, msg = list()
    )
    
    # process SNPs
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
        msg <- tagList("Error when checking data file content")
        out$msg <- append(out$msg, list(msg))
        pprint(err)
        return(out)
    }
    
    # no error
    snp_tab <- Reduce("bind_rows", snp_list)
    rm("snp_list")
    
    # check for invalid locus
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
                ),
                "Remove this locus (these loci) from your dataset"
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
                ),
                "Remove this locus (these loci) from your dataset"
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
        filter=tmp_filter, type=names(tmp_filter), 
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
    
    # merge all results into locus_count table
    out$locus_count <- merge(locus_count, tmp_filter)
    out$locus_count <- merge(out$locus_count, tmp_mono)
    
    out$locus_count$available <- out$locus_count$count - 
        out$locus_count$filter
    
    # output
    return(out)
}

#' Read and parse PoolSeq SNP data file
#' @keywords internal
#' @author Ghislain Durif
#' @param data_file string, data file name.
#' @param data_dir string, path to directory where data file is stored.
#' @importFrom tools file_ext
read_poolseq_snp_data <- function(data_file, data_dir) {
    
    # init output
    out <- list(
        msg = list(), valid = TRUE,
        data_file = NULL, n_loci = NULL, locus_count = NULL, 
        n_pop = NULL, sex_ratio = NULL, mrc = NULL
    )
    
    # full path
    file_name <- file.path(data_dir, data_file)
    
    # check file_name
    tmp <- check_file_name(file_name)
    if(!tmp) {
        out$valid <- FALSE
        msg <- tagList("Invalid data file name")
        out$msg <- append(out$msg, list(msg))
    }
    
    # check file content
    if(file.info(file_name)$size == 0) {
        out$valid <- FALSE
        msg <- tagList("Data file is empty")
        out$msg <- append(out$msg, list(msg))
    }
    
    # check file_type
    if(tools::file_ext(file_name) != "snp") {
        out$valid <- FALSE
        msg <- tagList(
            "IndSeq SNP files should have an extension", tags$code(".snp")
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
            "Missing", tags$b("sex ratio"), "in first line header"
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    out$sex_ratio <- str_extract(header1, pttrn)
    
    # MRC
    pttrn <- "(?i)(?<=MRC=)[0-9]+(?-i)"
    if(!str_detect(header1, pttrn)) {
        out$valid <- FALSE
        msg <- tagList(
            "Missing", tags$b("Minimum Read Count"), 
            "(MRC) in first line header.",
            "MRC should be a positive or null integer,", 
            "see manual."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    out$mrc <- as.integer(str_extract(header1, pttrn))
    
    ## HEADER 2
    header2 <- tryCatch(
        unname(unlist(read.table(file = file_name, skip = 1, nrows = 1))), 
        error = function(e) return(e)
    )
    if("error" %in% class(header2)) {
        out$valid <- FALSE
        msg <- tagList(
            "Formatting issue with second line header, ",
            "impossible to read it, see manual"
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # upper case
    header2 <- str_to_upper(header2)
    
    # header 2 content
    if(header2[1] != "POOL" &
       header2[2] != "POP_NAME:HAPLOID_SAMPLE_SIZE" & 
       !all(str_detect(header2[-(1:2)], "POP[0-9]+:[0-9]+"))) {
        out$valid <- FALSE
        msg <- tagList(
            "Formatting issue with header second line.",
            "Required column titles are", tags$code("POOL"), 
            tags$code("POP_NAME:HAPLOID_SAMPLE_SIZE"),
            "followed by a character string", 
            tags$code("POP<pop_id>:<sample_size>"),
            "for each population in the file (see manual)."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    ## number of population
    out$n_pop <- length(header2) - 2
    
    ## DATA FILE CONTENT
    content <- tryCatch(
        read.table(file_name, skip = 2), error = function(e) return(e)
    )
    if("error" %in% class(content)) {
        out$valid <- FALSE
        msg <- tagList(
            "Formatting issue with data, ",
            "impossible to read the file, see manual"
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # check number of population
    if(ncol(content) %% 2 != 0) {
        out$valid <- FALSE
        msg <- tagList(
            "Formatting issue with data:",
            "number of column should be even,",
            "providing pair of counts for reference",
            "and alternate alleles at each locus",
            "in each popultation, see manual."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    } 
    if(ncol(content) != 2*out$n_pop) {
        out$valid <- FALSE
        msg <- tagList(
            "Formatting issue with data:",
            "number of population indicated in file second-line header",
            "does not correspond to number of columns in file content,",
            "see manual."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # nb of locus
    out$n_loci <- nrow(content)
    
    # check data encoding
    check_snp_encoding <- apply(content, 1, is.integer)
    if(!all(unlist(check_snp_encoding))) {
        out$valid <- FALSE
        msg <- tagList(
            "Issue with data encoding at lines",
            tags$code(str_c(which(check_snp_encoding, collapse = ", "))), ".",
            "Expecting read counts, i.e. positive or null integer",
            "(see manual)."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    if(any(is.na(content))) {
        out$valid <- FALSE
        msg <- tagList(
            "Missing values (i.e.", tags$code("NA"), ")",
            "are not allowed (see manual)"
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    ## FILTERING LOCUS
    snp_check <- check_snp_poolseq(content, mrc = out$mrc)
    out$locus_count <- snp_check$locus_count
    
    ## output
    return(out)
}

#' Check (for missing values) and filter (based on MRC) IndSeq SNP data
#' @keywords internal
#' @author Ghislain Durif
#' @param content data.frame with columns corresponding to couples of 
#' PoolSeq encoding.
check_snp_poolseq <- function(content, mrc = 1) {
    
    # init output
    out <- list(locus_count = NULL)
    
    # number of pop
    n_pop <- ncol(content) / 2
    
    # count per allele
    allele1_count <- apply(
        content[,rep(c(TRUE,FALSE), n_pop)], 1, sum
    )
    allele2_count <- apply(
        content[,rep(c(FALSE,TRUE), n_pop)], 1, sum
    )
    
    # extract number of filtered loci by type
    tmp_filter <- (allele1_count < mrc) | (allele2_count < mrc)
    
    # extract number of monomorphic loci by type
    tmp_mono <- (allele1_count < 1) | (allele2_count < 1)
    
    # merge all results into locus_count table
    out$locus_count <- data.frame(
        type = "A",
        count = nrow(content),
        filter = sum(tmp_filter),
        mono = sum(tmp_mono),
        available = nrow(content) - sum(tmp_filter),
        stringsAsFactors = FALSE
    )
    
    # output
    return(out)
}

#' Check Microsat/Sequence (GenePop) data file
#' @keywords internal
#' @author Ghislain Durif
#' @param data_file string, data file name.
#' @param data_dir string, path to directory where data file is stored.
#' @param expected_data_file string, expected data file name for 
#' existing project, default is NULL.
#' @importFrom tools file_ext
read_mss_data <- function(data_file, data_dir) {
    
    # init output
    out <- list(
        msg = list(), valid = TRUE,
        data_file = NULL, n_loci = NULL, locus_count = NULL, 
        n_pop = NULL, n_indiv = NULL, pop_size = NULL,
        sex_ratio = NULL, 
        locus_type = NULL, locus_name = NULL, locus_mode = NULL,
        seq_length = NULL
    )
    
    # ## init output
    # locus <- NULL
    # locus_mode <- NULL
    # locus_name <- NULL
    # seq_length <- NULL
    # n_loci <- NULL
    # n_pop <- NULL
    # n_indiv <- NULL
    # pop_size <- NULL
    
    # full path
    file_name <- file.path(data_dir, data_file)
    
    # check file_name
    tmp <- check_file_name(file_name)
    if(!tmp) {
        out$valid <- FALSE
        msg <- tagList("Invalid data file name")
        out$msg <- append(out$msg, list(msg))
    }
    
    # check file content
    if(file.info(file_name)$size == 0) {
        out$valid <- FALSE
        msg <- tagList("Data file is empty")
        out$msg <- append(out$msg, list(msg))
    }
    
    # check file_type
    if(tools::file_ext(file_name) != "mss") {
        out$valid <- FALSE
        msg <- tagList(
            "IndSeq SNP files should have an extension", tags$code(".mss")
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
            "Missing", tags$b("sex ratio"), "in first line header"
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    out$sex_ratio <- str_extract(header1, pttrn)
    
    ## FILE CONTENT
    file_content <- readLines(file_name, warn = FALSE)
    
    ## LOCUS DESCRIPTION (between second line and first 'POP/pop' keyword)
    
    ## population keyword
    pttrn <- "^(?i)pop(?-i)$"
    pop_match_ind <- which(str_detect(file_content, pttrn))
    # check for missing pop keyword
    if(length(pop_match_ind) == 0) {
        out$valid <- FALSE
        msg <- tagList(
            "Keyword", tags$code("POP"), "is missing, see manual"
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    # check for locus description
    if(pop_match_ind[1] < 3) {
        out$valid <- FALSE
        msg <- tagList(
            "Missing locus description, see manual.",
            "Locus description should be located",
            "at beginning of data file, after header line,", 
            "with a single locus per line."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    ## extract locus description
    locus_desc_ind <- 2:(pop_match_ind[1] - 1)
    file_locus_desc <- file_content[locus_desc_ind]
    locus_desc_ind <- locus_desc_ind - 1 # for comparison below
    
    # check locus description format
    pttrn <- "^[A-Za-z0-9\\s_\\-]+( <[AHXYM]>)?$"
    locus_desc_match <- str_detect(file_locus_desc, pttrn)
    locus_desc_match_ind <- which(locus_desc_match)
    if(any(!locus_desc_match)) {
        out$valid <- FALSE
        issue_line <- 
            locus_desc_ind[!locus_desc_ind %in% locus_desc_match_ind] + 1
        msg <- tagList(
            "Issue with Microsat/Sequence locus description format at rows:",
            tags$code(str_c(issue_line, collapse = ",")), ".", br(),
            "You should only use the following characters to specify",
            "locus names:",
            tags$code("A-Z"), ",", tags$code("a-z"), ",", 
            tags$code("0-9"), ",", tags$code("_"), ",", tags$code("-"), 
            "and space.",
            "And you should specify the locus type at the end",
            "of each locus description line with",
            tags$code("<A>"), ",", tags$code("<H>"), ",", 
            tags$code("<X>"), ",", tags$code("<Y>"), ",", 
            tags$code("<M>"), ",", 
            "respectively for autosomal, haploid, X-linked, Y-linked",
            "or mitochondrial locus.",
            "If locus type is missing, then", tags$code("<A>"), "type",
            "is assumed for the corresponding locus."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    ## locus description
    pttrn <- "(?<=<)[AHXYM](?=>$)"
    locus_pttrn_match <- str_extract(file_locus_desc, pttrn)
    # missing locus type corresponds to default A
    locus_pttrn_match[is.na(locus_pttrn_match)] <- "A"
    
    ## number of locus
    out$n_loci <- length(locus_desc_match_ind)
    
    ## locus description output
    locus_type <- locus_pttrn_match
    
    ## locus name
    pttrn <- "^[A-Za-z0-9\\s_\\-]+(?=( <[AHXYM]>)?$)"
    locus_name <- str_extract(file_locus_desc, pttrn)
    locus_name <- str_replace_all(locus_name, " +", "_")
    # check
    if(length(unique(locus_name)) != length(locus_name)) {
        out$valid <- FALSE
        msg <- tagList(
            "Issue with locus description",
            "each locus should have a unique name, see manual"
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    ## remove unnecessary space
    file_content <- str_replace_all(
        str_trim(file_content), " +", " "
    )
    
    ## remove final empty line (if any)
    if(tail(file_content, 1) == "") {
        file_content <- head(file_content, length(file_content) - 1)
    }
    
    ## number of pop
    out$n_pop <- length(pop_match_ind)
    
    ## pop size
    out$pop_size <- diff(c(pop_match_ind, length(file_content))) - 
        c(rep(1, out$n_pop - 1), 0)
    
    ## number of individuals
    out$n_indiv <- sum(out$pop_size)
    
    ## population id
    pop_id <- unlist(lapply(
        1:out$n_pop, 
        function(ind) return(rep(ind, each = out$pop_size[ind]))
    ))
    
    ## EXTRACT DATA
    data_ind <- head(pop_match_ind, 1):length(file_content)
    data_ind <- data_ind[!data_ind %in% pop_match_ind]
    data_content <- file_content[data_ind]
    
    # write data content to a temporary file
    data_content <- str_replace_all(
        str_replace_all(data_content, ",", " "),
        " +",  ";"
    )
    tmp_file <- file.path(data_dir, "tmp_data_file.csv")
    tmp <- writeLines(data_content, tmp_file)
    on.exit({
        if(file.exists(tmp_file)) fs::file_delete(tmp_file)
    })
    
    # read data as data.frame
    data_content <- tryCatch(
        read.table(tmp_file, sep = ";", stringsAsFactors = FALSE,
                   colClasses = "character"), 
        error = function(e) e
    )
    if("error" %in% class(data_content)) {
        out$valid <- FALSE
        msg <- tagList(
            "Issue with Microsat/Sequences data file format, see manual"
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    ##### CHECK DATA
    ## check number of loci
    if(ncol(data_content) != out$n_loci + 1) {
        out$valid <- FALSE
        msg <- tagList(
            "Number of declared loci at beginning of file", 
            "does not correspond to number of actual loci", 
            "in the data, see manual."
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    ## format data.frame
    colnames(data_content) <- c("indiv", str_c("locus", 1:out$n_loci))
    data_content$pop <- as.character(pop_id)
    
    ## indiv name first column
    if(!all(str_detect(data_content$indiv, "[A-Za-z0-9_-]+"))) {
        out$valid <- FALSE
        msg <- tagList(
            "First column should provide individual names,",
            "you can use the following character to specify",
            "such names:",
            tags$code("A-Z"), tags$code("a-z"), tags$code("0-9"),
            tags$code("_"), tags$code("-"), "and", tags$code(" ")
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    ## check locus encoding
    microsat_hap_encoding <- "^[0-9]{3}$"
    microsat_dip_encoding <- "^[0-9]{6}$"
    microsat_x_encoding <- "^([0-9]{3}|[0-9]{6})$"
    nucleotid_encoding <- "[ATCGN\\-]*" # N and - for undetermined nucleotid
    seq_hap_encoding <- str_c("^<\\[", nucleotid_encoding, "\\]>")
    seq_dip_encoding <- str_c(
        "^<\\[", nucleotid_encoding, "\\]\\[", nucleotid_encoding, "\\]>"
    )
    seq_x_encoding <- str_c(
        "^<\\[", nucleotid_encoding, "\\](\\[", nucleotid_encoding, "\\])?>"
    )
    
    ## locus data
    locus_data <- data_content[,!colnames(data_content) %in% c("indiv", "pop")]
    # issue when a single locus
    if(ncol(data_content) == 3) {
        locus_data <- data.frame(locus1 = locus_data)
    }
    
    ### microsat locus
    microsat_hap_locus <- which(
        apply(
            locus_data, 2, function(loc) {
                return(all(str_detect(loc, microsat_hap_encoding)))
            }
        ) & (locus_type %in% c("H", "Y", "M"))
    )
    microsat_dip_locus <- which(
        apply(
            locus_data, 2, function(loc) {
                return(all(str_detect(loc, microsat_dip_encoding)))
            }
        ) & (locus_type == "A")
    )
    microsat_x_locus <- which(
        apply(
            locus_data, 2, function(loc) {
                return(all(str_detect(loc, microsat_x_encoding)))
            }
        ) & (locus_type == "X")
    )
    
    ### seq locus
    seq_hap_locus <- which(
        apply(
            locus_data, 2, function(loc) {
                return(all(str_detect(loc, seq_hap_encoding)))
            }
        ) & (locus_type %in% c("H", "Y", "M"))
    )
    seq_dip_locus <- which(
        apply(
            locus_data, 2, function(loc) {
                return(all(str_detect(loc, seq_dip_encoding)))
            }
        ) & (locus_type == "A")
    )
    seq_x_locus <- which(
        apply(
            locus_data, 2, function(loc) {
                return(all(str_detect(loc, seq_x_encoding)))
            }
        ) & (locus_type == "X")
    )
    
    ## check that A are diploid locus
    if(!all(which(locus_type == "A") %in% 
            c(microsat_dip_locus, seq_dip_locus))) {
        out$valid <- FALSE
        msg <- tagList(
            "Autosomal diploid (i.e. identified by a", tags$code("A"), ")",
            "should all be diploid loci"
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    ## check that H, M and Y are haploid locus
    if(!all(which(locus_type %in% c("H", "M", "Y")) %in% 
            c(microsat_hap_locus, seq_hap_locus))) {
        out$valid <- FALSE
        msg <- tagList(
            "Autosomal haploid loci",
            "(i.e. identified by a", tags$code("H"), ")", 
            "Y-linked loci (i.e. identified by a", tags$code("Y"), ")",
            "and Mitochondrial loci", 
            "(i.e. identified by a", tags$code("M"), ")",
            "should all be haploid loci"
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    ### check if other issue with formating
    if(!all(sort(c(microsat_hap_locus, microsat_dip_locus,
                   microsat_x_locus,
                   seq_hap_locus, seq_dip_locus,
                   seq_x_locus)) == 
            1:out$n_loci)) {
        out$valid <- FALSE
        msg <- tagList(
            "Issue with data content of columns:",
            tags$code(str_c(
                (1:n_loci)[!(1:n_loci) %in% 
                               c(microsat_hap_locus, 
                                 microsat_dip_locus,
                                 microsat_x_locus,
                                 seq_hap_locus, 
                                 seq_dip_locus,
                                 seq_x_locus)],
                collapse = ", "
            )), br(),
            "See manual"
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    ## locus mode (microsat or sequence, hap or dip)
    tmp_locus_mode <- data.frame(
        mode = c(
            rep("M", length(microsat_hap_locus) + length(microsat_dip_locus) +
                length(microsat_x_locus)), 
            rep("S", length(seq_hap_locus) + length(seq_dip_locus) +
                    length(seq_x_locus))
        ),
        index = c(microsat_hap_locus, microsat_dip_locus,
                  microsat_x_locus,
                  seq_hap_locus, seq_dip_locus,
                  seq_x_locus)
    )
    locus_mode <- tmp_locus_mode$mode[order(tmp_locus_mode$index)]
    
    ## check seq locus length
    seq_length <- unlist(lapply(
        which(locus_mode == "S"),
        function(col_ind) {
            tmp <- unlist(lapply(
                1:out$n_indiv,
                function(row_ind) {
                    return(
                        str_length(str_extract_all(
                            locus_data[row_ind,col_ind],
                            "\\[[ATCGN\\-]*\\]", 
                            # N or - for undetermined nucleotid
                            simplify = TRUE
                        )) - 2
                    )
                }
            ))
            
            out <- max(tmp)
            if(!all(unique(tmp) %in% c(0, out))) {
                return(-100)
            } else {
                return(out)
            }
        }
    ))
    
    if(any(seq_length == -100)) {
        out$valid <- FALSE
        msg <- tagList(
            "Non-missing sequence data attached to following loci",
            tags$code(str_c(which(seq_length == -100), collapse = ", ")),
            "do not have the same length in all individuals"
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    ## check for missing data in microsat locus
    microsat_missing_encoding <- "^(0{3}|0{6})$"
    missing_microsat <- apply(
        as.matrix(locus_data[,which(locus_mode == "M")]), c(1,2),
        function(item) return(str_detect(item, microsat_missing_encoding))
    )
    
    missing_pop <- apply(
        missing_microsat, 2,
        function(item) {
            return(any(unlist(tapply(item, pop_id, sum)) == out$pop_size))
        }
    )
    
    if(any(missing_pop)) {
        out$valid <- FALSE
        msg <- tagList(
            "Issue with missing data for entire population(s)",
            "regarding microsat locus:",
            tags$code(str_c(which(missing_pop), collapse = ", ")), br(),
            "Remove this locus (these loci) from your dataset"
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    ## check for missing data in seq locus
    seq_missing_encoding <- "^<\\[\\](\\[\\])?>$"
    missing_seq <- apply(
        as.matrix(locus_data[,which(locus_mode == "S")]), c(1,2),
        function(item) return(str_detect(item, seq_missing_encoding))
    )
    
    missing_pop <- apply(
        missing_seq, 2,
        function(item) {
            return(any(unlist(tapply(item, pop_id, sum)) == out$pop_size))
        }
    )
    
    if(any(missing_pop)) {
        out$valid <- FALSE
        msg <- tagList(
            "Issue with missing data for entire population(s)",
            "regarding sequence locus:",
            tags$code(str_c(which(missing_pop), collapse = ", ")), br(),
            "Remove this locus (these loci) from your dataset"
        )
        out$msg <- append(out$msg, list(msg))
        return(out)
    }
    
    ## locus count
    out$locus_count <- as.data.frame(table(locus_type, locus_mode))
    colnames(out$locus_count) <- c("type", "mode", "count")
    
    ## save locus name, description and mode
    out$locus_name <- locus_name
    out$locus_type <- locus_type
    out$locus_mode <- locus_mode
    
    out$seq_length <- rep(NA, out$n_loci)
    out$seq_length[locus_mode == "S"] <- seq_length
    
    ## output
    return(out)
}
