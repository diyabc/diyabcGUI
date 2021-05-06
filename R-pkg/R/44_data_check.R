#' Check data file
#' @keywords internal
#' @author Ghislain Durif
#' @param data_file string, data file name.
#' @param data_dir string, path to directory where data file is stored.
#' @param locus_type string, locus type `"mss"` or `"snp"`.
#' @param seq_mode string, `"indseq"` or `"poolseq"`.
check_data_file <- function(data_file, data_dir, locus_type, seq_mode) {
    # output
    out <- NULL
    # ## debugging
    # logging("data_file = ", data_file)
    ## mss locus
    if(locus_type == "mss") {
        out <- read_mss_data(data_file, data_dir)
    ## snp locus / indseq
    } else if((locus_type == "snp") & (seq_mode == "indseq")) {
        out <- read_indseq_snp_data(data_file, data_dir)
    ## snp locus / poolseq
    } else if((locus_type == "snp") & (seq_mode == "poolseq")) {
        out <- read_poolseq_snp_data(data_file, data_dir)
    } else {
        stop("Issue with input arguments")
    }
    
    ## output
    return(out)
}