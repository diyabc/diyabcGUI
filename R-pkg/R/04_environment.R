#' Initialize diyabcGUI package global environment
#' @keywords internal
#' @author Ghislain Durif
#' @description
#' Initialize container for each type of project 
#' (diyabc-rf analysis and data generation) inside `diyabcGUI` global 
#' environment.
init_diyabc_env <- function() {
    assign("ap", NULL, env) # diyabc-rf analysis project
    assign("dp", NULL, env) # data generation project
}

#' Initialize environment for DIYABC-RF pipeline
#' @keywords internal
#' @author Ghislain Durif
init_diyabcrf_env <- function() {
    
    # clean environment
    tmp <- reactiveValues(
        ## project setup
        proj_name = NULL,            # project name
        proj_dir = NULL,             # project directory
        locus_type = NULL,      # "SNP" or "MSS"
        seq_mode = NULL,        # "IndSeq" or "PoolSeq"
        ## observed data
        data_file = NULL,       # observed data file name
        metadata = reactiveValues(
            # number of loci in the data file
            n_loci = NULL,
            # table of locus description: name, type, number
            locus_des = NULL    
        ),
        ## training set simulation
        ts = reactiveValues(
            # list of historical models
            hist_model = NULL,
            # list number of parameters per model
            n_param = NULL,
            # list of model priors (discrete probabilities)
            model_prior = NULL,
            # table of historical model parameters (name, type, priors)
            param = NULL,
            # list of conditions on historical parameters
            cond = NULL,
            # table of loci description
            loci_des = NULL,
            # number of loci group
            n_group = NULL,
            # list of group priors for MSS data
            mss_prior = NULL,
            # specific ref table column names for MSS data
            mss_reftab_colname = NULL
        ),
        ## random forest analysis
        rf = reactiveValues(
            # analysis mode: "param_estim" or "model_choice"
            mode = NULL,
            # number of samples
            n_ref = NULL,
            # minimal node size
            min_node_size = NULL, 
            # number of tree
            n_tree = NULL, 
            # number of noise columns
            noise_columns = NULL, 
            # boolean: if TRUE, disable LDA for model choice or PLS for 
            #   parameter estimation
            no_linear = NULL, 
            # percentage of maximum explained Y-variance for retaining pls axis
            pls_max_var = NULL, 
            # Chosen scenario (mandatory for parameter estimation)
            chosen_scenario = NULL, 
            # number of oob testing samples (mandatory for parameter estimation)
            noob = NULL, 
            # name of the parameter of interest (mandatory for parameter 
            #   estimation)
            parameter = NULL, 
            # subset and/or groups of models
            groups = NULL
        )
    )
    
    # init project directory
    tmp$proj_dir <- mk_proj_dir("diyabc_rf")
    
    # init env
    assign("ap", tmp, env)
}

#' Initialize environment for data generation pipeline
#' @keywords internal
#' @author Ghislain Durif
init_datagen_env <- function() {
    
    # clean environment
    tmp <- reactiveValues(
        ## project setup
        proj_name = NULL,            # project name
        proj_dir = NULL,             # project directory
        locus_type = NULL,      # "SNP" or "MSS"
        seq_mode = NULL,        # "IndSeq" or "PoolSeq"
        ## data description
        model = NULL,            # historical model
        param = NULL,           # list of parameter values
        loci_des = NULL,        # table of loci description
        n_group = NULL,         # number of loci group
        mss_prior = NULL,       # list of group priors for MSS data
        sample_sizes = NULL,    # table of sample sizes
        n_rep = NULL,           # number of replicates
        sex_ratio = NULL        # sex ratio in the simulation
    )
    
    # init project directory
    tmp$proj_dir <- mk_proj_dir("diyabc_datagen")
    
    # init env
    assign("dp", tmp, env)
}

#' Print content of diyabc-rf project sub-environment for debugging purpose
#' @keywords internal
#' @author Ghislain Durif
debug_ap <- function() {
    pprint(reactiveValuesToList(env$ap))
}

#' Print content of metadata inside diyabc-rf project sub-environment 
#' for debugging purpose
#' @keywords internal
#' @author Ghislain Durif
debug_ap_metadata <- function() {
    pprint(reactiveValuesToList(env$ap$metadata))
}

#' Print content of training set simulation setup inside diyabc-rf project 
#' sub-environment for debugging purpose
#' @keywords internal
#' @author Ghislain Durif
debug_ap_ts <- function() {
    pprint(reactiveValuesToList(env$ap$ts))
}

#' Print content of random forest setup inside diyabc-rf project 
#' sub-environment for debugging purpose
#' @keywords internal
#' @author Ghislain Durif
debug_ap_rf <- function() {
    pprint(reactiveValuesToList(env$ap$rf))
}

#' Print content of data generation project sub-environment for debugging 
#' purpose
#' @keywords internal
#' @author Ghislain Durif
debug_dp <- function() {
    pprint(reactiveValuesToList(env$dp))
}


#' Get value from named list in diyabc local environment
#' @keywords internal
#' @author Ghislain Durif
#' @param var1 named list in local environment.
#' @param var2 element of named list to get the value from.
#' @param env local environment where to get the value. By default it is 
#' diyabc local environment.
#' @return the value of element `var2` in list `var1`
getter <- function(var1, var2, env = diyabc_env) {
    return(env[[ as.character(substitute(var1)) ]][[ as.character(substitute(var2)) ]])
}

#' Set value in named list in diyabc local environment
#' @keywords internal
#' @author Ghislain Durif
#' @param value value to be set.
#' @param var1 named list in local environment.
#' @param var2 element of named list where the value will be stored.
#' @param env local environment where to get the value. By default it is 
#' diyabc local environment.
setter <- function(val, var1, var2, env = diyabc_env) {
    env[[ as.character(substitute(var1)) ]][[ as.character(substitute(var2)) ]] <<- val
}
