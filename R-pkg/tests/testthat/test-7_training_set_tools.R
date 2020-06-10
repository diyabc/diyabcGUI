context("training_set_tools")

test("write_header", {
    
    proj_dir = mk_proj_dir()
    on.exit(tryCatch(fs::dir_detete(proj_dir)))
    param_list = list(
        "N1 N UN[100,10000,0.0,0.0]",
        "N2 N UN[100,10000,0.0,0.0]",
        "N3 N UN[100,10000,0.0,0.0]",
        "N4 N UN[100,10000,0.0,0.0]",
        "ra A UN[0.05,0.95,0.0,0.0]",
        "t32 T UN[10,1000,0.0,0.0]",
        "t21 T UN[10,1000,0.0,0.0]",
        "t431 T UN[10,1000,0.0,0.0]"
    )
    param_count_list = list(7)
    scenario_list = as.list(str_c(
        "N1 N2 N3 N4",
        "0 sample 1",
        "0 sample 2",
        "0 sample 3",
        "0 sample 4",
        "t431 split 4 1 3 ra",
        "t32 merge 2 3",
        "t21 merge 1 2",
        sep = "\n"
    ))
    cond_list = list("t21>t32", "t431<t32")
    data_file = "indseq_SNP_sim_dataset_4POP_001.snp"
    locus_type = "snp"
    seq_mode = "indseq"
    locus = "5000 <A> G1 from 1" 
    
    write_header(proj_dir, data_file, 
                 scenario_list, param_count_list, 
                 param_list, cond_list, 
                 locus_type, seq_mode, locus)
    
    file_name <- file.path(proj_dir,
                           "header.txt")
    file_type = "text/plain"
    data_type = "snp"
    expect_equal(
        parse_diyabc_header(file_name, file_type, data_type),
        list(data_file="indseq_SNP_sim_dataset_4POP_001.snp", 
             loci_description="5000 <A> G1 from 1", 
             n_loci_des=1, n_param=8, n_sumstat=2, 
             raw_cond_list=c("t21>t32", "t431<t32"), 
             raw_prior_list=c("N1 N UN[100,10000,0.0,0.0]", 
                              "N2 N UN[100,10000,0.0,0.0]",
                              "N3 N UN[100,10000,0.0,0.0]",
                              "N4 N UN[100,10000,0.0,0.0]",
                              "ra A UN[0.05,0.95,0.0,0.0]",
                              "t32 T UN[10,1000,0.0,0.0]",
                              "t21 T UN[10,1000,0.0,0.0]",
                              "t431 T UN[10,1000,0.0,0.0]"), 
             raw_scenario_list="N1 N2 N3 N4\n0 sample 1\n0 sample 2\n0 sample 3\n0 sample 4\nt431 split 4 1 3 ra\nt32 merge 2 3\nt21 merge 1 2", 
             simu_mode="DRAW UNTIL", valid=TRUE)
    )
    
    
    
})