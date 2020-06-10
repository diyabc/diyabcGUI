context("io_management")

test("check_data_file", {
    data_file <- file.path(example_dir(),
                           "IndSeq_SNP_estim_param",
                           "indseq_SNP_sim_dataset_4POP_001.snp")
    data_dir <- file.path(example_dir(),
                          "IndSeq_SNP_estim_param")
    locus_type <- "snp"
    seq_mode <- "indseq"
    check_data_file(data_file, data_dir, locus_type, seq_mode, 
                    expected_data_file = NULL)
})

test("check_file_name", {
    expect_true(check_file_name(system.file("DESCRIPTION", 
                                            package = "diyabcGUI")))
    expect_false(check_file_name(5))
    expect_false(check_file_name(file.path(fs::path_home(), 
                                           "not_existing_file")))
})


test("parse_diyabc_project", {
    file_name <- file.path(example_dir(),
                           "IndSeq_SNP_estim_param",
                           "diyabcGUI_proj.txt")
    file_type = "text/plain"
    expect_equal(parse_diyabc_project(file_name, file_type),
                 list(proj_name="IndSeq_SNP_estim_param", valid=TRUE))
    
    expect_equal(parse_diyabc_project(file_name, file_type = "bin"),
                 list(proj_name=NULL, valid=FALSE))
    
    file_name <- file.path(test_input_dir(),
                           "bad_files",
                           "diyabcGUI_proj.txt")
    file_type = "text/plain"
    expect_equal(parse_diyabc_project(file_name, file_type),
                 list(proj_name=NULL, valid=FALSE))
    
    expect_equal(parse_diyabc_project(file.path(fs::path_home(), 
                                                "not_existing_file"), 
                                      type),
                 list(proj_name=NULL, valid=FALSE))
})

test("parse_diyabc_header", {
    # model choice
    file_name <- file.path(example_dir(),
                           "IndSeq_SNP_model_choice",
                           "header.txt")
    file_type = "text/plain"
    data_type = "snp"
    expect_equal(
        parse_diyabc_header(file_name, file_type, data_type),
        list(data_file="indseq_SNP_sim_dataset_4POP_001.snp", 
             loci_description="5000 <A> G1 from 1", 
             n_loci_des=1, n_param=13, n_sumstat=2, 
             raw_cond_list=c("t21>t32", "t42<t21", "t43<t32", "t32>t423", 
                             "t431<t32", "t421<t21"), 
             raw_prior_list=c("N1 N UN[100,10000,0.0,0.0]", 
                              "N2 N UN[100,10000,0.0,0.0]",
                              "N3 N UN[100,10000,0.0,0.0]",
                              "N4 N UN[100,10000,0.0,0.0]",
                              "t423 T UN[10,1000,0.0,0.0]",
                              "ra A UN[0.05,0.95,0.0,0.0]",
                              "t32 T UN[10,1000,0.0,0.0]",
                              "t21 T UN[10,1000,0.0,0.0]",
                              "t421 T UN[10,1000,0.0,0.0]",
                              "t431 T UN[10,1000,0.0,0.0]",
                              "t41 T UN[10,1000,0.0,0.0]",
                              "t42 T UN[10,1000,0.0,0.0]",
                              "t43 T UN[10,1000,0.0,0.0]"), 
             raw_scenario_list=c(
                 "N1 N2 N3 N4\n0 sample 1\n0 sample 2\n0 sample 3\n0 sample 4\nt423 split 4 2 3 ra\nt32 merge 2 3\nt21 merge 1 2",
                 "N1 N2 N3 N4\n0 sample 1\n0 sample 2\n0 sample 3\n0 sample 4\nt421 split 4 2 1 ra\nt32 merge 2 3\nt21 merge 1 2",
                 "N1 N2 N3 N4\n0 sample 1\n0 sample 2\n0 sample 3\n0 sample 4\nt431 split 4 1 3 ra\nt32 merge 2 3\nt21 merge 1 2",
                 "N1 N2 N3 N4\n0 sample 1\n0 sample 2\n0 sample 3\n0 sample 4\nt41 merge 1 4\nt32 merge 2 3\nt21 merge 1 2",
                 "N1 N2 N3 N4\n0 sample 1\n0 sample 2\n0 sample 3\n0 sample 4\nt42 merge 2 4\nt32 merge 2 3\nt21 merge 1 2",
                 "N1 N2 N3 N4\n0 sample 1\n0 sample 2\n0 sample 3\n0 sample 4\nt43 merge 3 4\nt32 merge 2 3\nt21 merge 1 2"), 
             simu_mode="DRAW UNTIL", valid=TRUE)
    )
    
    # parameter estimation
    file_name <- file.path(example_dir(),
                           "IndSeq_SNP_estim_param",
                           "header.txt")
    file_type = "text/plain"
    data_type = "snp"
    expect_equal(
        parse_diyabc_header(file_name, file_type, data_type),
        list(data_file="indseq_SNP_sim_dataset_4POP_001.snp", 
             loci_description="5000 <A> G1 from 1", 
             n_loci_des=1, n_param=13, n_sumstat=2, 
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
    
    # bad input
    file_name <- file.path(test_input_dir(),
                           "bad_files",
                           "header.txt")
    file_type = "text/plain"
    expect_equal(
        parse_diyabc_header(file_name, file_type, data_type = "snp"),
        list(data_file="indseq_SNP_sim_dataset_4POP_001.snp", 
             loci_description=NULL, 
             n_loci_des=NULL, n_param=13, n_sumstat=2, 
             raw_cond_list=NULL,
             raw_prior_list=NULL, 
             raw_scenario_list=NULL, 
             simu_mode=NULL, valid=FALSE)
    )
    
    expect_equal(
        parse_diyabc_header(file.path(fs::path_home(), "not_existing_file"), 
                            file_type, data_type),
        list(data_file=NULL, 
             loci_description=NULL, 
             n_loci_des=NULL, n_param=NULL, n_sumstat=NULL, 
             raw_cond_list=NULL,
             raw_prior_list=NULL, 
             raw_scenario_list=NULL, 
             simu_mode=NULL, valid=FALSE)
    )
})

test("parse_diyabc_header_scenarii", {
    # model choice
    file_name <- file.path(example_dir(),
                           "IndSeq_SNP_model_choice",
                           "header.txt")
    raw_content <- readLines(file_name)
    raw_content <- raw_content[raw_content != ""]
    
    expect_equal(parse_diyabc_header_scenarii(raw_content[4:12]),
                 list(id=1, n_param=8, prior=0.16667,
                      raw_scenario=str_c(raw_content[5:12], collapse = "\n"),
                      valid=TRUE))
    
    expect_equal(parse_diyabc_header_scenarii(raw_content[5:12]),
                 list(id=NULL, n_param=NULL, prior=NULL, raw_scenario=NULL,
                      valid=FALSE))
})