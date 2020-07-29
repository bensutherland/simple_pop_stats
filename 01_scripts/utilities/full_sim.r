## Running 100 percent simulations
# proportions.FN should be a tab-delim file with header collections \t ppn
full_sim <- function(rubias_base.FN = "03_results/rubias_output.txt"
                     , num_sim_indiv = 200 # number of indiv to sim from the baseline
                     , sim_reps = 100 
                     , filter_by_pop_size = FALSE
                     , min_pop_size = 10
                     , proportions.FN = NULL
                     #, repunit_sim = FALSE   # simulate by repunit
                     #, keep_false = FALSE     # Select whether all populations are retained (TRUE) or populations marked as FALSE in the keep column are dropped (FALSE)
                     #, all_collections = TRUE # Run all collections in 100 percent simulation; # Are all populations to be run, or only those that are specified by 'collections_to_use'
                     ){
  
  #### 01. Load in baseline ####
  ## Load baseline data
  rubias_base <- read_tsv(rubias_base.FN,guess_max=100000)
  
  
  #### TODO: This is better somewhere else ####
  ## Optional # filtering
  if(filter_by_pop_size==TRUE){
    
    # Reporting
    print("Dropping pops due to sample size")
    
    # Filter by pop size on collection
    rubias_base <- rubias_base %>%
                    group_by(collection) %>%
                    filter(n() > min_pop_size) %>%
                    ungroup(.)
    
    # Reporting
    print("After filtering, save out the rubias base")
    
    # Save
    filtered_base.FN <- gsub(pattern = ".txt", replacement = "", x = rubias_base.FN)
    filtered_base.FN <- paste0(filtered_base.FN, "_coll_10_", format(Sys.time(), "%Y-%m-%d"),".txt")
    write_tsv(x = rubias_base, path = filtered_base.FN)
  }
  #### END\TODO: This is better somewhere else ####
  
  
  #### 02. Generate counts per collection and per repunit ####
  print("Generating counts per collection and per repunit")
  
  # Number of fish per collection
  collection.CNT <- rubias_base %>%
    count(collection, name="n_per_collection")
  
  # Number of fish per repunit
  repunit.CNT <- rubias_base %>%
    count(repunit, name="n_per_repunit")
  
  # Number of collections per repunit
  repunit.collection_CNT <- rubias_base %>%
    group_by(repunit, collection) %>%
    tally() %>%
    select(-n) %>%
    count(repunit, name="#collections")
  
  # Drop column if exists (warning if doesn't)
  # rubias_base <- select(rubias_base, -one_of("allele.Count"))
  
  # Ungroup tibble as required by Rubias
  rubias_base <- ungroup(rubias_base)
  
  # Format cols to character or integer as required for rubias; based on the indiv col
  rubias_base[, 1:which(colnames(rubias_base)=="indiv")] <- lapply(X = rubias_base[,1:which(colnames(rubias_base)=="indiv")], FUN = as.character)
  rubias_base[, (which(colnames(rubias_base)=="indiv") + 1):ncol(rubias_base)] <- lapply(X = rubias_base[, (which(colnames(rubias_base)=="indiv") + 1):ncol(rubias_base)], FUN = as.integer)
  
  # Identify repunits
  repunits <- unique(rubias_base$repunit)
  
  # Identify collections
  collections <- unique(rubias_base$collection)
  
  
  #### 03. Define proportions per stock ####
  ### Determine 100% simulation or fishery sample
  # If a proportion filename is given, use it
  if(!is.null(proportions.FN)){
  
    # Reporting
    print("Performing a set-proportion mixture")
    
    # Read in the proportion filename:
    ppn.df <- read_tsv(file = proportions.FN)
    
    # Sub in this proportion for the all_collection_scenario variable
    ppn.df
    
    # Put into the list of scenarios
    all_collection_scenario <- list()
    scenario.name <- gsub(pattern = "\\..*", replacement = "", x = proportions.FN)
    all_collection_scenario[[scenario.name]] <- ppn.df
    
  
  }else {
  
    # Reporting
    print("Performing 100% simulation")
    
    # Make a repunit list, essentially a list of tibbles (one per repunit), contains 'ppn'
    all_collection_scenario <- lapply(collections,
                                      function(x) tibble(collection = x, ppn = 1.0))
    # Name the list
    #names(all_collection_scenario) <- collections # old method
    for(i in 1:length(all_collection_scenario)){
      names(all_collection_scenario)[[i]] <- all_collection_scenario[[i]]$collection
    }
    
    all_collection_scenario
    
  
  }

  
  #### 04. Run Simulation ####
  print("Running simulation and assessing the simulation assignments")
  print(paste0("This will include a total of ***", length(all_collection_scenario), "*** scenarios"))
  # Run simulation and put results into the repunit list
  # creates genotype-logL matrix based on simulation-by-indiv w/ randomly drawn popn proportions
  # then uses two estimates of mixture (maxL, MCMC)
  all_collection_results <- assess_reference_loo(reference = rubias_base
                                                 , gen_start_col = (which(colnames(rubias_base)=="indiv") + 1)
                                                 , reps = sim_reps
                                                 , mixsize = num_sim_indiv
                                                 , alpha_collection = all_collection_scenario
  )
  
  

  
  #### 05. Collect and summarize output ####
  print("Collecting and summarizing outputs")
  
  # Filter for same-on-same (targets); take the average of all reps for post_mean_pi, mle_pi, and true_pi
  coll_to_coll_filt_res <- all_collection_results %>%
                              group_by(collection_scenario, repunit, collection) %>%
                              summarise_at(.vars=c("post_mean_pi","mle_pi","true_pi")
                              , .funs=c(mean="mean")) %>%
                                  group_by(collection_scenario) %>%
                                  filter(true_pi_mean > 0)
  
  
  # Match repunit with estimated repunit and summarize
  coll_to_rep_filt_res <- all_collection_results %>%
                              group_by(collection_scenario, repunit, collection) %>%
                              summarise_at(.vars=c("post_mean_pi","mle_pi","true_pi")
                              , .funs=c(mean="mean")) %>%
                                  group_by(collection_scenario, repunit) %>%
                                  summarise_at(.vars=c("post_mean_pi_mean","mle_pi_mean","true_pi_mean")
                                  , .funs=c(sum="sum"))  %>%
                              filter(true_pi_mean_sum > 0)
  
  # Assign names
  names(coll_to_rep_filt_res) <- c("collection_scenario"
                                   , "repunit"
                                   , "repunit_post_mean_pi"
                                   , "repunit_mle_pi"
                                   , "repunit_true_pi")
  
  
  # Merge outputs for report (automatically finds the joining )
  coll_all <- inner_join(collection.CNT, coll_to_coll_filt_res) %>%
                inner_join(., coll_to_rep_filt_res) %>%
                inner_join(., repunit.collection_CNT ) %>%
                inner_join(., repunit.CNT )
  
  
  
  #### Collect all results to collection, to find everything past the first result assignment details ####
  coll_to_coll_filt_all <- all_collection_results %>%
                              group_by(collection_scenario, repunit, collection) %>%
                                summarise_at(.vars=c("post_mean_pi","mle_pi","true_pi")
                                , .funs=c(mean="mean")) %>%
                                group_by(collection_scenario)
  
  # Provide names
  names(coll_to_coll_filt_all) <- c("collection_scenario"
                                    ,"repunit"
                                    ,"collection"
                                    ,"coll_post_mean_pi"
                                    ,"coll_mle_pi"
                                    ,"coll_true_pi")
  
  #### Collect all results to repunit, to find everything past the first result assignment details ####
  coll_to_rep_filt_all <- all_collection_results %>%
                              group_by(collection_scenario, repunit, collection) %>%
                                summarise_at(.vars=c("post_mean_pi","mle_pi","true_pi")
                                , .funs=c(mean="mean")) %>%
                                  group_by(collection_scenario, repunit) %>%
                                    summarise_at(.vars=c("post_mean_pi_mean","mle_pi_mean","true_pi_mean")
                                    , .funs=c(sum="sum"))
  # Provide names
  names(coll_to_rep_filt_all) <- c("collection_scenario"
                                   ,"repunit"
                                   ,"repunit_post_mean_pi"
                                   ,"repunit_mle_pi"
                                   ,"repunit_true_pi")
  
  
  
  #### Export results ###
  print("Exporting results")
  # Write out the full raw result of the simulation assignments
  write_tsv(x = all_collection_results, path = paste0(result.path, "all_collection_results_", format(Sys.time(), "%Y-%m-%d"),".txt.gz"))
  
  # Write out the summary report of the collection and repunit assignment results
  write_tsv(x = coll_all, path = paste0(result.path, "collection_100_stats_", format(Sys.time(), "%Y-%m-%d"),".txt"))
  
  # Write out all info
  write_tsv(x = coll_to_coll_filt_all, path = paste0(result.path, "collection_100_stats_all_pops_",format(Sys.time(), "%Y-%m-%d"),".txt"))
  write_tsv(x = coll_to_rep_filt_all, path = paste0(result.path, "collection_100_stats_all_reps_",format(Sys.time(), "%Y-%m-%d"),".txt"))
  
  
  coll_to_coll_filt_all_matrix <- dcast(coll_to_coll_filt_all,collection~collection_scenario,value.var="coll_post_mean_pi")
  write_tsv(x = coll_to_coll_filt_all_matrix, path = paste0(result.path, "collection_100_stats_all_pops_matrix_",format(Sys.time(), "%Y-%m-%d"),".txt"))
}


