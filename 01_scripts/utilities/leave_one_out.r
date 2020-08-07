## Running full-baseline leave-one-out

leave_one_out <- function(){
  
  #### 01. Load in baseline ####
  
  
  
  # Select a baseline to analyse
  rubias_base.FN <- choose.files(caption = "Select a rubias formatted base file")

  ## Load baseline data - assuming pre-filterd here.
  rubias_base <- read_tsv(rubias_base.FN,guess_max=100000)
  
  
  # Reduce repunits
  repunit_desc.FN <- choose.files(getwd(),caption = "Path to repunits for analysis")
  
  # Load repunits file specified in config
  print("Loading Reporting Units Detail")
  repunits <- read_tsv(repunit_desc.FN)
  
  Display_Order <- repunits[c("repunit","Display_Order")]
  
  
  
  colls <- unique(rubias_base[, c("collection", "repunit")])
  coll_infer <- colls
  colnames(coll_infer) <- c("inferred_collection","inferred_repunit")
  
  # Format cols to character or integer as required for rubias; based on the indiv col
  rubias_base[, 1:which(colnames(rubias_base)=="indiv")] <- lapply(X = rubias_base[,1:which(colnames(rubias_base)=="indiv")], FUN = as.character)
  rubias_base[, (which(colnames(rubias_base)=="indiv") + 1):ncol(rubias_base)] <- lapply(X = rubias_base[, (which(colnames(rubias_base)=="indiv") + 1):ncol(rubias_base)], FUN = as.integer)
  
  # Run the LOO self-assign script in rubias
  all_collection_results <- self_assign(reference = rubias_base
                                        , gen_start_col = (which(colnames(rubias_base)=="indiv") + 1)
                                                
  )
  
  best_assignment <- all_collection_results %>% 
    group_by(indiv) %>% 
    filter(scaled_likelihood == max(scaled_likelihood)) %>% 
    distinct
  
  
  #### Collect all results to collection 
  best_assignment_sum <- best_assignment %>%
    group_by(collection,inferred_collection) %>%
    tally() %>%
    mutate(freq = n / sum(n)* 100)
  
  best_assignment_sum <- merge(best_assignment_sum,colls)
  best_assignment_sum <- merge(best_assignment_sum,coll_infer)

  best_assignment_sum_rep <- best_assignment %>%
    group_by(collection,inferred_repunit) %>%
    tally() %>%
    mutate(freq = n / sum(n)* 100)
  
  best_assignment_sum_rep <- merge(best_assignment_sum_rep,colls)
  
  best_assignment_sum_rep_rep <- best_assignment %>%
    group_by(repunit,inferred_repunit) %>%
    tally() %>%
    mutate(freq = n / sum(n) * 100)
  
  best_assignment_matrix <- dcast(best_assignment_sum,collection+repunit~inferred_collection,value.var="freq")
  best_assignment_matrix_rep <- dcast(best_assignment_sum_rep,collection+repunit~inferred_repunit,value.var="freq")
  best_assignment_matrix_rep_rep <- dcast(best_assignment_sum_rep_rep,repunit~inferred_repunit,value.var="freq")
  
  # Write out full-summary results
  write_tsv(x = best_assignment_matrix, path = paste0(result.path, "all_collection_LOO_results_coll_coll_matrix_", format(Sys.time(), "%Y-%m-%d"),".txt"),na="")
  write_tsv(x = best_assignment_matrix_rep, path = paste0(result.path, "all_collection_LOO_results_coll_rep_matrix_", format(Sys.time(), "%Y-%m-%d"),".txt"),na="")
  write_tsv(x = best_assignment_matrix_rep_rep, path = paste0(result.path, "all_collection_LOO_results_rep_rep_matrix_", format(Sys.time(), "%Y-%m-%d"),".txt"),na="")
  
  

  #### Collect all results to collection 
  coll_to_coll_filt_all <- all_collection_results %>%
    group_by(repunit,collection, inferred_repunit, inferred_collection) %>%
    summarise_at(.vars=c("scaled_likelihood")
                 , .funs=c(mean_scaled_likelihood_coll="mean")) %>%
    group_by(collection)
  
  
  
  
  
  # Collect all results to repunit
  coll_to_rep <- coll_to_coll_filt_all %>%
    group_by(collection, inferred_repunit) %>%
    summarise_at(.vars=c("mean_scaled_likelihood_coll")
                 , .funs=c(mean_scaled_likelihood_repunit="sum")) 
  
  # Re-sort so that results are easy to view
  coll_to_coll_filt_all_sort <- coll_to_coll_filt_all[with(coll_to_coll_filt_all, order(collection, -mean_scaled_likelihood_coll)),]
  
  

  
  # Add in summary to repunit
  coll_to_coll_filt_all_sort <- merge(coll_to_coll_filt_all_sort,coll_to_rep)
  
  # Create a 1-line summary per collection
  coll_to_coll_match <- coll_to_coll_filt_all_sort[coll_to_coll_filt_all_sort$collection == coll_to_coll_filt_all_sort$inferred_collection,]
  
  # Write out full-summary results
  write_tsv(x = coll_to_coll_filt_all_sort, path = paste0(result.path, "all_collection_LOO_results_", format(Sys.time(), "%Y-%m-%d"),".txt"))
  
  # Write out the 1-line summary per collection
  write_tsv(x = coll_to_coll_match, path = paste0(result.path, "collection_100_LOO_stats_", format(Sys.time(), "%Y-%m-%d"),".txt"))
  
  coll_to_coll_filt_all_matrix <- dcast(coll_to_coll_filt_all_sort,collection+repunit~inferred_collection,value.var="mean_scaled_likelihood_coll")
  coll_to_coll_filt_all_matrix <- merge(coll_to_coll_filt_all_matrix, Display_Order)
  
  collections <- coll_to_coll_filt_all_matrix[c("collection","repunit","Display_Order")]
  
  
  collections_t <- t(collections)
  
  write_tsv(x = coll_to_coll_filt_all_matrix, path = paste0(result.path, "collection_100_LOO_stats_all_pops_matrix_",format(Sys.time(), "%Y-%m-%d"),".txt"))
  
  
}


