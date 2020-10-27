## Running full-baseline leave-one-out

leave_one_out <- function(repunits_file=FALSE){
  
  #### 01. Load in baseline ####
  # Select a baseline to analyse
  rubias_base.FN <- choose.files(caption = "Select a rubias formatted base file")

  ## Load baseline data - assuming pre-filtered here.
  rubias_base <- read_tsv(rubias_base.FN,guess_max=150000)
  
  if(repunits_file==TRUE){
    # Load repunits file
    repunit_desc.FN <- choose.files(getwd(),caption = "Path to repunits for analysis")
   
    # Load repunits file specified in config
    print("Loading Reporting Units Detail")
    repunits <- read_tsv(repunit_desc.FN)
   
    # Keep the Display_Order for downstream sorting
    Display_Order <- repunits[c("repunit","Display_Order")]
   
  }
  
  # Find a list of the unique collections and repunits
  colls <- unique(rubias_base[, c("collection", "repunit")])
  
  # Create an identical file for downstream labelling
  coll_infer <- colls
  
  # Change the names to make it obvious as to what they mean
  colnames(coll_infer) <- c("inferred_collection","inferred_repunit")
  
  # Format cols to character or integer as required for rubias; based on the indiv col
  rubias_base[, 1:which(colnames(rubias_base)=="indiv")] <- lapply(X = rubias_base[,1:which(colnames(rubias_base)=="indiv")], FUN = as.character)
  rubias_base[, (which(colnames(rubias_base)=="indiv") + 1):ncol(rubias_base)] <- lapply(X = rubias_base[, (which(colnames(rubias_base)=="indiv") + 1):ncol(rubias_base)], FUN = as.integer)
  
  # Run the LOO self-assign script in rubias
  all_collection_results <- self_assign(reference = rubias_base
                                        , gen_start_col = (which(colnames(rubias_base)=="indiv") + 1)
                                                
  )
  
  # Group by inferred repunit and summarize - this is directly from Rubias read-me.
  # This creates a summary of likelihoods to repunits
  best_assignment_repu <- all_collection_results %>%
    group_by(indiv, collection, repunit, inferred_repunit) %>%
    summarise(repu_scaled_like = sum(scaled_likelihood))

  # Our goal here is to take the "best" assignment to repunit
  # There was some question as to whether this is the correct way to represent the data.
  # We decided this likely approximates the Oncor method as closely as we can.
  best_assignment_repu <- best_assignment_repu %>% 
    group_by(indiv) %>% 
    filter(repu_scaled_like == max(repu_scaled_like)) %>% 
    distinct
  
  # Do the same for "collection" - take the "best" assignment to collection. 
  best_assignment_coll <- all_collection_results %>%
    group_by(indiv) %>%
    filter(scaled_likelihood == max(scaled_likelihood)) %>%
    distinct
  
  # Count the number of assignments, summarized by "collection" individual was pulled from, and "inferred repunits" to which it assigned. 
  best_assignment_repu_sum <- best_assignment_repu %>%
    group_by(collection,inferred_repunit) %>%
    tally() %>%
    mutate(freq = n / sum(n)* 100)
  
  # Count the number of assignments, summarized by "collection" individual was pulled from, and "inferred collection" to which it assigned. 
  best_assignment_coll_sum <- best_assignment_coll %>%
    group_by(collection,inferred_collection) %>%
    tally() %>%
    mutate(freq = n / sum(n)* 100)

  # Use the tables created earlier to add back in the repunit ID to the collection and inferred_collection in the summary table
  best_assignment_coll_sum <- merge(best_assignment_coll_sum,colls)
  best_assignment_coll_sum <- merge(best_assignment_coll_sum,coll_infer)

  # Use the tables created earlier to add back in the repunit ID to the collection - the inferred_repunit was already included (this is a to-repunit summary!)
  best_assignment_repu_sum <- merge(best_assignment_repu_sum,colls)
  # 
  ########
  # Leftovers from trying to create a matrix - keep it in in case needed in the future. 
  # best_assignment_matrix <- dcast(best_assignment_sum,collection+repunit~inferred_collection,value.var="freq")
  # best_assignment_matrix_rep <- dcast(best_assignment_sum_rep,collection+repunit~inferred_repunit,value.var="freq")
  # best_assignment_matrix_rep_rep <- dcast(best_assignment_sum_rep_rep,repunit~inferred_repunit,value.var="freq")
  # 
  # # Write out full-summary results
  # write_tsv(x = best_assignment_matrix, path = paste0(result.path, "all_collection_LOO_results_coll_coll_matrix_", format(Sys.time(), "%Y-%m-%d"),".txt"),na="")
  # write_tsv(x = best_assignment_matrix_rep, path = paste0(result.path, "all_collection_LOO_results_coll_rep_matrix_", format(Sys.time(), "%Y-%m-%d"),".txt"),na="")
  # write_tsv(x = best_assignment_matrix_rep_rep, path = paste0(result.path, "all_collection_LOO_results_rep_rep_matrix_", format(Sys.time(), "%Y-%m-%d"),".txt"),na="")
  # 
  ########
  
  write_tsv(x = best_assignment_coll_sum, path = paste0(result.path, "all_collection_LOO_results_all-coll_", format(Sys.time(), "%Y-%m-%d"),".txt"),na="")
  
  write_tsv(x = best_assignment_repu_sum, path = paste0(result.path, "all_collection_LOO_results_all-reps_", format(Sys.time(), "%Y-%m-%d"),".txt"),na="")
  
  
}


