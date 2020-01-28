## 100 percent simulations
# First source the main script (i.e., PBT_analysis_main.R) to load all of the paths

# Set working directory
setwd(paste0(current.path, "/04_analysis"))


# Set parameters and settings
# Set variables
min_pop_size <- 10
filter_by_pop_size <- FALSE

# On validating baseline, are CUs correct.
repunit_sim <- FALSE # True does by repunit, FALSE Skips this step

# Select the number of individuals to simulate from the baseline
num_sim_indiv <- 200 # Standard from Terry
sim_reps <- 100 # 10 to test, 100 default, more better. 

# Select whether all populations are retained (TRUE) or populations marked as FALSE in the keep column are dropped (FALSE)
keep_false <- FALSE 

# Are all populations to be run, or only those that are specified by 'collections_to_use'
all_collections <- TRUE  # Run all collections in 100 percent simulation
# all_collections <- FALSE # Run a subset of populations, under the "collections_to_use" vector
# collections_to_use <- c("NITINAT_RIVER","SOOKE_RIVER") 


# Load baseline
# Select your rubias formatted baseline file
base.FN <- choose.files(caption = "Select a Rubias Baseline file") # interactive

# Load baseline data
rubias_base <- read_tsv(base.FN)


# Optional filtering (remove populations with fewer than 10 indiv
if(filter_by_pop_size==TRUE){
  
  # Reporting
  print("Filtering by population size")
  
  # Filter by pop size on collection
  rubias_base <- rubias_base %>%
    group_by(collection) %>%
    filter(n() > min_pop_size) %>%
    ungroup(.)
  
  # Reporting
  print("Saving out the filtered pop rubias base")
  
  # Save
  input_base.FN <- gsub(pattern = ".txt", replacement = "", x = basename(base.FN))
  
  write_tsv(x = rubias_base, path = paste0(dirname(base.FN), "/"
                , input_base.FN
                , "_coll_10_"
                , format(Sys.time(), "%Y-%m-%d"),".txt")
            )
  }
# End optional filtering


## Statistics per repunit and collection
# Number of fish per collection
collection.CNT <- rubias_base %>%
                    count(collection, name="n_per_collection")

# Number fish per repunit
repunit.CNT <- rubias_base %>%
                    count(repunit, name="n_per_repunit")

# Number of collections per repunit
repunit.collection_CNT <- rubias_base %>%
                            group_by(repunit, collection) %>%
                            tally() %>%
                            count(repunit,name="#collections")


# Drop column if exists (warning if doesn't)
rubias_base <- select(rubias_base, -one_of("allele.Count"))

# Ungroup tibble as required by Rubias
rubias_base <- ungroup(rubias_base)


# Format cols to character or integer as required for rubias
# Modified to select columns based on the position of the "indiv" column. 
# Expectation is that it is the last column before allele data. 
rubias_base[, 1:which(colnames(rubias_base)=="indiv")] <- lapply(X = rubias_base[,1:which(colnames(rubias_base)=="indiv")], FUN = as.character)
rubias_base[, (which(colnames(rubias_base)=="indiv") + 1):ncol(rubias_base)] <- lapply(X = rubias_base[, (which(colnames(rubias_base)=="indiv") + 1):ncol(rubias_base)], FUN = as.integer) 

## #TODO: this needs cleaning up ##
# # If keep_false == FALSE, the rows marked "FALSE" in the KEEP column are retained. 
# if(keep_false == FALSE){
#     rubias_base <- rubias_base[rubias_base$Keep == TRUE,]
#     }
## #TODO end

# Identify repunits
repunits <- unique(rubias_base$repunit)


# #### Repunit simulations ####
# if(repunit_sim==TRUE){
#   
#   # Make an empty list of tibbles, one per repunit
#   all_repunit_scenario <- lapply(repunits,
#                                  function(x) tibble(repunit = x, ppn = 1.0))
#   
#   # Give the list slots names
#   names(all_repunit_scenario) <- paste("All", repunits, sep = "-")
#   
#   # Run simulation and put results into repunit list
#   # Creates genotype-logL matrix based on simulation-by-indiv w/ randomly drawn popn proportions
#   #  then uses two estimates of mixture (maxL, MCMC)
#   all_repunit_results <- assess_reference_loo(reference = rubias_base
#                                               , gen_start_col = (which(colnames(rubias_base)=="indiv") + 1)
#                                               , reps = sim_reps
#                                               , mixsize = num_sim_indiv
#                                               , alpha_repunit = all_repunit_scenario
#                                               )
#   
#   # Filter for targets 
#   repunit.100 <- all_repunit_results %>%
#                     
#     # Group the data and summarize
#     group_by(repunit_scenario, repunit, collection) %>%
#                     
#     summarise_at(.vars=c("post_mean_pi","mle_pi","true_pi"),
#     .funs=c(mean="mean"))  %>%
#     group_by(repunit_scenario,repunit) %>%
#     filter(true_pi_mean > 0)  %>%
#     summarise_at(.vars=c("post_mean_pi_mean","mle_pi_mean","true_pi_mean"),
#     .funs=c(sum="sum")) 
#   
#   repunit.100<-inner_join(repunit.CNT,repunit.100) %>%
#     inner_join(.,repunit.collection_CNT )
#   
#   write_tsv(all_repunit_results,paste0("all_repunit_results",format(Sys.time(), "%Y-%m-%d"),".gz"))
#   write_tsv(repunit.100,paste0("repunit_100_",format(Sys.time(), "%Y-%m-%d"),".txt"))          
#   
#   
#   
# }


#### Collection by Collection Simulation ####
# Determine what collections are to be used
if(all_collections == TRUE){
  # define all collections
  collections <- unique(rubias_base$collection)
  
} else if(all_collections == FALSE){
collections <- collections_to_use
}

# Make an empty list of tibbles, one per repunit
all_collection_scenario <- lapply(collections,
                               function(x) tibble(collection = x, ppn = 1.0))
# Name the list
names(all_collection_scenario) <- collections

# Run simulation and put results into repunit list
# creates genotype-logL matrix based on simulation-by-indiv w/ randomly drawn popn proportions
# then uses two estimates of mixture (maxL, MCMC)
all_collection_results <- assess_reference_loo(reference = rubias_base
                                            , gen_start_col = (which(colnames(rubias_base)=="indiv") + 1)
                                            , reps = sim_reps
                                            , mixsize = num_sim_indiv
                                            , alpha_collection = all_collection_scenario
                                            )

# Filter for same-on-same (targets)
coll_to_coll_filt_res <- all_collection_results %>%
                 group_by(collection_scenario, repunit, collection) %>%
                 summarise_at(.vars=c("post_mean_pi","mle_pi","true_pi"),
                             .funs=c(mean="mean")) %>%
                 group_by(collection_scenario) %>%
                 filter(true_pi_mean == 1)
# This gives results to collection # Note, single 



### Note to be removed ###
# Can't just filter on true = 1
### End note to be removed ###


# Match repunit with estimated repunit and summarize
# i.e. How well does this collection result go back to its repunit
coll_to_rep_filt_res <- all_collection_results %>%
                    group_by(collection_scenario, repunit, collection) %>%
                    summarise_at(.vars=c("post_mean_pi","mle_pi","true_pi"),
                                 .funs=c(mean="mean")) %>%
                    group_by(collection_scenario, repunit) %>%
                    summarise_at(.vars=c("post_mean_pi_mean","mle_pi_mean","true_pi_mean"),
                                 .funs=c(sum="sum"))  %>%
                    filter(true_pi_mean_sum > 0)

names(coll_to_rep_filt_res) <- c("collection","repunit","repunit_post_mean_pi"
                             ,"repunit_mle_pi","repunit_true_pi")




# Merge outputs for report (automatically finds the joining )
coll_all <- inner_join(collection.CNT, coll_to_coll_filt_res) %>%
                           inner_join(., coll_to_rep_filt_res) %>%
                           inner_join(., repunit.collection_CNT )



coll_to_coll_filt_all <- all_collection_results %>%
  group_by(collection_scenario, repunit, collection) %>%
  summarise_at(.vars=c("post_mean_pi","mle_pi","true_pi"),
               .funs=c(mean="mean")) %>%
  group_by(collection_scenario)


coll_to_rep_filt_all <- all_collection_results %>%
  group_by(collection_scenario, repunit, collection) %>%
  summarise_at(.vars=c("post_mean_pi","mle_pi","true_pi"),
               .funs=c(mean="mean")) %>%
  group_by(collection_scenario, repunit) %>%
  summarise_at(.vars=c("post_mean_pi_mean","mle_pi_mean","true_pi_mean"),
               .funs=c(sum="sum"))  

names(coll_to_rep_filt_all) <- c("collection","repunit","repunit_post_mean_pi"
                                 ,"repunit_mle_pi","repunit_true_pi")


# Keep these outputs for later
# Write out the large object
write_tsv(all_collection_results, paste0("all_collection_results_", format(Sys.time(), "%Y-%m-%d"),".txt.gz"))

# Write out the report
write_tsv(coll_all, paste0("collection_100_stats_",format(Sys.time(), "%Y-%m-%d"),".txt"))

# Write out all info
write_tsv(coll_to_coll_filt_all,paste0("collection_100_stats_all_pops_",format(Sys.time(), "%Y-%m-%d"),".txt.gz"))
write_tsv(coll_to_rep_filt_all,paste0("collection_100_stats_all_reps_",format(Sys.time(), "%Y-%m-%d"),".txt.gz"))
