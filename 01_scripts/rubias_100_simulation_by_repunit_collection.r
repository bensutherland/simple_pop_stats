library(rubias)
library(readr)
library(dplyr)

# Select your config file
base.FN <- choose.files(caption = "Select a Rubias Baseline file") # interactive
setwd( dirname(base.FN))
rubias_base<-read_tsv(base.FN)

rubias_base <- rubias_base %>%
                group_by(collection) %>%
                filter(n() >10) 

#save this as new baseline
write_tsv(rubias_base,paste0(dirname(base.FN),"Rubias_Coll_10_",
                             format(Sys.time(), "%Y-%m-%d"),".txt"))

repunit.CNT<-rubias_base %>%
             count(repunit,name="n_per_repunit")

repunit.collection_CNT<-rubias_base %>%
             group_by(repunit,collection) %>%
             tally() %>%
             count(repunit,name="#collections")

collection.CNT<-rubias_base %>%
                count(collection,name="n_per_collection")
  
# Remove column if exists (warning if doesn't)
rubias_base<-select(rubias_base,-one_of("allele.Count"))
#Rubias requires an ungrouped tibble - 
rubias_base<-ungroup(rubias_base)

# Format cols to character or integer required for rubias 
rubias_base[, 1:4] <- lapply(X = rubias_base[,1:4], FUN = as.character)
rubias_base[, 5:ncol(rubias_base)] <- lapply(X = rubias_base[, 5:ncol(rubias_base)], FUN = as.integer)

repunits<-unique(rubias_base$repunit)

if(FALSE)  { # Skip repunits Simulations
all_repunit_scenario <- lapply(repunits,
                               function(x) tibble(repunit = x, ppn = 1.0))
names(all_repunit_scenario) <- paste("All", repunits, sep = "-")
# Then, we use it, producing only 5 replicates for each scenario:
  
all_repunit_results <- assess_reference_loo(reference = rubias_base, 
                                             gen_start_col = 5, 
                                             reps = 5, 
                                             mixsize = 200,
                                             alpha_repunit = all_repunit_scenario)
#filter for targets 
repunit.100<-all_repunit_results %>%
  group_by(repunit_scenario,repunit,collection) %>%
  summarise_at(.vars=c("post_mean_pi","mle_pi","true_pi"),
               .funs=c(mean="mean"))  %>%
  group_by(repunit_scenario,repunit) %>%
  filter(true_pi_mean > 0)  %>%
  summarise_at(.vars=c("post_mean_pi_mean","mle_pi_mean","true_pi_mean"),
               .funs=c(sum="sum")) 

repunit.100<-inner_join(repunit.CNT,repunit.100) %>%
  inner_join(.,repunit.collection_CNT )

write_tsv(all_repunit_results,paste0("all_repunit_results",format(Sys.time(), "%Y-%m-%d"),".gz"))
write_tsv(repunit.100,paste0("repunit_100_",format(Sys.time(), "%Y-%m-%d"),".txt"))          

}  # End of Skipped lines

collections<-unique(rubias_base$collection)

all_collection_scenario <- lapply(collections,
                               function(x) tibble(collection = x, ppn = 1.0))
names(all_collection_scenario) <- collections

# Then, we use it, producing only 5 replicates for each scenario:
all_collection_results <- assess_reference_loo(reference = rubias_base
                                            ,gen_start_col = 5
                                            ,reps = 10
                                            ,mixsize = 200
                                            ,alpha_collection = all_collection_scenario
                                            )
#Collection by Collection
collection.100<-all_collection_results %>%
                group_by(collection_scenario,repunit,collection) %>%
                summarise_at(.vars=c("post_mean_pi","mle_pi","true_pi"),
                             .funs=c(mean="mean")) %>%
                group_by(collection_scenario) %>%
                filter(true_pi_mean == 1)

# Can't just filter on true = 1
# match repunit with estimated repunit and summerize
collection.rep_100<-all_collection_results %>%
                    group_by(collection_scenario,repunit,collection) %>%
                    summarise_at(.vars=c("post_mean_pi","mle_pi","true_pi"),
                                 .funs=c(mean="mean")) %>%
                    group_by(collection_scenario,repunit) %>%
                    summarise_at(.vars=c("post_mean_pi_mean","mle_pi_mean","true_pi_mean"),
                                 .funs=c(sum="sum"))  %>%
                    filter(true_pi_mean_sum > 0)

names(collection.rep_100)<-c("collection","repunit","repunit_post_mean_pi"
                             ,"repunit_mle_pi","repunit_true_pi")

collection.100<-inner_join(collection.CNT,collection.100) %>%
                           inner_join(.,collection.rep_100) %>%
                           inner_join(.,repunit.collection_CNT )

# Keep these outputs for later
write_tsv(all_collection_results,paste0("all_collection_results",format(Sys.time(), "%Y-%m-%d"),".gz"))
write_tsv(collection.100,paste0("collection_100_",format(Sys.time(), "%Y-%m-%d"),".txt"))          



