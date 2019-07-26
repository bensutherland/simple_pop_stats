# Calculate AMOVA based on defined repunits
# To deal with missing, use "mean" to impute, "zero" to convert NAs to 0 with a threshold of no missing allowed
#   , "genotype" to drop individuals with missing, or "ignore" to not do anything

calculate_AMOVA <- function(data = obj_pop_filt, missing_treat = "ignore", build_file = FALSE){
  
    if(build_file==TRUE){
      
      print("Building template to create repunits")
      print("The template will be saved as '00_archive/unique_pops.csv'")
      print("Fill in the template by adding column 'repunit' and re-save as comma separated '00_archive/unique_pops_w_repunit.csv'")
      
      # Create file
      unique_pops <- as.data.frame(unique(pop(data)), stringsAsFactors = F)
      colnames(unique_pops) <- "collection"
      print(unique_pops)
      
      # Write out to annotate
      write.table(x = unique_pops, file = "00_archive/unique_pops.csv", row.names = F, col.names = T, quote = F)
      # Manually edit the tab-delim file to include repunits, save as "00_archive/unique_pops_w_repunits.txt
      
    } else if(build_file==FALSE){
      
      print("Assuming the repunit file has already been completed, and is named '00_archive/unique_pops_w_repunit.csv'")
      
      # Apply missing filter
      print(paste0("Applying missing data filter, using ", missing_treat, " method."))
      data <- missingno(pop = data, type = missing_treat, cutoff = 0)
      
      # Read in the repunit file
      strat.FN <- "00_archive/unique_pops_w_repunit.txt"
      strat_obj <- read.table(file = strat.FN, header = T, sep = ",")
      
      ## Add a strat obj to the genind
      # Identify the pop of each sample in the genind
      pops <- as.data.frame(pop(data), stringsAsFactors = F)
      colnames(pops) <- "collection"
      
      # Merge the strata info with the per-indiv pop info to add repunit to each indiv
      strat_and_pops <- merge(x = pops, y = strat_obj, by= "collection", sort = F, all.x = T)
      
      # Attach the strata object to the genind
      data$strata <- strat_and_pops
      unique(data$strata)
      
      ## Characterize sample sizes
      print(table(strata(data, ~collection)))
      print(table(strata(data, ~repunit, combine = FALSE)))
      print(table(strata(data, ~repunit/collection, combine = FALSE)))
      
      # Calculate amova (note: not calculating 'within individuals variance'; lowest level is at the sample level)
      print("Calculating amova with defaults (ade4) in poppr")
      obj_amova <- poppr.amova(x = data, hier = ~repunit/collection, within = FALSE)
      print("Calculating amova with pegas in poppr")
      obj_amova.pegas <- poppr.amova(x = data, hier = ~repunit/collection, within = FALSE, method = "pegas")
      
      assign(x = "obj_amova", value = obj_amova, pos = .GlobalEnv)
      assign(x = "obj_amova.pegas", value = obj_amova.pegas, pos = .GlobalEnv)
      
      print("Your outputs are obj_amova and obj_amova.pegas")
    }
}
