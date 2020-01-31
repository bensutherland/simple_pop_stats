# Replace pop in genind from 'collection' to 'repunit'
# Will use the stock codes file created in calculate_AMOVA

pop_to_repunit <- function(data = obj){
  
  # Reporting
  print("Your original data contains the following populations")
  print(table(pop(data)))
  
  # Hack solution, needs to be updated with test to see if file exists, if doesn't will crash
  strat.FN <- "00_archive/unique_pops_w_repunit.csv"
  
  if(file.exists(strat.FN)==TRUE){
  
    # Read in the repunit file
    strat_obj <- read.table(file = strat.FN, header = T, sep = ",")
    
    # Identify the pop ID
    pops <- as.data.frame(pop(data), stringsAsFactors = F)
    colnames(pops) <- "collection"
    
    # Merge the strata info with the per-indiv pop info to add repunit to each indiv
    strat_and_pops <- merge(x = pops, y = strat_obj, by= "collection", sort = F, all.x = T)
    
    # Rename
    pop(data) <- strat_and_pops$repunit
    
    print("Your data will now be saved as 'obj_repunit'")
    assign(x = "obj_repunit", value = data, envir = .GlobalEnv)
    
    # Reporting
    print("Your new data contains the following populations")
    print(table(pop(obj_repunit)))
    
    
  } else if(file.exists(strat.FN)==FALSE){
    
    print("There is no strata file, build this using the calculate_AMOVA(build = TRUE) function")
    
  }
}
