# Rename microsat pops
# This assumes you have access to a names.dat that has been converted to tab-delimited format (use fixed-width text-to-columns in excel)

fix_pop_names <- function(data = obj, append_region = FALSE, stockcode.FN = NULL){
  
  # Pull pop names out of the genind data
  print("Renaming populations")
  
  # Inclusively replace everything after first space in vector (also must be done to the stock code file)
  all_pops <- as.character(pop(data))
  all_pops <- sub(pattern = " .*", replacement = "", x = all_pops)
  
  # # Confirm names are matching
  if(length(unique(all_pops))==length(unique(as.character(pop(data))))){
    print("After removing everything after the first space, there are still the same number of populations")
    print("Continuing...")
  }else{
    print("After removing everything after the first space, some loss of info happened. Stopping now.")
    stop()
  }
  
  pop(data) <- all_pops
  
  # If appending region, use the following:
  if(append_region==TRUE & !is.null(stockcode.FN)){
    
    # Read in stock code file
    stockcode.df <- read.delim(stockcode.FN, stringsAsFactors = FALSE)
    
    # Sort by stock code
    stockcode.df <- stockcode.df[order(stockcode.df$Code), ]
    
    # There are occasionally errors in stock code files, where a collection is assigned two different stock codes and same region
    # , this causes merges to fail, so get rid of any duplicates like that
    duplicate.df <- stockcode.df[duplicated(paste0(stockcode.df$collection)),] # Identifies duplicates (second copy)
    print("If there are duplicate stock collection names, the following will identify those that will be removed: ")
    print(duplicate.df)
    
    # Remove duplicates
    dim(stockcode.df)
    stockcode.df <- stockcode.df[!duplicated(paste0(stockcode.df$collection)),] 
    dim(stockcode.df)
    
    # Make all_pops into a dataframe for merging
    all_pops.df  <- as.data.frame(all_pops, stringsAsFactors = FALSE)
    
    # confirm that all data is present and equal
    if(length(intersect(x = all_pops.df$all_pops, y = stockcode.df$collection))==length(unique(all_pops.df$all_pops))){
      print("OK, all data from your data is present in the stock code file, continue...")
    }else{
      print("Not all pops in your data are in the stock code file, crashing...")
      stop()
    }
    
    #### TODO ####
    #HACK to solve issue of multiple collection IDs the same:
    # May not be needed anymore?
    if(species=="sockeye"){
      
      # Remove the section of the stock code that has the remove from baseline statement:
      stockcode.df <- stockcode.df[stockcode.df$repunit!="Remove from Baseline",]
      
    }
    
    # Append the region code to the back of pop names
    all_pops_w_region.df <- merge(x = all_pops.df, y = stockcode.df, by.x = "all_pops", by.y = "collection", all.x = TRUE, sort = F) # CRITICAL THAT SORT IS FALSE
    head(all_pops.df)
    head(all_pops_w_region.df)
    
    all_pops_w_region.df$add_region <- paste0(all_pops_w_region.df$all_pops, "_", all_pops_w_region.df$repunit)
    
    # Rename pop value in object
    pop(data) <- all_pops_w_region.df$add_region
    
    print("Your populations are now named:   ")
    print(unique(pop(data)))
    
  }

  print("Your data will be output as obj_renamed")
  
  assign(x = "obj_renamed", value = data, envir = .GlobalEnv)

}
  