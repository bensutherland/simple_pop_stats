# Create text file of individual names from genind file and write out for user to manually populate
#  will also simplify amplitools popnames
# Ben J. G. Sutherland, initialized 2023-08-29

generate_popmap <- function(df = obj, format = "amplitools"){
 
  # Warning
  print("Note: this function has only been implemented for amplitools input so far")
  
  # Confirm that format is supported
  if(format=="amplitools"){
    
    print("Individual names are indicated as amplitools format")
    print("Expecting three fields (run, barcode, sample.id), separated by double underscores, i.e., '__'")
    
  }else{
    
    stop("Only amplitools format has been implemented so far")
    
  }
  
  ## Create a df with all included indiv names
  indiv.df <- as.data.frame(indNames(df))
  colnames(indiv.df) <- "indiv"
  
  # Separate components of indiv ID (i.e., run, barcode, sample)
  print("Separating data into: ")
  indiv.df <- separate(data = indiv.df, col = "indiv", into = c("run", "barcode", "indiv"), sep = "__", remove = T)
  print(head(indiv.df))
  
  # Use reduced indiv name as indname in genind
  indNames(df) <- indiv.df$indiv
  
  # How many samples from each run? 
  print("How many samples are present in each run in the genind?")
  print(table(indiv.df$run))
  
  # Prepare to write out a clean text file to add pop attribute per sample
  indiv.df <- as.data.frame(indiv.df[, "indiv"])
  colnames(indiv.df) <- "indiv"
  
  # Add fields to manually complete
  indiv.df$pop    <- NA # population ID
  indiv.df$alt.ID <- NA # alternate sample name
  indiv.df$sex    <- NA # sex (include for potential parents for parentage applications)
  print("The form to complete is as follows: ")
  print(head(indiv.df))
  print("Note: alt.ID and sex columns are optional")

  
  # Reporting
  pop_map.FN <- "02_input_data/my_data_ind-to-pop.txt"
  print(paste0("Writing out the population map file ", pop_map.FN, " to be completed manually."))
  
  # Write out empty file to provide pop names
  write.table(x = indiv.df, file = pop_map.FN
              , sep = "\t", col.names = T, row.names = F
              , quote = F
  )
  
  assign(x = "obj.simplified_names", value = df, pos = .GlobalEnv)
  assign(x = "indiv.df", value = indiv.df, pos = .GlobalEnv)
  
}
