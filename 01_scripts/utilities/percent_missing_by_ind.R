# Calculate per sample percent missing data
#  note: assumes that missing data is encoded as NA in the genind obj

percent_missing_by_ind <- function(df = obj){
  
  # How many markers are present? (this becomes the total used below for calc % missing)
  n.markers <- nLoc(df)
  
  # Reporting
  print(paste0("The input genind has ", n.markers, " markers."))
  print("Per sample, now determining total markers typed and percent missing.")
  
  # Convert genind to df
  obj.df <- genind2df(df)
  obj.df[1:5,1:5]
  
  # Create a tally df to summarize per indiv
  tally.df <- matrix(data = NA, nrow = nInd(df), ncol = 5)
  colnames(tally.df) <- c("ind", "pop", "tot.markers.present", "ind.num.typed", "ind.per.missing")
  tally.df <- as.data.frame(tally.df)
  
  # Retain number markers, sample id and pop
  tally.df$tot.markers.present <- n.markers # record the number of markers in dataset for output
  tally.df$ind <- rownames(obj.df)
  tally.df$pop       <- obj.df$pop
  
  # Then remove the pop column to not disrupt tallies
  obj.df <- obj.df[, grep(pattern = "pop", x = colnames(obj.df), invert = T)] # Drop the 'pop' column
  
  # Reporting
  print(paste0("Evaluating ", ncol(obj.df), " loci"))
  print(paste0("Evaluating ", nrow(obj.df), " individuals."))
  
  # Calculate per individual the percentage of missing data, and the total number of typed markers
  # Note: missing data are NA in this case, as result of genind2df()
  for(i in 1:(nrow(obj.df))){
    
    # Fill in the vectors in the tally df
    tally.df[i, "ind.num.typed"]   <- sum(!is.na(obj.df[i,]))            # number typed (not missing)
    tally.df[i, "ind.per.missing"] <- sum(is.na(obj.df[i,])) / n.markers # percent missing
    
  }
  
  
  # Provide summaries
  print(paste0("Per individual (n = ", nrow(tally.df), ") proportion missing data:"))
  print(summary(tally.df$ind.per.missing))
  print(paste0("sd: ", round(sd(tally.df$ind.per.missing), digits = 7)))
  
    # Write out results to file
  print(paste0("Output written to ", paste0(result.path, "missing_data_per_indiv.csv")))
  write.csv(x = tally.df, file = paste0(result.path, "missing_data_per_indiv.csv"), row.names = F)
  
  # Save out results to the global environment
  print("Output saved in enviro as 'missing_data.df'")
  assign(x = "missing_data.df", value = tally.df, envir = .GlobalEnv)
  
}
