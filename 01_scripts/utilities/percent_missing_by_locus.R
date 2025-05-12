# Calculate per locus number and percent missing data
# B. Sutherland, SBIO/VIU (2024-09-26)

percent_missing_by_locus <- function(df = obj){
  
  # Reporting
  print("Calculating missing data per locus in input genind object")
  
  print(paste0("There are ", nInd(df), " indivs and ", nLoc(df), " loci in the input genind object"))
  
  # Convert to df from genind, genotype is one col per locus
  obj.df <- genind2df(df)
  
  # Drop population column
  obj.df <- obj.df[, grep(pattern = "pop", x = colnames(obj.df), invert = T)] # remove pop col
  obj.df[1:5,1:5]
  
  obj.df <- t(obj.df) # transpose
  obj.df <- as.data.frame(obj.df)
  obj.df[1:5,1:5]     # cols = inds; rows = loci
  
  # Create collector df
  missing_data_loci.df <- rownames(obj.df)
  missing_data_loci.df <- as.data.frame(missing_data_loci.df)
  colnames(missing_data_loci.df) <- "locus.id"
  head(missing_data_loci.df)
  
  # Calculate number missing
  missing_data_loci.df$num.missing    <- rowSums(is.na(obj.df))
  head(missing_data_loci.df)
  
  # Record number individuals (genotypes) present
  missing_data_loci.df$num.inds.typed <- ncol(obj.df)
  
  # Calculate percentage missing
  missing_data_loci.df$perc.missing   <- missing_data_loci.df$num.missing / missing_data_loci.df$num.inds.typed
  
  # Write out results to file
  print("Writing out missing data per locus to file as '03_results/missing_data_per_locus.csv'")
  write.csv(x = missing_data_loci.df, file = paste0(result.path, "missing_data_per_locus.csv"), row.names = F)
  
  # Save out results to the global environment
  print("The output object will be saved as 'missing_data_loci.df'")
  assign(x = "missing_data_loci.df", value = missing_data_loci.df, envir = .GlobalEnv)
  
}
