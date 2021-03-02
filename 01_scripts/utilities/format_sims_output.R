# Join the sims output and the repunit for cleanup and sorting
format_sims_output <- function(sims.FN = NULL, repunit = NULL){

  # Reporting
  print(paste0("Merging sims output with repunit file for ", species))
  
  # Set repunit filename
  repunit.FN <- paste0("H:/Stock_Codes/", species, "/repunits_full.txt")
  
  # Read in repunits file
  repunit.df <- read.delim2(file = repunit.FN, header = T, sep = "\t", stringsAsFactors = FALSE)
  #head(repunit.df)
  
  # Manually read in sims file (function depends windows or linux)
  print("Select your sims (stats) output from file...")
  
  if(.Platform$OS.type == "unix") {
    
    sims.FN <- tk_choose.files(caption = "Select a sims 100 stats file" )
    
  } else if(.Platform$OS.type == "windows") {
    
    sims.FN <- choose.files(caption = "Select a sims 100 stats file")
    
  }
  
  sims.df <- read.delim2(file = sims.FN, header = T, sep = "\t", stringsAsFactors = FALSE)
  #head(sims.df)
  
  # Merge and make dataframe
  all_data <- merge(x = repunit.df, y = sims.df, by = "repunit", all.y = T)
  all_data.df  <- as.data.frame(x = all_data, stringsAsFactors = FALSE)
  #head(all_data.df)
  #str(all_data.df)
  
  # Sort by display order, then by ascending repunit assignment
  print("Sorting by display order, then by repunit assignment percentage")
  all_data.df <- all_data.df[order(all_data.df$Display_Order, all_data.df$repunit_post_mean_pi),]
  head(all_data.df)
  
  # Create output fn
  output.FN <- basename(sims.FN)
  output.FN <- gsub(pattern = "\\.txt", replacement = "_sorted.txt", x = output.FN)
  
  # Write output
  print(paste0("Writing result to ", output.FN))
  write.table(x = all_data.df, file = paste0(result.path, output.FN)
              , quote = FALSE, sep = "\t", col.names = T, row.names = F)
  
  
}
