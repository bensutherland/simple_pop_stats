## Prepare a proportions file
# proportions.FN should be a tab-delim file with header collections \t ppn
prep_ppn <- function(rubias_base.FN = "03_results/rubias_output.txt"
                     , ppn.FN = "ppn.txt"){
  
  print(paste0("Using this file to create a ppn template: ", rubias_base.FN))
  
  # Read in the rubias base
  rubias_base <- read_tsv(rubias_base.FN, guess_max=100000)
  
  # Create ppn file
  ppn.df <- as.data.frame(x = unique(rubias_base$collection)
                   , stringsAsFactors = F
                   )
  
  # Add dummy ppn vals
  ppn.df <- cbind(ppn.df, rep(x = 0, times = nrow(ppn.df)))
  
  # Give colnames
  colnames(ppn.df) <- c("collection", "ppn")
  
  # Reporting
  print(ppn.df)
  
  # Set output fn
  out.FN <- paste0(result.path, ppn.FN)
  
  # Inform user
  print("Go to the following location and manually edit your file: ")
  print(out.FN)
  print("Adjust the proportion column to equal 1.0 total")
  
  # Save ppn file
  write_delim(x = ppn.df, path = out.FN, delim = "\t", col_names = T)
  
}
