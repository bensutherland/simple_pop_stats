hwe_eval <- function(data = obj, alpha = 0.01){
  
  # Run a HWE test on each population in the genepop using pegas
  # Sourced / adapted from: https://grunwaldlab.github.io/Population_Genetics_in_R/Locus_Stats.html
  
  # Test the hypothesis that genotype frequencies follow HWE
  hwe.dat <- seppop(data) %>% lapply(hw.test, B = 0) # B is for number of reps for Monte Carlo, for regular HW test, set B = 0
  # chi^2 test based on expected genotype frequencies calculated from the allelic frequencies
  # Expect warnings if too much missing data
  
  # Output is a list with a slot per population; 
  #  slot contains three columns, chi2 val, df, p-val of chi2
  #  marker names are the rownames
   
  # Reporting
  print(paste0("There are a total of ", length(hwe.dat), " populations being evaluated"))
  
  # Extract and write out each population's per locus values to file and to global envir
  popn.name <- NULL; output_per_locus.FN <- NULL; obj_per_locus.FN <- NULL ; popn_hwe.df <- NULL; mname <- NULL
  for(i in 1:length(hwe.dat)){
    
    # obtain pop name
    popn.name <- names(hwe.dat)[i]
    
    # Prepare output FN (text or global env)
    output_per_locus.FN <- paste0("03_results", "/", "per_locus_hwe_", popn.name, ".txt")
    obj_per_locus.FN    <- paste0("per_locus_hwe_", popn.name, ".df")
    
    # Collect df from list
    popn_hwe.df <- as.data.frame(hwe.dat[[i]])
    
    # Add marker names
    mname       <- rownames(popn_hwe.df)
    popn_hwe.df <- cbind(mname, popn_hwe.df)
    
    # Save out result
    write_delim(x = popn_hwe.df, file = output_per_locus.FN, delim = "\t", col_names = T)
    
    # Assign to global env for further use
    assign(x = obj_per_locus.FN, value = popn_hwe.df, envir = .GlobalEnv)
    
    print(paste0("Per locus hwe p-values have been saved to the GlobalEnv as: ", obj_per_locus.FN))
    
  }
  
  # Subset using sapply to take the third column with all rows ("[" is used here to subset data and it is also a function)
  hwe.mat <- sapply(hwe.dat, "[", i = TRUE, j = 3)
  
  # Reporting
  print("Summarizing for the following number of markers (rows) and populations (columns): ")
  print(dim(hwe.mat))
  print("p-values retained in a matrix, a subset shown below")
  print(head(hwe.mat))
  
  print("Confirm the p-values from a slot of the original list")
  head(hwe.dat[[1]])
  
  # Note, details on handling NA
  # table((obj_hwe.mat[,i] < alpha), useNA = "always")
  
  ## Summarize this into a usable format
  print("Summarizing per-population stats")
  # Set nulls
  non_hw.nmar <- NULL; per.non_hw <- NULL; all_pops.df <- NULL
  
  for(i in 1:ncol(hwe.mat)){
    
    # How many markers are out of hwe? 
    nmar.non_hw <- length(hwe.mat[,i][hwe.mat[,i] < alpha])
    
    # Express number of markers out of HWE as a percentage for those markers non-NA
    per.non_hw <- nmar.non_hw / sum(!is.na(hwe.mat[,i])) * 100
    per.non_hw <- round(x = per.non_hw, digits = 1)
    
    temp <- c(nmar.non_hw, per.non_hw)
    temp <- as.data.frame(t(temp))
    
    colnames(temp) <- c("nmar.non.hw", "per.non.hw")
    rownames(temp) <- colnames(hwe.mat)[i]
    
    all_pops.df <- rbind(all_pops.df, temp)
    
  }
  
  # Reporting
  print(paste0("the alpha value used is ", alpha))
  
  # Add collection as a column
  all_pops.df$collection <- rownames(all_pops.df)
  # Re-order
  all_pops.df <- all_pops.df[,c("collection", "nmar.non.hw", "per.non.hw")]
  
  # Save out result
  write_delim(x = all_pops.df, file = paste0(result.path, "HWE_result_alpha_", alpha, ".txt"), delim = "\t", col_names = T)
  
}
