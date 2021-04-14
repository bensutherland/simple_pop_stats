hwe_eval <- function(data = obj, alpha = 0.01){
  
  # Sourced from: https://grunwaldlab.github.io/Population_Genetics_in_R/Locus_Stats.html
  
  
  # Run a HWE test on each population in the genepop (pegas)
  # Tests the hypothesis that genotype frequencies follow HWE
  hwe.dat <- seppop(data) %>% lapply(hw.test, B = 0) # B is for permutations
  # Currently only runs classical chi^2 test based on the expected genotype frequencies calculated from the allelic frequencies
  # Expect warnings on too much missing data
  
  # Produces a list per population, with the following structure
  # > head(hwe.dat[[1]])
  # chi^2 df Pr(chi^2 >)
  # CMRDDFW_10186  0.01373205  1 0.906714386
  # CMRDDFW_112    0.29203727  1 0.588917945
  # CMRDDFW_112379 8.23822206  1 0.004101729
  # 
  
  # Reporting
  print(paste0("There are a total of ", length(hwe.dat), " populations in your HWE test"))
  
  # Subset using sapply to take the third column with all rows ("[" is used here to subset data and it is also a function)
  hwe.mat <- sapply(hwe.dat, "[", i = TRUE, j = 3)
  
  # Reporting
  print("Summarizing for the following number of markers (rows) and populations (columns): ")
  print(dim(hwe.mat))
  print("p-values retained in a matrix, a subset shown below")
  hwe.mat[1:5,1:2]
  
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
