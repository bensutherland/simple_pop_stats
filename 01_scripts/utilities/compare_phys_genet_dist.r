# Compare physical distance to genetic distance
# Requires two files:
# 1. physical distance file: 03_results/physical_distance.txt
## ...from: calculate_physical_dist()
# 2. genetic distance file: 03_results/gen_diff_wcfst.csv
## ...from: calculate_FST()

compare_phys_genet_dist <- function(){
  
  # Set input filenames
  phys_dist.FN <- "03_results/physical_distance.txt"
  genet_dist.FN <- "03_results/gen_diff_wcfst.csv"
  
  # Set output filenames
  output_fig.FN <- "03_results/pairwise_fst_v_physical_dist.pdf"
  
  # Read in FST data
  print("Reading in FST result file")
  fst <- read.delim2(file = genet_dist.FN, header = T, sep = ",", row.names = 1, stringsAsFactors = F)
  
  # Format FST data (make numeric)
  print("Formatting FST result file")
  for(i in 1:ncol(fst)){
    fst[,i] <- as.numeric(fst[,i])
  }
  
  ### Collect all info from fst table ###
  print("Collecting info from FST results")
  
  # Set nulls
  row_of_interest <- NULL; col_of_interest <- NULL
  loc1 <- NULL; loc2 <- NULL
  ref_loc_all <- NULL; comp_loc_all <- NULL
  fst_all <- NULL; fst_of_interest <- NULL
  
  # Obtain an FST val for each pairwise comparison
  for(i in 1:nrow(fst)){
    loc1 <- rownames(fst)[i]
    print(paste0("**", loc1))
    
    for(j in 1:ncol(fst)){
      loc2 <- colnames(fst)[j]
      print(loc2)
      
      row_of_interest <- which(rownames(fst)==loc1) 
      col_of_interest <- which(colnames(fst)==loc2)
      
      fst_of_interest <- fst[row_of_interest, col_of_interest]
      
      ref_loc_all <- c(ref_loc_all, loc1)
      comp_loc_all <- c(comp_loc_all, loc2)
      fst_all <- c(fst_all, fst_of_interest)
      
    }
  }
  
  # Combine results and format
  fst_data.df <- cbind(ref_loc_all, comp_loc_all, fst_all)
  fst_data.df <- as.data.frame(fst_data.df, stringsAsFactors = F)
  fst_data.df$fst_all <- as.numeric(fst_data.df$fst_all)
  str(fst_data.df)
  fst_data.df$comparison <- paste0(fst_data.df$ref_loc_all, "_v_", fst_data.df$comp_loc_all)
  
  # Retain records with no NAs (will remove same-on-same comparison)
  fst_data.df <- fst_data.df[complete.cases(fst_data.df), ]
  fst_data.df <- fst_data.df[,c("fst_all", "comparison")] # only keep necessary cols
  head(fst_data.df)
  
  
  ### Combine physical dist and Fst dist ###
  # Load physical distance data
  distance_full.df <- read.delim2(file = phys_dist.FN, header = T, sep = "\t", stringsAsFactors = F)
  distance_full.df$dist.m <- as.numeric(distance_full.df$dist.m)
  distance_full.df$dist.km <- distance_full.df$dist.m / 1000
  
  # Identify whether any contrasts are missing from either 
  print("The following contrasts are missing from either the phys.dist or genet.dist")
  print(setdiff(x = fst_data.df$comparison, y = distance_full.df$comparison))
  
  # Combine
  print("Combining physical and genetic distance values")
  all_data.df <- merge(x = fst_data.df, y = distance_full.df, by = "comparison")
  
  
  # Plot
  
  pdf(file = output_fig.FN, width = 6, height = 6)
  plot(x = all_data.df$dist.km, y = all_data.df$fst_all
       , xlab = "Physical Distance (km)"
       , ylab = "Pairwise Fst"
       , las = 1)
  
  # mod for adjusted Rsquared
  mod <- lm(all_data.df$fst_all ~ all_data.df$dist.km)
  summary(mod)
  abline(mod)
  
  text(x = 400, y = 0.05
       , labels = paste0("adj.rsquared = ", round(summary(mod)$adj.r.squared, digits = 4))
  )
  dev.off()
  
}