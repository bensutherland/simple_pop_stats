# Compare physical distance to genetic distance
# Requires two files:
# 1. physical distance file: 03_results/physical_distance.txt
## ...from: calculate_physical_dist()
# 2. genetic distance file: set in argument for function
## ...from: calculate_FST()

compare_phys_genet_dist <- function(FST_file = NULL, phys_dist_file = NULL, highlight_pops = "FALSE", highlight_selection = NULL){
  
  # Set input filenames
  phys_dist.FN  <- phys_dist_file
  genet_dist.FN <- FST_file
  
  # Set output filenames
  output_fig.FN <- paste0(result.path, "pairwise_fst_v_physical_dist.pdf")
  
  # Read in FST data
  print("Reading in FST result file")
  fst <- read.delim2(file = genet_dist.FN, header = T, sep = ",", row.names = 1, stringsAsFactors = F)
  head(fst) # note: issue w/ '-' transferred to '.'
  
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
  
  # Data cleanup; correct all spaces with underscores
  fst_data.df$ref_loc_all <- gsub(pattern = " |\\.", replacement = "_", x = fst_data.df$ref_loc_all, perl = T)
  fst_data.df$comp_loc_all <- gsub(pattern = " |\\.", replacement = "_", x = fst_data.df$comp_loc_all, perl = T)
  
  # Convert to numeric
  fst_data.df$fst_all <- as.numeric(fst_data.df$fst_all)
  str(fst_data.df)
  fst_data.df$comparison <- paste0(fst_data.df$ref_loc_all, "_v_", fst_data.df$comp_loc_all)
  
  # Retain records with no NAs (will remove same-on-same comparison)
  fst_data.df <- fst_data.df[complete.cases(fst_data.df), ]
  
  ## Remove duplicate comparisons
  # Create vector with ordered contrasts
  for(r in 1:nrow(fst_data.df)){
    
  fst_data.df$ordered_comp[r]  <- paste0(
        min(c(fst_data.df$ref_loc_all[r], fst_data.df$comp_loc_all[r]))
      , "__"
      , max(c(fst_data.df$ref_loc_all[r], fst_data.df$comp_loc_all[r]))
  )
    
  }
  
  # Mark duplicates for removal
  fst_data.df$duplicates <- duplicated(fst_data.df$ordered_comp)
  
  fst_data.df <- fst_data.df[which(fst_data.df$duplicates=="FALSE") , ]
  
  fst_data.df <- fst_data.df[,c("fst_all", "comparison")] # only keep necessary cols
  head(fst_data.df)
  
  # Find the appropriate Y-axis for text in plot below
  fst_max <- max(fst_data.df$fst_all)
  print(paste0("The maximum FST val for plotting is FST = ", round(fst_max, digits = 3)))
  
  
  ### Combine physical dist and Fst dist ###
  # Load physical distance data
  distance_full.df <- read.delim2(file = phys_dist.FN, header = T, sep = "\t", stringsAsFactors = F)
  
  # Data cleanup: correct all spaces or dots with underscores
  distance_full.df$comparison <- gsub(pattern = " |\\.", replacement = "_", x = distance_full.df$comparison, perl = T)
  
  # Convert to numeric
  distance_full.df$dist.m <- as.numeric(distance_full.df$dist.m)
  distance_full.df$dist.km <- distance_full.df$dist.m / 1000
  
  # Find the appropriate X-axis for text in plot below
  dist_max <- max(distance_full.df$dist.km)
  print(paste0("The maximum distance val for plotting is ", round(dist_max, digits = 0), " km"))
  
  # Identify whether any contrasts are missing from either 
  print("The following contrasts are missing from either the phys.dist or genet.dist")
  print(setdiff(x = fst_data.df$comparison, y = distance_full.df$comparison))
  
  # Combine
  print("Combining physical and genetic distance values")
  all_data.df <- merge(x = fst_data.df, y = distance_full.df, by = "comparison")
  
  # Make column for colouring
  all_data.df$colour <- rep("black", times = nrow(all_data.df))
  
  # Make backup
  #all_data.df.bck <- all_data.df 
  #all_data.df <- all_data.df.bck
  
  # Are specific pops being coloured?
  if(highlight_pops=="TRUE"){
    
    row_IDs <- NULL; row_IDs_all <- NULL
    
    for(i in 1:length(highlight_selection)){
      
    row_IDs  <- grep(pattern = highlight_selection[i], x = all_data.df$comparison)
    row_IDs_all <- c(row_IDs_all, row_IDs)
      
    }
    
    row_IDs_all <- as.numeric(row_IDs_all)
    
    # Add red colour at the specific locs
    all_data.df$colour[row_IDs_all] <- "red"
    
  }
  
  
  # Plot
  pdf(file = output_fig.FN, width = 6, height = 6)
  plot(x = all_data.df$dist.km, y = all_data.df$fst_all
       , xlab = "Physical Distance (km)"
       , ylab = "Pairwise Fst"
       , las = 1
       , col = all_data.df$colour
       , pch = 19
       )
  
  # mod for adjusted Rsquared
  mod <- lm(all_data.df$fst_all ~ all_data.df$dist.km)
  summary(mod)
  abline(mod)
  
  text(    x = (dist_max - (dist_max * 0.8))
         , y = (fst_max - (fst_max * 0.05))
       , labels = paste0("adj.rsquared = ", round(summary(mod)$adj.r.squared, digits = 3))
  )
  dev.off()
  
  # Also plot with highlight pops gone
  if(highlight_pops=="TRUE"){
    
    # Drop highlight pop rows
    all_data_limited.df <- all_data.df[all_data.df$colour=="black",]
    output_fig_special.FN <- gsub(x = output_fig.FN, pattern = "\\.pdf", replacement = "_no_highlight_pops.pdf")
    
    pdf(file = output_fig_special.FN, width = 6, height = 6)
    plot(x = all_data_limited.df$dist.km, y = all_data_limited.df$fst_all
         , xlab = "Physical Distance (km)"
         , ylab = "Pairwise Fst"
         , las = 1
         , col = all_data_limited.df$colour
         , pch = 19
    )
    
    # mod for adjusted Rsquared
    mod <- lm(all_data_limited.df$fst_all ~ all_data_limited.df$dist.km)
    summary(mod)
    abline(mod)
    
    text(    x = (dist_max - (dist_max * 0.8))
             , y = (fst_max - (fst_max * 0.05))
             , labels = paste0("adj.rsquared = ", round(summary(mod)$adj.r.squared, digits = 3))
    )
    dev.off()
  
    
      
  }
  
  
  
  
}