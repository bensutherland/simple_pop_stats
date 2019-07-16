# Calculate the distance among points

calculate_physical_dist <- function(){
  
  # Load stock code file
  stock_codes.df <- read.delim2(file = sc.base, stringsAsFactors = F)
  
  # Convert long and lat to numeric values
  stock_codes.df$XLONG <- as.numeric(stock_codes.df$XLONG)
  stock_codes.df$YLAT <- as.numeric(stock_codes.df$YLAT)

  # Retain locations that do not have NA vals
  table(complete.cases(stock_codes.df))
  print("The following locations have NA vals in the stock code file: ")
  stock_codes.df[ complete.cases(stock_codes.df) == F, ]
  print("Dropping stock codes that contain NA records")
  stock_codes.df <- stock_codes.df[complete.cases(stock_codes.df), ]


  #### 1. identify distance between points #####
  # Set nulls
  distance.df <- NULL; this_dist <- NULL; cumul_dist <- NULL; cumul_loc <- NULL
  loc1 <- NULL ; loc2 <- NULL
  distance_full.df <- NULL; report_contrast <- NULL

  # Find out the distance between stock code locations for all pairs of comparisons
  print("Finding pairwise physical distance between all stock code locations")
  for(i in 1:nrow(stock_codes.df)){
  
    loc1 <- stock_codes.df$collection[i]
    print(paste0("Comparing all against: ", loc1))
  
    # Compare all stock locations against loc1
    for(j in 1:nrow(stock_codes.df)){
    
      # Identify the stock to be compared
      loc2 <- stock_codes.df$collection[j]
      print(loc2)
    
      # What is the distance for this comparison?
      this_dist <- distm(x = c(stock_codes.df$XLONG[which(stock_codes.df$collection==loc1)]
                             , stock_codes.df$YLAT[which(stock_codes.df$collection==loc1)])
                       , y = c(stock_codes.df$XLONG[which(stock_codes.df$collection==loc2)]
                               , stock_codes.df$YLAT[which(stock_codes.df$collection==loc2)]
                       )
                    )
    
      report_contrast <- paste0(loc1, "_v_", loc2)
    
      # Add to cumulative distance
      cumul_dist <- c(cumul_dist, this_dist)
      cumul_loc <- c(cumul_loc, report_contrast)
      
      # Clear variables
      rm(report_contrast, this_dist)
    
    }
  
    # Combine physical distance and locations together into df
    distance.df <- as.data.frame(cbind(cumul_loc, as.numeric(cumul_dist)), stringsAsFactors = F)
    colnames(distance.df) <- c("comparison", "dist.m")
    distance.df$dist.m <- as.numeric(distance.df$dist.m)
    
    # Collect everything together into a dataframe
    distance_full.df <- rbind(distance_full.df, distance.df)
    
    # Clear variables
    rm(distance.df)

  }
  
  # Keep only a single record for each contrast
  distance_full.df <- unique(distance_full.df)
  
  # Save out physical distance file
  write.table(x = distance_full.df, file = "03_results/physical_distance.txt"
              , quote = F, sep = "\t", row.names = F, col.names = T)
  
}
