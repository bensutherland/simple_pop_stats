# Generate per-locus Hobs, Hexp, and Fst from genepop. Output will be 'per_locus_stats_*.txt

per_locus_stats <- function( data = obj ){
  
  # Calculate per-locus Fst
  print("Calculating per-locus FST")
  obj_fst <- Fst(as.loci(data)) # extract the loci from genepop obj and calculate FST per locus
  obj_fst <- as.data.frame(obj_fst)
  obj_fst$mname <- rownames(obj_fst)
  # produces a df (number loci x 3 (Fit, Fst, Fis))
  
  # Additional calculations
  print("Calculating additional per-locus stats")
  obj_sum <- summary(data) # this is a large genind summary, complex obj with names ("n", "n.by.pop", "loc.n.all", "pop.n.all", "NA.perc", "Hobs", "Hexp")
  
  # Obtain hobs
  hobs.df <- as.data.frame(obj_sum$Hobs)
  hobs.df$mname <- rownames(hobs.df)
  colnames(hobs.df) <- c("Hobs", "mname")
  
  # Obtain hexp
  hexp.df <- as.data.frame(obj_sum$Hexp)
  hexp.df$mname <- rownames(hexp.df)
  colnames(hexp.df) <- c("Hexp", "mname")
  
  # Join all per-locus stats
  obj_fst_h <- merge(x = obj_fst, y = hobs.df, by = "mname")
  obj_fst_h <- merge(x = obj_fst_h, y = hexp.df, by = "mname")
  
  # Reduce the numeric variables to four sig figs
  for(i in 2:ncol(obj_fst_h)){
    
    obj_fst_h[,i] <- round(x = obj_fst_h[,i], digits = 4)
    
  }
  
  # Write out the per-locus stats
  print("Writing out to 03_results/per_locus_stats*.txt")
  write.table(obj_fst_h
              , file = paste0("03_results/per_locus_stats_", format(Sys.time(), "%Y-%m-%d"),".txt")
              , quote = F, sep="\t", row.names = F, col.names = T
              )
  
  # Assign to global variable
  print("Saving output to global environment as per_loc_stats.df")
  assign(x = "per_loc_stats.df", value = obj_fst_h, envir = .GlobalEnv)
  
}

