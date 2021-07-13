# Generate per-locus Hobs, Hexp, and Fst from genepop. Output will be 'per_locus_stats_*.txt

per_locus_stats <- function( data = obj ){
  
  # Calculate per-locus Fst
  print("Calculating per-locus FST")
  obj_fst <- Fst(as.loci(obj)) # extract the loci from genepop obj and calculate FST per locus
  # produces a matrix (number loci x 3 (Fit, Fst, Fis))
  
  # Additional calculations
  print("Calculating additional per-locus stats")
  obj_sum <- summary(obj) # this is a large genind summary, complex obj with names ("n", "n.by.pop", "loc.n.all", "pop.n.all", "NA.perc", "Hobs", "Hexp")
  
  # Bring in per locus Hobs to the per locus summary
  obj_fst_h <- cbind(obj_fst, obj_sum$Hobs)
  head(obj_fst_h)
  colnames(obj_fst_h)[ncol(obj_fst_h)] <- "Hobs"
  
  # Bring in per locus Hexp to the per locus summary
  obj_fst_h <- cbind(obj_fst_h, obj_sum$Hexp)
  head(obj_fst_h)
  colnames(obj_fst_h)[ncol(obj_fst_h)] <- "Hexp"

  # obj_fst_h.bck <- obj_fst_h
  # obj_fst_h <- obj_fst_h.bck
  
  # Reduce sig figs
  for(i in 1:ncol(obj_fst_h)){
    print(i)
    
    obj_fst_h[,i] <- round(x = obj_fst_h[,i], digits = 4)
    
  }
  
  # Write out the per-locus stats
  print("Writing out to 03_results/per_locus_stats*.txt")
  write.table(obj_fst_h
              , file = paste0("03_results/per_locus_stats_", format(Sys.time(), "%Y-%m-%d"),".txt")
              , quote = F, sep="\t", row.names = T, col.names = T
              )
}

