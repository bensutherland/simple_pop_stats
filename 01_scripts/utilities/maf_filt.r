# Calculate MAF and remove markers less than cutoff
# Note: only designed for SNP data

maf_filt <- function(data = obj, maf = 0.01){
 
  # Convert genind to genlight
  obj.gl <- gi2gl(gi = data, parallel = T)
  
  # Calculate frequency of second allele
  myFreq <- glMean(obj.gl)
  
  # Ensure each locus second allele is the minor allele
  for(i in 1:length(myFreq)){
    
    if(myFreq[i] > 0.5){
      
      myFreq[i] <- 1-myFreq[i]
      
    }else{
      
      myFreq[i] <- myFreq[i]
      
    }
    
  }
  
  ## Identify which variants are under the MAF filter
  MAF_rem.vec <- names(myFreq[which(myFreq < maf)])
  print(paste0("Number of variants under MAF cutoff of ", maf, ": ", length(MAF_rem.vec)))
  
  markers_to_keep <- setdiff(x = locNames(data), y = MAF_rem.vec)
  print(paste0("Keeping ", length(markers_to_keep), " variants"))
  
  print("Removing low MAF markers from the genind")
  data <- data[, loc=markers_to_keep]
  
  # Keep AF of only the retained variants
  myFreq <- myFreq[which(myFreq >= maf)]
  length(myFreq)
  
  assign(x = "obj_maf_filt", value = data
         , pos = .GlobalEnv
         )
  assign(x = "myFreq", value = myFreq
         , pos = .GlobalEnv
         )
   
  print("The filtered genind is available as 'obj_maf_filt' and the retained marker frequencies as 'myFreq'")
  
}
  