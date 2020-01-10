# Combine an allele frequency dataframe built by calculate_allele_freq() with the hotspot file to bring in the actual alleles
# Note: requires that the hotspot file was used to create all of the genetic data from MGL_GSI_SNP

build_allele_freq_table <- function(freq_file = freq.df, include_alleles = TRUE){
  
  # Read in current hotspot file for ref and alt alleles
  hotspot.df <- read.delim2(file = hotspot.file, stringsAsFactors = F)

  # Remove the top row (novel SNP field)  
  hotspot.df <- hotspot.df[grep(pattern = "Novel", x = hotspot.df$Origin, invert = T), ]
  dim(hotspot.df)
  
  # Limit df to only the necessary columns
  hotspot.df <- hotspot.df[,c("Allele.Name", "Ref", "Variant")]
  head(hotspot.df)
  
  # Reformat to the same format as the freq_file
  tall_alleles <- rep(x = hotspot.df$Allele.Name, each = 2)
  suffix <- rep(x = c(".01", ".02"), times = nrow(hotspot.df))
  
  # Data checking
  head(tall_alleles)
  head(suffix)
  length(tall_alleles)
  length(suffix)
  
  # Combine
  tall_alleles <- paste0(tall_alleles, suffix)
  head(tall_alleles)
  
  # Interleave alleles
  true_alleles <- c(rbind(hotspot.df$Ref, hotspot.df$Variant))
  
  # Combine
  data.df <- cbind(tall_alleles, true_alleles)
  data.df <- as.data.frame(data.df, stringsAsFactors = FALSE)
  head(data.df)
  colnames(data.df) <- c("mname", "allele")
  
  # NOTE: there may be a better way to do that, but this seems to work
  
  # Combine with the allele freq table
  #head(freq_file)
  freq_file$mname <- rownames(freq_file)
  all_data.df <- merge(x = freq_file, y = data.df, by = "mname")
  
  head(all_data.df)
  
  # Put into order
  all_data.df <- dplyr::select(all_data.df, mname, allele, everything())
  head(all_data.df)
  
  # Assign
  print("Your data is in the obj freq_and_alleles.df")
  assign(x = "freq_and_alleles.df", value = all_data.df, envir = .GlobalEnv)
  
  # Write out
  write.table(x = freq_and_alleles.df, file = "03_results/freq_and_alleles.txt", sep = "\t", quote = F, row.names = F, col.names = T)
  
  }