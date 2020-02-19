# NOTE: microsat assumes you have "00_archive/euStockCodes_microsat.txt"
# in the tab-delim format of: 
#collection	repunit
#12Mile_Creek	GoA

annotate_rubias <- function(two_allele_data = two_allele_data, sample_type = sample_type){
  
  #### Adding non-genetic columns #####
  
  # Create vectors to add to the data
  sample_type.vec <- rep(x = sample_type, times = nrow(two_allele_data))
  indiv.vec <- rownames(x = two_allele_data)
  
  # Use stock code info to get collection name and repunit; depends if microsat or SNP
  print("Using stock code file to collect names and repunits")
  if(datatype == "SNP"){
    
    # Remove everything after the first underscore to get the stock code (if in MGL_GSI_SNP format)
    pop_code <- gsub(pattern = "\\_.*", replacement = "", x = indiv.vec)
    pop_code <- as.data.frame(pop_code, stringsAsFactors = FALSE)
    sc.df <- read.delim2(file = sc.base, header = TRUE, stringsAsFactors = F) # TODO: if mix, do X
    pop_code <- merge(x = pop_code, y = sc.df, by.x = "pop_code", by.y = "Code", sort = F, all.x = TRUE)
    # Create vectors
    collection.vec <- pop_code$collection
    repunit.vec <- pop_code$repunit
    
  }else if(datatype == "microsat"){
    
    # Take everything up to the first set of four numbers (year)
    #collection.vec <- gsub(pattern = "\\_[0-9]+\\_.*", replacement = "", x = indiv.vec)
    collection.vec <- gsub(pattern = "\\_[0-9][0-9][0-9][0-9]\\_.*", replacement = "", x = indiv.vec)
    collection.vec <- as.data.frame(collection.vec)
    #microsat.sc.FN <- paste0("00_archive/", two.letter.code, "StockCodes_microsat.txt")
    microsat.sc.FN <- paste0(current.path, "/00_archive/", two.letter.code, "StockCodes_microsat.txt")
    sc.df <- read.delim2(file = microsat.sc.FN, header = T, sep = "\t")
    
    #### TODO ####
    #HACK to solve issue of multiple collection IDs the same:
    if(species=="sockeye"){
      
      # Remove the section of the stock code that has the remove from baseline statement:
      sc.df <- sc.df[sc.df$repunit!="Remove from Baseline",]
      
    }
    
    sc.df <- merge(x = collection.vec, y = sc.df, by.x = "collection.vec", by.y = "collection", sort = F)
    repunit.vec <- sc.df$repunit
    
  }
  
  
  # Add these together followed by genetic data
  all_data.df <- cbind(sample_type.vec, collection.vec, repunit.vec, indiv.vec, two_allele_data)
  colnames(all_data.df)[colnames(all_data.df)=="sample_type.vec"] <- "sample_type"
  colnames(all_data.df)[colnames(all_data.df)=="collection.vec"] <- "collection"
  colnames(all_data.df)[colnames(all_data.df)=="repunit.vec"] <- "repunit"
  colnames(all_data.df)[colnames(all_data.df)=="indiv.vec"] <- "indiv"
  
  
  assign(x = "all_data.df", value = all_data.df, envir = .GlobalEnv)
  
  # Create filename
  FN <- paste0(result.path, "rubias_output_", datatype, ".txt")
  
  write.table(x = all_data.df, file = FN, sep = "\t", row.names = FALSE)
  
}