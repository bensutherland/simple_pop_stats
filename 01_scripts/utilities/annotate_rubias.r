# NOTE: microsat assumes you have "00_archive/euStockCodes_microsat.txt"
# in the tab-delim format of: 
#collection	repunit
#12Mile_Creek	GoA

# NOTE: custom_format also requires a population-repunit interpretation file, AND
# NOTE: custom_format = FALSE ; requires "02_input_data/my_data_ind-to-pop_annot.txt", a tab-delim file in form of: 
#indiv  pop
#101  JPN
#1847 FRA

annotate_rubias <- function(two_allele_data = two_allele_data, sample_type = sample_type, micro_stock_code.FN = micro_stock_code.FN, custom_format = custom_format){
  
  # Interactive read in stock name to repunit conversion (for use later)
  # Select stock code file
  # micro_stock_code.FN <-  choose.files(default=file.path("03_results/"), caption = "Select microsatellite stock code file (tab delim)")
  if(sample_type=="reference"){
    sc.df <- read.delim2(file = micro_stock_code.FN, header = T, sep = "\t", stringsAsFactors = F)
  }
  
  #### Adding non-genetic columns #####
  
  # Create vectors to add to the data
  sample_type.vec <- rep(x = sample_type, times = nrow(two_allele_data))
  indiv.vec <- rownames(x = two_allele_data)
  
  # Use stock code info to get collection name and repunit; depends if microsat or SNP
  print("Using stock code file to collect names and repunits")
  if(datatype == "SNP"){
    
    # If in MGL_GSI_SNP format
    if(custom_format == FALSE){
      
      # Remove everything after the first underscore to get the stock code
      pop_code <- gsub(pattern = "\\_.*", replacement = "", x = indiv.vec)
      pop_code <- as.data.frame(pop_code, stringsAsFactors = FALSE)
      sc.df <- read.delim2(file = sc.base, header = TRUE, stringsAsFactors = F) # TODO: if mix, do X
      pop_code <- merge(x = pop_code, y = sc.df, by.x = "pop_code", by.y = "Code", sort = F, all.x = TRUE)
      
    # If not in MGL_GSI_SNP format, it is necessary to provide an interpretation file
    }else if(custom_format == TRUE){
      
      # Create dataframe with individual names
      pop_code <- as.data.frame(indiv.vec, stringsAsFactors = FALSE)
      
      # Read in the samplename to pop interpretation file
      sample_to_pop_interp.df <- read.table(file = "02_input_data/my_data_ind-to-pop_annot.txt"
                                   , header = T, sep = "\t"
                                   #, quote = F
      )
      
      # Attach the pops to the individuals
      pop_code <- merge(x = pop_code, y = sample_to_pop_interp.df, by.x = "indiv.vec", by.y = "indiv", all.x = T, sort = F)
      
      # Attach the repunits to the pops
      pop_code <- merge(x = pop_code, y = sc.df, by.x = "pop", by.y = "collection", all.x = T, sort = F)
      
      head(pop_code)
      colnames(pop_code) <- c("collection", "ind", "repunit")
      pop_code <- pop_code[, c("ind", "collection", "repunit")]
      
    }
    
    # Create vectors
    collection.vec <- pop_code$collection
    repunit.vec <- pop_code$repunit
    
    
  }else if(datatype == "microsat"){
    
    # Identify stock name per individual
    # Take everything up to the first set of four numbers (year)
    collection.vec <- gsub(pattern = "\\_[0-9][0-9][0-9][0-9]\\_.*|\\_[0-9][0-9][0-9]\\_.*", replacement = "", x = indiv.vec)
    collection.vec <- as.data.frame(collection.vec)
    
    #### TODO ####
    #HACK to solve issue of multiple collection IDs the same:
    if(species=="sockeye"){
      
      # Remove the section of the stock code that has the remove from baseline statement:
      #sc.df <- sc.df[sc.df$repunit!="Remove from Baseline",]
      
      # Alternate method
      sc.df[grep(pattern = "Remove", x = sc.df$repunit, ignore.case = T, invert = T), ]
      
    } else if (species == "chum"){
      # Alternate method
      sc.df <- sc.df[grep(pattern = "Remove", x = sc.df$repunit, ignore.case = T, invert = T), ]
      sc.df <- sc.df[grep(pattern = "outofdata", x = sc.df$repunit, ignore.case = T, invert = T), ]
      
    }
    
    # dim(collection.vec)
    # temp.sc <- merge(x = collection.vec, y = sc.df, by.x = "collection.vec", by.y = "collection", sort = F)
    # dim(temp.sc)
    # combined_stock_and_repunit <- paste0(temp.sc$collection.vec, "--", temp.sc$repunit)
    # combinations <- unique(combined_stock_and_repunit)
    # write.csv(x = combinations, file = "03_results/combinations.csv")
    # repunit.vec <- temp.sc$repunit
    # 
    
    if (sample_type=="reference"){
      sc.df <- merge(x = collection.vec, y = sc.df, by.x = "collection.vec", by.y = "collection", sort = F)
    } else {
      sc.df <- as.data.frame(collection.vec)
      sc.df$repunit <- NA
    }
    
    
    # Data checking: 
    if(nrow(collection.vec)==nrow(sc.df)){
      
      print("All input stock names were found in the stock code file, continuing...")
      
    }else{
      
      stop("At least one of your input file stock names was not found in your stock code file OR (occasionally AND) your stock code file has multiple stock codes for the same collection name stopping...")
      
    }
    
    repunit.vec <- sc.df$repunit

  }
  
  
  # Reporting
  print("Completed all merging, building full rubias file")
  
  # Add these together followed by genetic data
  all_data.df <- cbind(sample_type.vec, collection.vec, repunit.vec, indiv.vec, two_allele_data)
  colnames(all_data.df)[colnames(all_data.df)=="sample_type.vec"] <- "sample_type"
  colnames(all_data.df)[colnames(all_data.df)=="collection.vec"] <- "collection"
  colnames(all_data.df)[colnames(all_data.df)=="repunit.vec"] <- "repunit"
  colnames(all_data.df)[colnames(all_data.df)=="indiv.vec"] <- "indiv"

  assign(x = "rubias_data.df", value = all_data.df, envir = .GlobalEnv)
  
  # Create filename
  FN <- paste0(result.path, "rubias_output_", datatype, ".txt")
  
  write.table(x = all_data.df, file = FN, sep = "\t", row.names = FALSE)
  
}
