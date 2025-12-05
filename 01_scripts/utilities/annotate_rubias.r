# NOTE: microsat assumes you have "00_archive/euStockCodes_microsat.txt"
# in the tab-delim format of: 
#collection	repunit
#12Mile_Creek	GoA

annotate_rubias <- function(two_allele_data = two_allele_data
                            , sample_type = sample_type
                            , micro_stock_code.FN = micro_stock_code.FN
                            , pop_map.FN = pop_map.FN
                            , datatype = datatype){
  
  #### 01. Read in required data ####
  
  ## Read in stock code file (all datasets)
  sc.df <- NULL
  if(sample_type=="reference"){
    
    sc.df <- read.delim2(file = micro_stock_code.FN, header = T, sep = "\t", stringsAsFactors = F)
    
  }
  
  print("Stock code file has been read in.")
  print(head(sc.df))
  
  
  #### 02. Add non-genetic columns ####
  # Create vectors to add to the data
  sample_type.vec <- rep(x = sample_type, times = nrow(two_allele_data))
  indiv.vec <- rownames(x = two_allele_data)
  
  # Use stock code info to get collection name and repunit; depends if microsat or SNP
  print("Using stock code file to collect names and repunits")
  
  if(datatype == "SNP"){
    
    # Remove everything after the first underscore to get the stock code
    pop_code <- gsub(pattern = "\\_.*", replacement = "", x = indiv.vec)
    pop_code <- as.data.frame(pop_code, stringsAsFactors = FALSE)
    sc.df <- read.delim2(file = sc.base, header = TRUE, stringsAsFactors = F) # TODO: if mix, do X
    pop_code <- merge(x = pop_code, y = sc.df, by.x = "pop_code", by.y = "Code", sort = F, all.x = TRUE)
    
    # Create vectors
    collection.vec <- pop_code$collection
    repunit.vec <- pop_code$repunit
    
    
  }else if(datatype == "microsat"){
    
    # In MGL_GSI_SNP format
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
  all_data.df[1:5,1:9]
  colnames(all_data.df)[colnames(all_data.df)=="sample_type.vec"] <- "sample_type"
  colnames(all_data.df)[colnames(all_data.df)=="collection.vec"] <- "collection"
  colnames(all_data.df)[colnames(all_data.df)=="repunit.vec"] <- "repunit"
  colnames(all_data.df)[colnames(all_data.df)=="indiv.vec"] <- "indiv"

  ## Data check
  print("Confirming that the output individual names are matched and in the same order as the input genotype df, for all samples")
  print(table(all_data.df$indiv==rownames(two_allele_data)))
  
  ### TODO: should add a data check based on the above here with a stop command if the original order (i.e., two_allele_data)
  ###         doesn't match the output order (i.e., all_data.df)
  
  assign(x = "rubias_data.df", value = all_data.df, envir = .GlobalEnv)
  
  # Create filename
  FN <- paste0(result.path, "rubias_output_", datatype, ".txt")
  
  write.table(x = all_data.df, file = FN, sep = "\t", row.names = FALSE)
  
}

