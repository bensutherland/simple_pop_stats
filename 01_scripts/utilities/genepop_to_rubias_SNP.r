# Convert genepop data (SNP) to rubias input (where data = a genind file)
genepop_to_rubias_SNP <- function(data = data
                                  , sample_type=sample_type
                                  , custom_format = FALSE
                                  , micro_stock_code.FN = NULL
                                  , pop_map.FN = NULL
                                  , datatype = "SNP"){
  
  print("Converting SNP genepop to rubias format")
  
  # Select out the data component
  data <- data$tab
  #data[1:5,1:7]
  
  # Order columns (alphanumerically)
  data <- data[ , order(colnames(data))]
  #data[1:5,1:7]
  
  # Only retain the first allele (NOTE: differs from microsat approach)
  one_allele_data <- data[, grep(pattern = "\\.01|\\.1", x = colnames(data),perl = T)]
  #one_allele_data[1:5,1:5]
  
  # Note: 
  # It is possible to derive the second allele based on the information of the first allele only, 
  # e.g., first allele = 0, second = 2; first allele = 1, second = 1; first allele = 2, second = 0; missing data is NA
  
  # Convert the genepop code back to homo/het info
  print("Converting from genind first allele to proton format")
  
  for(i in 1:ncol(one_allele_data)){
    
    # Replace based on the number the datatype in torrent format
    one_allele_data[,i] <- gsub(pattern = "0", replacement = "homo.alt", x = one_allele_data[,i])
    one_allele_data[,i] <- gsub(pattern = "1", replacement = "het", x = one_allele_data[,i])
    one_allele_data[,i] <- gsub(pattern = "2", replacement = "homo.ref", x = one_allele_data[,i])
    
    # Replace NAs with 'missing'
    one_allele_data[is.na(one_allele_data[,i]),i] <- "missing"
    
  }
  
  #one_allele_data[1:5,1:5]
  
  # Drop the .01 or .1 in the column name for rubias format
  colnames(one_allele_data) <- gsub(pattern = "\\.01$|\\.1$", replacement = "", perl = T, x = colnames(one_allele_data))
  #one_allele_data[1:5,1:5]
  
  # Convert from torrent format to rubias format
  print("Converting from proton format to rubias format")
  
  for(i in 1:ncol(one_allele_data)){
    
    one_allele_data[,i] <- ifelse(test = one_allele_data[,i]=="homo.ref", yes = "1 1"
                                  , no = ifelse(test = one_allele_data[,i]=="het", yes = "1 2"
                                  , no = ifelse(test = one_allele_data[,i]=="homo.alt", yes = "2 2"
                                  , no = ifelse(test = one_allele_data[,i]=="missing", yes = "NA NA"
                                  , no = "NA NA"
                                                              ))))
    
  }
  
  #one_allele_data[1:5,1:5]
  
  # Convert the new table to df
  one_allele_data <- as.data.frame(one_allele_data, stringsAsFactors = F)
  
  # Identify column names
  colnames_to_change <- colnames(one_allele_data) # this way allows expansion of columns without disrupting incremental process
  
  # Split the df into two cols, with colnames for markers in rubias format
  colname1 <- NULL; colname2 <- NULL
  for(i in 1:length(colnames_to_change)){
    
    # Set colnames for the column
    colname1 <- colnames_to_change[i]  # just the marker
    colname2 <- paste0(colname1, "_1") # marker plus '_1'
    
    # Separate the data
    one_allele_data <- separate(data = one_allele_data
                                , col = which(colnames(one_allele_data)==colname1)
                                , into = c(colname1, colname2)
                                , sep = " "
                                , remove = T)
    
  }
  
  #one_allele_data[1:5,1:10]
  
  # Once the loop is done, can rename object to the more correct 'two_allele_data'
  two_allele_data <- one_allele_data
  rm(one_allele_data)
  
  # make df
  two_allele_data <- as.data.frame(x = two_allele_data, stringsAsFactors = FALSE)
  dim(two_allele_data)
  #two_allele_data[1:5,1:10]
  
  #### Adding non-genetic columns #####
  print("Adding non-genetic columns")
  annotate_rubias(two_allele_data = two_allele_data, sample_type = sample_type, custom_format = custom_format, micro_stock_code.FN = micro_stock_code.FN, pop_map.FN = pop_map.FN)
  
}


