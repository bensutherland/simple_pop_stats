# Convert genepop data (SNP) to rubias input (where data = a genind file)
genepop_to_rubias_SNP <- function(data){
  
  print("Converting SNP genepop to rubias format")
  
  # Select out the data component
  data <- data$tab
  #data[1:5,1:7]
  
  # Order columns (alphabetically)
  data <- data[ , order(colnames(data))]
  #data[1:5,1:7]
  
  # Only retain the first allele (how this differs from microsat)
  one_allele_data <- data[, grep(pattern = ".01", x = colnames(data), fixed = T)]
  #one_allele_data[1:5,1:5]
  
  # Convert the genepop code back to homo/het info
  for(i in 1:ncol(one_allele_data)){
    
    # Replace based on the number the datatype in torrent format
    one_allele_data[,i] <- gsub(pattern = "0", replacement = "homo.alt", x = one_allele_data[,i])
    one_allele_data[,i] <- gsub(pattern = "1", replacement = "het", x = one_allele_data[,i])
    one_allele_data[,i] <- gsub(pattern = "2", replacement = "homo.ref", x = one_allele_data[,i])
    
    # Replace NAs with 'missing'
    one_allele_data[is.na(one_allele_data[,i]),i] <- "missing"
    
  }
  #one_allele_data[1:15,1:10]
  
  # Drop the .01 in the name for rubias format
  colnames(one_allele_data) <- gsub(pattern = "\\.01$", replacement = "", perl = T, x = colnames(one_allele_data))
  #one_allele_data[1:15,1:10]
  
  # Convert to rubias format from proton format
  for(i in 1:ncol(one_allele_data)){
    one_allele_data[,i] <- ifelse(test = one_allele_data[,i]=="homo.ref", yes = "1 1"
                                  , no = ifelse(test = one_allele_data[,i]=="het", yes = "1 2"
                                  , no = ifelse(test = one_allele_data[,i]=="homo.alt", yes = "2 2"
                                  , no = ifelse(test = one_allele_data[,i]=="missing", yes = "NA NA"
                                  , no = "NA NA"
                                                              ))))
  }
  #one_allele_data[1:15,1:10]

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
  
  one_allele_data[1:5,1:5]
  
  two_allele_data <- one_allele_data
  rm(one_allele_data)
  
  # make df
  two_allele_data <- as.data.frame(x = two_allele_data, stringsAsFactors = FALSE)
  
  #### HERE TODAY - CAN'T GET IT TO ADD NEW COLUMNS...
  
  # Format the rubias non-genetic columns
  two_allele_data$sample_type <- rep(x = sample_type, times = nrow(two_allele_data))
  #two_allele_data
  
  # Look up pop name
  two_allele_data$indiv  <- rownames(two_allele_data)
  head(two_allele_data$indiv)
  pop_code <- as.data.frame(as.character(gsub(pattern = "\\_.*", replacement = "", x = two_allele_data$indiv), stringsAsFactors = F))
  colnames(pop_code) <- "pop_code"
  sc.df <- read.delim2(file = sc.base, header = TRUE, stringsAsFactors = F)
  pop_code <- merge(x = pop_code, y = sc.df, by.x = "pop_code", by.y = "Code", sort = F, all.x = TRUE)
  
  # Add in data
  two_allele_data$collection  <- pop_code$collection
  two_allele_data$repunit <- pop_code$repunit
  head(two_allele_data)
  
  
  
  
  
  dplyr::select(sample_type, dplyr::everything())
  
}


