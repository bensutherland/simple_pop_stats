## Annotate rubias file using custom data
# Developed from the non-custom script
# Only tested on microsatellite data so far
# Ben J. G. Sutherland 
# 2025-12-05

# Requires (see README for formats): 
# - pop map
# - stock code file
# - sample names do not have spaces

annotate_rubias_custom <- function(two_allele_data = NULL
                                    , sample_type = NULL
                                    , stock_code.FN = NULL
                                    , pop_map.FN = NULL
                                    , datatype = NULL){
  
  if(datatype == "SNP"){
    
    stop("SNP data custom approach not tested and developed yet")
    
  }
  
  #### 01. Read in required data ####
  ## Read in stock code file (required by all datasets)
  print(paste0("Reading in stock code file from: ", stock_code.FN))
  stock_code.df <- read.delim2(file = stock_code.FN, header = T, sep = "\t", stringsAsFactors = F)
  print(stock_code.df)
  
  ## Read in pop map file 
  print(paste0("Reading in pop map file from: ", pop_map.FN))
  pop_map.df <- read.delim2(file = pop_map.FN, header = T, sep = "\t") 
  print(head(pop_map.df))
  print(tail(pop_map.df))
    
  # The pop map file does not have repunit info, add from stock code file (order does not matter)
  pop_map.df <- merge(x = pop_map.df, y = stock_code.df, by.x = "pop", by.y = "collection", all.x = T)
  
  # Clean up df
  pop_map.df <- pop_map.df[,c("indiv", "pop", "repunit")]
  
  # Reporting
  print("The pop map file has the following individuals per repunit: ")
  print(table(pop_map.df$pop, useNA = "ifany"))
  
  
  #### 02. Add non-genetic columns ####
  # Use stock code info to get collection name and repunit; depends if microsat or SNP
  print("Using stock code file to collect names and repunits")
  
  # Update variable name
  rubias.df <- two_allele_data

  # Add the sample type (i.e., reference)
  rubias.df$sample_type <- rep(x = sample_type, times = nrow(rubias.df))
  
  # Add an indiv col 
  rubias.df$indiv <- rownames(rubias.df)
  
  # Add information from the pop map file
  rubias.df <- merge(x = rubias.df, y = pop_map.df, by.x = "indiv", by.y = "indiv", all.x = T)
  
  # Rename column as needed for rubias format
  colnames(rubias.df)[which(colnames(rubias.df)=="pop")] <- "collection"
  
  # View
  rubias.df[1:5,c(1:4, 28:32)] # TODO: make more adaptive
  
  # Put columns in order for rubias format
  rubias.df <- rubias.df %>% select(sample_type, repunit, collection, indiv, everything())
  
  #### Data checks ####
  #TODO: Confirm no NAs before proceeding
  print(table(rubias.df$indiv%in%rownames(two_allele_data)))
  
  
  #### Save output ####
  print("Saving output as 'rubias.df' in global env")
  assign(x = "rubias.df", value = rubias.df, envir = .GlobalEnv)
  
  # Create filename
  output.FN <- paste0(result.path, "rubias_output_", datatype, ".txt")
  
  # Reporting
  print(paste0("Writing out rubias file as ", output.FN))
  
  # Write out rubias file
  write.table(x = rubias.df, file = output.FN, sep = "\t", row.names = FALSE, quote = F)
  
}
