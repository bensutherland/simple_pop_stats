# Function for microsat data, to create a stock code file the same as SNP data
prepare_stockcode_file <- function(fix_names = TRUE, names_file.FN = NULL){
  
  if(!is.null(names_file.FN)==TRUE){
    
    print("Using the stock code file to bring in region names for stocks")
    
    # Load in the names file
    names.df <- read.delim2(file = names_file.FN, header = F, sep = "\t")
    
    ## Split names file into the two fundamental parts
    # the stocks
    stock_info <- names.df[, c(1,3,2)]
    head(stock_info)
    colnames(stock_info) <- c("stock.name", "stock.code", "region.code")
    
    # the regions
    region_info <- names.df[, c(4, 3)]
    colnames(region_info) <- c("region.name", "region.code")
    
    # Combine into a single df, w/ region shown per stock
    all_pops_info.df <- merge(x = stock_info, y = region_info, by = "region.code", all.x = TRUE)
    
    # Reporting
    print("The formatted stock and region code info is in all_pops_info.df")
    
    head(all_pops_info.df)
    ###### NEXT: WRITE THIS OUT AS A SNP-TYPE STOCK CODE REPUNIT FILE ######
    
  }else{
    
    print("Set a path for the names.txt file")
    stop()
    
  }
  
  
  
  ### Fix names
  
  if(fix_names==TRUE){
  # For data checking, pull out the info
  retrospect_stocknames <- all_pops_info.df$stock.name
  print(paste0("There are ", length(unique(retrospect_stocknames)), " unique stock names before editing"))
  
  # Remove stock name characters after the first space
  all_pops_info.df$stock.name <- sub(pattern = " .*", replacement = "", x = all_pops_info.df$stock.name)
  
  # Confirm there are still the same number of unique pops after the edit:
  postedit_stocknames <- all_pops_info.df$stock.name
  print(paste0("There are ", length(unique(postedit_stocknames)), " unique stock names after editing"))
  
  # Data checking
  if(length(unique(postedit_stocknames))==length(unique(retrospect_stocknames))){
    print("No issue after stock name edit, continuing")
  }else{
    print("The total number of unique stock names was impacted by edit, crashing")
    stop()
  }
  }
  
  
  ### Format as per SNP data
  all_pops_info.df <- all_pops_info.df[,c("stock.code", "stock.name", "region.name")]
  colnames(all_pops_info.df) <- c("Code", "collection", "repunit")
  
  # Output results
  output.FN <- paste0(result.path, two.letter.code, "StockCodes_microsat.txt")
  print(paste0("Writing out the microsat stock code file formatted as per SNPs to ", output.FN))
  write_delim(x = all_pops_info.df, path = output.FN , delim = "\t")
  
  
}