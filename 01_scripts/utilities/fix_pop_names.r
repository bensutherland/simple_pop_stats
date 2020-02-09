# Rename microsat pops
# This assumes you have access to a names.dat that has been converted to tab-delimited format (use fixed-width text-to-columns in excel)

fix_pop_names <- function(df = obj, stock_code.FN = NULL){
  
  if(!is.null(stock_code.FN)==TRUE){
    
    print("Using a stock code file to bring in region names for stocks")
    # Load in the names file
    names.df <- read.delim2(file = stock_code.FN, header = F, sep = "\t")
    # names.df[10:15,]
    
    ## Split the old format names file into the two fundamental parts
    # First the stocks df
    
    stock_info <- names.df[, c(1,3,2)]
    head(stock_info)
    colnames(stock_info) <- c("stock.name", "stock.code", "region.code")
    
    # Second the regions df
    region_info <- names.df[, c(4, 3)]
    #region_info[10:15,]
    colnames(region_info) <- c("region.name", "region.code")
    
    # Combine these two to fix into one df again, with the correct region for every stock
    all_pops_info.df <- merge(x = stock_info, y = region_info, by = "region.code", all.x = TRUE)
    head(all_pops_info.df, n = 30)
    
    print("The formatted stock and region code info is in all_pops_info.df")
    
  }
  
  
  # Next, pull out the pop names from the genind file
  print("Renaming populations")
  
  # Inclusively replace everything after first space in vector
  all_pops <- as.character(pop(obj))
  all_pops <- sub(pattern = " .*", replacement = "", x = all_pops)
  
  # Remove other characters
  all_pops <- sub(pattern = "/.*", replacement = "", x = all_pops)
  
  # Confirm that your names match (the following should give you the same)
  # length(intersect(x = all_pops_info.df$stock.name, y = all_pops))
  # length(unique(all_pops))
   
  # Confirm names are matching
  if(length(intersect(x = all_pops_info.df$stock.name, y = all_pops))==length(unique(all_pops))){
    print("All of your stock names in the genind are reflected in your region file, and therefore are the same names")
  }else{
    print("Your stock names are not equal, confirm you have formatted the genind and region file correctly")
    stop()
  }
  
  all_pops <- as.data.frame(all_pops, stringsAsFactors = F)
  
  # Here can concatenate the region to the end
  #### TODO #####
  all_pops_w_region.df <- merge(x = all_pops, y = all_pops_info.df, by.x = "all_pops", by.y = "stock.name", all.x = TRUE, sort = F)
  head(all_pops)
  head(all_pops_w_region.df)
  
  all_pops_w_region.df$add_region <- paste0(all_pops_w_region.df$all_pops, "_", all_pops_w_region.df$region.name)
  
  
  # Rename pop value in object
  pop(obj) <- all_pops_w_region.df$add_region
  
  print("Your populations are now named:   ")
  print(unique(pop(obj)))
  
  assign(x = "obj", value = obj, envir = .GlobalEnv)
  
}
  