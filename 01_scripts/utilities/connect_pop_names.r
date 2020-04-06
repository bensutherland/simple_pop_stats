# Specifically for microsat and SNP analysis, rename microsat pops to SNP pop names

connect_two_datatypes <- function(df = obj_pop_filt, crosswalk.FN = NULL){
  
  # Read in stock code base
  stock_codes.df <- read.delim2(file = sc.base, stringsAsFactors = F)
  stock_codes.df
  
  # see what pops are in the dataset currently
  unique(pop(df))
  
  # Read in crosswalk file
  if(!is.null(crosswalk.FN)){
    print("Reading in crosswalk file")
    print(crosswalk.FN)
    
    crosswalk <- read.delim2(file = crosswalk.FN, stringsAsFactors = F
                             , sep = ",", header = T)  
    
    print(crosswalk)
    
  }else{
    
    print("There was no crosswalk file selected")
    
    print("Use the following file to create a crosswalk file")
    crosswalk_blank.FN <- paste0(result.path, "crosswalk_blank.csv")
    print(crosswalk_blank.df)
    
    crosswalk_blank.df <- as.data.frame(unique(pop(obj_renamed)))
    colnames(crosswalk_blank.df) <- c("msat_pops")
    crosswalk_blank.df$snp_pops <- rep("unkn", times = nrow(crosswalk_blank.df))
    crosswalk_blank.df
    
    write.csv(x = crosswalk_blank.df, file = crosswalk_blank.FN, row.names = F, quote = F)
    
  }
  
  
  # Obtain the pop names from the genind df
  all_pops <- as.character(pop(df))
  all_pops.df <- as.data.frame(all_pops, stringsAsFactors = F)
  
  # add order column
  all_pops.df$id <- 1:nrow(all_pops.df)
  
  # Match up the microsat data to the crosswalk file
  rosetta <- merge(x = crosswalk, y = all_pops.df, by.x = "msat_pops", by.y = "all_pops"
                   , sort = F, all.y = T)
  
  # Re-order back to original
  print("Re-ordering back into original order")
  rosetta <- rosetta[order(rosetta$id), ]
  
  # Replace original pop names with datatype 2 pop names
  print("Replacing original pop names with datatype2 pop names")
  pop(df) <- rosetta$snp_pops
  
  # Reporting
  print("Your data now has the following pop names")
  print(unique(pop(df)))
  print("Your file is now called obj_connected")
  
  assign(x = "obj_connected", value = df, envir = .GlobalEnv)

}
