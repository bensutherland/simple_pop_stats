# Calculate relatedness for each population
# Data will be a genind

relatedness_calc <- function(data = obj_pop_filt, datatype = "SNP"){
  
  if(datatype == "SNP"){
    
  # Convert genind to genlight using dartR
  print("Converting genind to genlight")
  obj.gl <- gi2gl(data, parallel = TRUE)
  
  # Convert genlight to demerelate.df using demerelate
  print("Converting genlight to demerelate.df")
  obj_demerelate.df <- gl2demerelate(gl = obj.gl)
  obj_demerelate.df[1:5,1:5]
  
  ## Relatedness: info on related input can be obtained here: https://rdrr.io/rforge/related/man/related-package.html
  ### genotype file should have one column of individual identifiers and 
  ### then 2 columns for each locus (one for each allele). No other columns are allowed. 
  ### Missing data should be formatted as zeros ("0").
  ### The file should NOT contain a header row.
  
  # Convert demerelate.df to related format
  print("Converting demerelate.df to related")
  
  # Remove pop column
  print("Removing column with title 'Population'")
  obj_demerelate.df <- obj_demerelate.df[, -which(colnames(x = obj_demerelate.df)=="Population")]
  
  # Convert NA to "0" for related format
  obj_demerelate.df[is.na(obj_demerelate.df)] <- 0 
  str(obj_demerelate.df[1:10,1:10])
  
  # Save out as text file to be able to read in via readgenotypedata
  print("Exporting for ease of import")
  write.table(x = obj_demerelate.df, file = paste0(result.path, "obj_demerelate.txt")
              , quote = F, sep = "\t", row.names = F, col.names = F)
  
  # Read in via readgenotypedata as 'related' format
  print("Reading in as 'related' format")
  my_data.related <- readgenotypedata(genotype.data = paste0(result.path, "obj_demerelate.txt"))
  names(my_data.related)
  
  
  } else if(datatype == "microsat"){
    
    # Get genotypes, but note that it will be in the format of one column per marker (both alleles in one)
    data.hf <- genind2hierfstat(data)
    
    # Make into six character format
    for(i in 2:ncol(data.hf)){
      data.hf[,i] <- str_pad(string = data.hf[,i], width = 6, side = "left", pad = "0")
    }
    
    # Split the genotype data into two columns per marker
    data_out <- NULL; data_all <- NULL
    
    for(c in ncol(data.hf):2){
      data.hf <- data.hf %>%
        separate(col = colnames(data.hf)[c]
                 , into = c(colnames(data.hf)[c], paste0(colnames(data.hf)[c], "_1")
                 )
                 , sep = 3
        )
    }
    
    # Make pop a character
    data.hf$pop <- as.character(data.hf$pop)
    
    # Convert the pop names to stock codes
    # Read in stock codes
    print("Reading in stock code file")
    stock_codes.df <- read.delim2(file = sc.base, stringsAsFactors = F)
    
    # Combine the stock codes file with the data
    rosetta <- merge(x = data.hf, y = stock_codes.df, by.x = "pop", by.y = "collection", sort = F, all.x = T)
    
    data.hf <- rosetta
    
    # Change missing data to 0s
    data.hf[is.na(data.hf)] <- 0
    
    # Keep only the required columns and put into the appropriate format
    data.hf <- dplyr::select(data.hf, -c("pop", "repunit", "ProvState", "YLAT", "XLONG"))
    data.hf <- select(data.hf, Code, everything())
    
    # Write out
    write.table(x = data.hf, file = paste0(result.path, "Demerelate_input.txt"), quote = F, sep = "\t", col.names = F, row.names = F)
    
    # Read in via readgenotypedata as 'related' format
    print("Reading in as 'related' format")
    my_data.related <- readgenotypedata(genotype.data = paste0(result.path, "Demerelate_input.txt"))
    names(my_data.related)
    
    str(my_data.related$gdata)
    
    # Need to make 'group' a two-digit value instead of having single digits, due to the way related handles it
    my_data.related$gdata[,1] <- str_pad(string = my_data.related$gdata[,1], width = 2, side = "left", pad = "0")
    str(my_data.related$gdata)
    
  }
  
  # coancestry analysis w/ related
  output <- coancestry(genotype.data = my_data.related$gdata
                       , lynchrd = 2
                       , quellergt = 2
                       , wang = 2
  ) # all settings default from website
  
  
  # Save out results
  date <- format(Sys.time(), "%Y-%m-%d")
  assign(x = "output", value = output, envir = .GlobalEnv)
  save.image(file = paste0(result.path, "kinship_analysis_", date, ".Rdata"))
  
}
