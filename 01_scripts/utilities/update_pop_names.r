# Update population names
#  Requires to be in MGL_GSI_SNP format (stock.code, year, fish.id, sex)
#  can rename populations using sep_by and "collection", "collection_and_year", or "none"
#  If using collection, can also translate from number to name by using name_by = "stockname"

update_pop_names <- function(sep_by = "collection", name_by = "stockname", add_CU = FALSE){
  
  # Create backup
  obj.bck <- obj
  
  # Extract genepop indiv names and put into df
  indiv_names.df <- as.data.frame(rownames(obj$tab))
  colnames(indiv_names.df) <- "indiv.name"
  head(indiv_names.df)
  
  # Separate the df into individual components
  indiv_names.df <- separate(data = indiv_names.df
                             , sep = "_"
                             , col = "indiv.name"
                             , into = c("stock.code", "year", "fish.id", "sex")
                            )
  head(indiv_names.df)

  
  ## Generate new names for populations
  # Reporting
  print(paste0("Renaming pops to ", sep_by))
  
  if(sep_by=="collection"){
    
    pop_short <- str_pad(string = indiv_names.df$stock.code, width = 2, side = "left", pad = "0")
    
  }else if(sep_by=="collection_and_year"){
    
    pop_short <- paste0(str_pad(string = indiv_names.df$stock.code, width = 2, side = "left", pad = "0"), "_", indiv_names.df$year)
    
  }else {
    
    print("Not renaming pops")
    pop_short <- pop(obj)
    
  }

  ## Update pop names
  pop(obj) <- pop_short
  print(unique(pop(obj)))
  
  ## Collect the new names into a dataframe
  pop_short.df <- as.data.frame(pop(obj))
  colnames(pop_short.df) <- "pop_short"
  
  # Make numeric for matching with stock code below
  if(sep_by == "collection"){
    
    # TO REMOVE
    #pop_short.df$pop_short <- as.integer(as.character(pop_short.df$pop_short)) # for below, this must be integer
    # TO REMOVE
    
  }else if(sep_by == "collection_and_year"){
    
    # separate the column into the collection and the year again
    pop_short.df <- separate(data = pop_short.df, sep = "_", col = "pop_short", into = c("stock.code", "year"))
    
    # rename stock.code into 'pop_short' for compatibility between methods
    colnames(pop_short.df)[which(colnames(pop_short.df)=="stock.code")] <- "pop_short"
    
  }else{
    
    print("Pop names still in original format")
    
  }
  
  # To merge with stock code file below, the pop short variable must be an integer
  if(sep_by != "none"){
    pop_short.df$pop_short <- as.integer(as.character(pop_short.df$pop_short)) 
  }

  
  ## Change from numeric to names
  #### 01.2 Pop names as numeric or character?
  if(sep_by!="none" && name_by=="stockname"){
    
    # Generate an order column for re-ordering the samples' stock code vector
    pop_short.df$id <- 1:nrow(pop_short.df)
    
    # Reporting
    print("Reading in stock codes")
    
    stock_codes.df <- read.delim2(file = sc.base)
    
    # Merge to get the stock code name
    rosetta <- merge(x = stock_codes.df, y = pop_short.df, by.x = "Code", by.y = "pop_short", sort = F, all.y = T)
    
    print("re-ordering back into original order")
    rosetta <- rosetta[order(rosetta$id), ]
    
    # Add the year if necessary, and create vector for using as pop ID
    if(sep_by=="collection_and_year"){
      
      rosetta$name <- paste0(rosetta$collection, "-", rosetta$year)
      
    }else if(sep_by=="collection"){
      
      rosetta$name <- rosetta$collection
      
    }
    
    # Add the CU if requested
    if(add_CU==TRUE){
      
      print("Adding CU to the collection name for the pop variable")
      
      rosetta$name <- paste(rosetta$name, rosetta$repunit, sep =  "_")
      
    }
    
    # Use the newly designated identifier per individual
    pop(obj) <- rosetta$name
    
    # Reporting
    print(unique(pop(obj)))
    print(table(pop(obj)))
    
  } else {
    
    print("Not renaming pop stock code to stock names")
    
  }
  
  assign(x = "obj", value = obj, envir = .GlobalEnv)
  assign(x = "obj.bck", value = obj.bck, envir = .GlobalEnv)
  assign(x = "sep_by", value = sep_by, envir = .GlobalEnv)
  assign(x = "name_by", value = name_by, envir = .GlobalEnv)
  assign(x = "add_CU", value = add_CU, envir = .GlobalEnv)
  
}
