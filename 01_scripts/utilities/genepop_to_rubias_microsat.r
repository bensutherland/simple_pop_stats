## Convert genepop data (microsat) to rubias format
genepop_to_rubias_microsat <- function(data = data, sample_type = sample_type, micro_stock_code.FN = NULL){
  
  print("Converting microsat genepop to rubias format")
  
  
  ## How to find details regarding the data (genind)
  # nLoc(data)
  # data$loc.fac # the marker names and number of alleles
  # length(unique(data$loc.fac)) # How many different markers are there?
  # data$tab[1:5,1:20] # What does the main data look like?
  
  # Convert 'NA' vals to 0
  print(paste0("Converting ", length(data$tab[is.na(data$tab)]),  " NA values to 0"))
  data$tab[is.na(data$tab)] <- 0
  
  # What are the unique markers in this genind?
  unique_markers <- unique(gsub(pattern = "\\..*", replacement = "", x = colnames(data$tab)))
  unique_markers_allele_2 <- paste0(unique_markers, "_1")
  all_markers <- c(rbind(unique_markers, unique_markers_allele_2)) # interleave allele and allele_2
  print("For this genind, your markers are... ")
  print(all_markers)
  
  # Create an object that contains allele 1 and allele 2 for each individual, using the alleles in tab
  # Set nulls
  moi <- NULL ; loc_name.oi <- NULL ; allele_name.oi <- NULL; all_data <- list()
  
  # Per individual
  for(i in 1:nrow(data$tab)){
    
    indiv.list <- list() # list method
    indiv.df <- NULL # vector df method
    
    # Create df to fill
    indiv.df <- as.data.frame(all_markers, stringsAsFactors = F) # make df
    head(indiv.df)
    
    # Make empty vector for this individual, two NAs for each marker to fill in with alleles
    NA.vec <- rep(x = "NA", times = length(indiv.df$all_markers))
    indiv.df <- cbind(indiv.df, NA.vec)
    indiv.df$NA.vec <- as.character(indiv.df$NA.vec)
    # str(indiv.df)
    
    # What is the indiv name for this round?
    indiv_name <- gsub(pattern = " ", replacement = "_", x = rownames(data$tab)[i]) # replace spaces with underscores
    indiv_name <- gsub(pattern = "\\_*\\_", replacement = "_", x = indiv_name) # replace multiple "_" w/ single "_"
    colnames(indiv.df)[which(colnames(indiv.df)=="NA.vec")] <- indiv_name # replace the column name with the individual name
    head(indiv.df)
    
    # Transfer the allelic info per marker by scanning over all columns
    
    # Per column (marker_allele)
    for(c in 1:ncol(data$tab)){
      
      # What marker_allele are we dealing with
      moi <- colnames(data$tab)[c]
      
      # Identify marker by removing the allele info
      loc_name.oi <- gsub(pattern = "\\..*", replacement = "", x = moi)
      
      # Identify allele by removing the marker info
      allele_name.oi <- gsub(pattern = ".*\\.", replacement = "", x = moi)
      
      # Is it first or second allele being identified for this individual and marker combo?
      # For this marker, has an allele been identified yet?
      if( indiv.df[which(indiv.df$all_markers==loc_name.oi), indiv_name]=="NA"){
        
        # It is the first allele for this marker
        marker <- "first"
        
      }else if( indiv.df[which(indiv.df$all_markers==loc_name.oi), indiv_name]!="NA" ){
        
        # It is the second allele for this marker
        marker <- "second"
        
      }
      
      
      ## Check scores
      ## DEBUGGING
      # print(i)
      # print(c)
       
      # If the indiv-marker_allele is a 0, this allele is not present
      if(data$tab[i,c]=="0"){
        
        # print("Not this allele")
        
        
      # If the indiv x marker_allele is a 1, one allele is present here
      }else if(data$tab[i,c]=="1"){
        
        
        # If it is the first allele identified
        if( marker=="first" ){
          
          rubias_locus_name <- paste0(loc_name.oi)
          
          
        # If it is the second allele identified 
        }else if( marker=="second" ){
          
          rubias_locus_name <- paste0(loc_name.oi, "_1")
          
        }
        
        # Write in the info into the df
        indiv.df[indiv.df$all_markers==rubias_locus_name, indiv_name] <- allele_name.oi
        
        # If the indiv x marker_allele is a 2, both alleles for the individual-marker are this allele
      }else if(data$tab[i,c]=="2"){
        
        rubias_locus_name <- paste0(loc_name.oi)
        indiv.df[indiv.df$all_markers==rubias_locus_name, indiv_name] <- allele_name.oi
        
        rubias_locus_name <- paste0(loc_name.oi, "_1")
        indiv.df[indiv.df$all_markers==rubias_locus_name, indiv_name] <- allele_name.oi
        
        # Write to a list with this marker x allele
        indiv.list[[rubias_locus_name]] <- allele_name.oi
        
        
      }
      
    }
    
    ## Debugging
    # print(paste0("Finished indiv", rownames(data$tab)[i]))
    
    all_data[[indiv_name]] <- indiv.df
    
    
    
  }
  
  ## Data checking ## before proceeding, make sure all individuals have the four-digit year as needed
  all_indiv_names <- names(all_data)
  all_indiv_names
  
  # If each record has a four or three digit year, as needed for the downstream annotation, continue, else crash
  if(length(grep(pattern = "\\_[0-9][0-9][0-9][0-9]\\_.*|\\_[0-9][0-9][0-9]\\_.*", x = all_indiv_names, perl = T))==length(all_indiv_names)){
    
    print("All records have a four- or three-digit year as necessary, continue")
    
  }else{
    
    stop("Not all records have a four- or three digit year, as is needed below. Please check input genepop and make sure four- or three-digit year is present.")
    
  }
  
  ##### 2. Take data out of list #####
  # Set nulls
  genos <- NULL
  
  # Define the markers and start the dataframe (could work on any 'i')
  genos.df <- as.data.frame(all_data[[i]][1])
  
  # Loop across the list, pulling out the genos
  for(i in 1:length(all_data)){
    
    # Genos
    genos <- all_data[[i]][2]
    
    # Indiv
    colnames(all_data[[i]])[2] # this doesn't appear to be being used... (#TODO: delete?)
    
    # mnames
    all_data[[i]][1]           # this doesn't appear to be being used... (#TODO: delete?)
    
    # assumes that the order of the markers is constant in every element of the list
    
    genos.df <- cbind(genos.df, genos)
    
  }
  
  # Data is now in a df
  genos.df[1:5, 1:5]
  
  # Transpose into a rubias starting format
  rubias.df <- as.data.frame(t(genos.df), stringsAsFactors = F)
  rubias.df[1:5,1:5] # view
  
  # Rename the columns as per the first row, then delete the first row
  colnames(rubias.df) <- rubias.df[1,]
  rubias.df <- rubias.df[-1,]
  rubias.df[1:5,1:5] # view
  
  # Rename the object
  two_allele_data <- rubias.df
  
  # Reporting
  print("Successfully completed conversion from genepop to two-allele data")
  
  # Create backup
  assign(x = "two_allele_data.bck", value = two_allele_data, pos = .GlobalEnv)
  write.csv(x = two_allele_data, file = paste0(result.path, "two_allele_data_genepop_rubias_mid_convert.txt"))
  
  # Reporting
  print("Created backup, two_allele_data.bck")
  
  # Note: this will change any allele name that starts with a number to have a preceeding "X" (e.g., X1b)
  
  ## In case of fail, restart here
  # rubias.df <- read_delim(file = paste0(result.path, "rubias_mid.txt"), delim = "\t")
  # dim(rubias.df)
  # head(rubias.df)
  # two_allele_data <- rubias.df
  # sample_type <- "reference"
   
  # Reporting
  print("Starting the annotation step to complete the rubias conversion, running annotate_rubias()")
  
  #### Adding non-genetic columns #####
  annotate_rubias(two_allele_data = two_allele_data, sample_type = sample_type, micro_stock_code.FN = micro_stock_code.FN)

}

