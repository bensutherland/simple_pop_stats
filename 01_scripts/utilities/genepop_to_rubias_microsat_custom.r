## Convert genepop data (microsat) to rubias format using custom data
# Developed from the non-custom script
# Ben J. G. Sutherland 
# 2025-12-05

# Requires (see README for formats): 
# - pop map
# - stock code file 
# - sample names do not have spaces
# - diploid only
# - no marker names that start with a number (otherwise will rename as Xmarkername)

genepop_to_rubias_microsat_custom <- function(data = obj, sample_type = "reference", stock_code.FN = NULL, pop_map.FN = NULL){
  
  print("Converting microsat genepop with custom samplenames to rubias format")
  
  # Convert 'NA' vals to 0
  print(paste0("Converting ", length(data$tab[is.na(data$tab)]),  " NA values to 0"))
  data$tab[is.na(data$tab)] <- 0
  
  
  ##### 1. Collect allele info per indiv and per locus #####
  
  # Identify unique markers in genind
  unique_markers <- locNames(data)
  unique_markers_allele_2 <- paste0(unique_markers, "_1")
  all_markers <- c(rbind(unique_markers, unique_markers_allele_2)) # interleave allele and allele_2
  print("Converting the following markers: ")
  print(unique_markers)
  print("...to...")
  print(all_markers)
  
  # Create an object that contains allele 1 and allele 2 for each individual, using the alleles in tab
  # Set nulls
  moi <- NULL ; loc_name.oi <- NULL ; allele_name.oi <- NULL; all_data <- list(); 
  indiv_name <- NULL; indiv.df <- NULL
  
  # Per individual
  for(i in 1:nrow(data$tab)){
    
    # Set additional nulls here
    #indiv.list <- list() # list method
    
    # Create df to fill
    indiv.df <- as.data.frame(all_markers, stringsAsFactors = F)
    head(indiv.df)
    
    # Create empty column to fill
    indiv.df$NA.vec <- NA
    
    # Identify indiv samplename
    indiv_name <- rownames(data$tab)[i]
    colnames(indiv.df)[which(colnames(indiv.df)=="NA.vec")] <- indiv_name # replace the column name with the individual name
    head(indiv.df)
    
    
    ## Collect the allelic info per marker by scanning over all columns
    # Per column (marker_allele)
    for(c in 1:ncol(data$tab)){
      
      # Identify marker_allele for iteration
      moi <- colnames(data$tab)[c]
      
      # Identify marker name only (without allele)
      loc_name.oi <- gsub(pattern = "\\..*", replacement = "", x = moi)
      
      # Identify allele name only (without marker)
      allele_name.oi <- gsub(pattern = ".*\\.", replacement = "", x = moi)
      
      ## Is it first or second allele being identified for this individual and marker combo?
      # For this marker, has an allele been identified yet?
      if( is.na(indiv.df[which(indiv.df$all_markers==loc_name.oi), indiv_name]) ){
        
        # The original marker has no data yet (NA), so it is the first allele for this marker
        marker <- "first"
        
      }else if( !is.na(indiv.df[which(indiv.df$all_markers==loc_name.oi), indiv_name]) ){
        
        # It is the second allele for this marker
        marker <- "second"
        
      }
      
      # If the indiv-marker_allele is a 0, this allele is not present
      if(data$tab[i,c]=="0"){
        
        # print("Not this allele")
        
      # If the indiv-marker_allele is a 1, one allele is present here
      }else if(data$tab[i,c]=="1"){
        
        # If it is the first allele identified
        if( marker=="first" ){
          
          # Set rubias locus name as first allele (no underscore)
          rubias_locus_name <- paste0(loc_name.oi)
          
        # If it is the second allele identified 
        }else if( marker=="second" ){
          
          # Set rubias locus name as second allele (w/ underscore)
          rubias_locus_name <- paste0(loc_name.oi, "_1")
          
        }
        
        # Write in the info into the df
        indiv.df[indiv.df$all_markers==rubias_locus_name, indiv_name] <- allele_name.oi
        
      # If the indiv x marker_allele is a 2, both alleles for the individual-marker are this allele
      }else if(data$tab[i,c]=="2"){
        
        # Set rubias locus names and write in info
        rubias_locus_name <- paste0(loc_name.oi)
        indiv.df[indiv.df$all_markers==rubias_locus_name, indiv_name] <- allele_name.oi
        
        rubias_locus_name <- paste0(loc_name.oi, "_1")
        indiv.df[indiv.df$all_markers==rubias_locus_name, indiv_name] <- allele_name.oi
        
        # # Write to a list with this marker x allele
        # indiv.list[[rubias_locus_name]] <- allele_name.oi
        
      }
      
    }
    
    ## Debugging
    # print(paste0("Finished indiv", rownames(data$tab)[i]))
    
    all_data[[indiv_name]] <- indiv.df
    
  }
  
  # Reporting
  print("Genotypes have been extracted and put into a list for each individual")
  print(paste0("The list contains data for ", length(all_data), " individuals."))
  
  #TODO: What about missing data, is it properly handled here? To confirm (BJGS: 2025-12-05)
  
  
  ##### 2. Convert list to df #####
  
  # Define the markers and start a dataframe (all samples are the same for this vector)
  genos.df <- as.data.frame(all_data[[1]][1])
  
  # Set nulls
  genos <- NULL
  
  # Loop across the list, pulling out the genos
  for(i in 1:length(all_data)){
    
    # Genos
    genos <- all_data[[i]][2]
    
    # NOTE: assumes that the order of the markers is constant in every element of the list
    # TODO: confirm this assumption is valid, or put a check in to verify
    genos.df <- cbind(genos.df, genos)
    
  }
  
  # View df
  genos.df[1:5, 1:5]
  
  # Transpose
  rubias.df <- as.data.frame(t(genos.df), stringsAsFactors = F)
  
  # Clean space
  rm(genos.df)
  
  # View df
  rubias.df[1:5,1:5]
  
  # Take first row as colnames, then delete the first row
  colnames(rubias.df) <- rubias.df[1,]
  rubias.df <- rubias.df[-1,]
  
  # View df
  rubias.df[1:5,1:5]
  
  # Rename the object
  two_allele_data <- rubias.df
  
  # Clean space
  rm(rubias.df)
  
  # Reporting
  print("Completed conversion from genepop to two-allele data")
  print("Saving as two_allele_data.bck in the global enviro, and saving to output folder 'two_allele_data_genepop_rubias_mid_convert.txt' ")
  
  # Create backup
  assign(x = "two_allele_data.bck", value = two_allele_data, pos = .GlobalEnv)
  write.csv(x = two_allele_data, file = paste0(result.path, "two_allele_data_genepop_rubias_mid_convert.txt"))
  
  # Reporting
  print("Starting the annotation step to complete the rubias conversion, running annotate_rubias_custom()")
  
  ## Add non-genetic columns
  annotate_rubias_custom(two_allele_data = two_allele_data
                         , sample_type = sample_type
                         , stock_code.FN = stock_code.FN
                         , pop_map.FN = pop_map.FN
                         , datatype = datatype
                         )

}

