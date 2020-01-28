## Convert genepop data (microsat) to rubias format
genepop_to_rubias_microsat <- function(data){
  
  print("Converting microsat genepop to rubias format")
  
  # data is in a genind obj
  obj
  
  nLoc(obj)
  obj$loc.fac # the marker names and number of alleles
  # How many different markers are there?
  length(unique(obj$loc.fac))
  
  # What does the main data look like?
  obj$tab[1:5,1:20]
  
  # Create an object that contains allele 1 and allele 2 for each individual, using the alleles scored in tab
  
  
  # Set nulls
  moi <- NULL ; loc_name.oi <- NULL ; allele_name.oi <- NULL; all_data <- list()
  
  # Keep for later: nrow(obj$tab)
  
  # Per individual
  for(i in 1:10){
    
    # indiv.list <- list() # list method
    indiv.df <- NULL # vector df method
    
    # What are the unique markers?
    unique_markers <- unique(gsub(pattern = "\\..*", replacement = "", x = colnames(obj$tab)))
    unique_markers_allele_2 <- paste0(unique_markers, "_1")
    all_markers <- c(rbind(unique_markers, unique_markers_allele_2)) # interleave allele and allele_2
    head(all_markers)
    
    indiv.df <- as.data.frame(all_markers, stringsAsFactors = F) # make df
    head(indiv.df)
    
    # Make empty vector for this individual, two NAs for each marker to fill in with alleles
    NA.vec <- rep(x = "NA", times = length(indiv.df$all_markers))
    indiv.df <- cbind(indiv.df, NA.vec)
    indiv.df$NA.vec <- as.character(indiv.df$NA.vec)
    str(indiv.df)
    
    # What is the indiv name for this round?
    indiv_name <- gsub(pattern = " ", replacement = "_", x = rownames(obj$tab)[i])
    colnames(indiv.df)[which(colnames(indiv.df)=="NA.vec")] <- indiv_name
    head(indiv.df)
    
    # Transfer the allelic info per marker by scanning over all columns
    
    # Per column (marker_allele)
    for(c in 1:ncol(obj$tab)){
      
      # What marker_allele are we dealing with
      moi <- colnames(obj$tab)[c]
      
      # Identify marker
      loc_name.oi <- gsub(pattern = "\\..*", replacement = "", x = moi)
      
      # Identify allele
      allele_name.oi <- gsub(pattern = ".*\\.", replacement = "", x = moi)
      
      # Is it first or second allele being identified?
      # For this marker, has an allele been identified yet?
      if( indiv.df[which(indiv.df$all_markers==loc_name.oi), indiv_name]=="NA"){
        
        # It is the first allele for this marker
        marker <- "first"
        
      }else if( indiv.df[which(indiv.df$all_markers==loc_name.oi), indiv_name]!="NA" ){
        
        # It is the second allele for this marker
        marker <- "second"
        
      }
      
      # Check scores
      # If the indiv x marker_allele is a 0, not present here
      if(obj$tab[i,c]=="0"){
        
        print("Not this allele")
        
        
        # If the indiv x marker_allele is a 1, one allele is present here
      }else if(obj$tab[i,c]=="1"){
        
        
        # If it is the first allele identified
        if( marker=="first" ){
          
          rubias_locus_name <- paste0(loc_name.oi)
          
          
          # If it is the second allele identified 
        }else if( marker=="second" ){
          
          rubias_locus_name <- paste0(loc_name.oi, "_1")
          
        }
        
        # Write in the info into the df
        indiv.df[indiv.df$all_markers==rubias_locus_name, indiv_name] <- allele_name.oi
        
        # If the indiv x marker_allele is a 2, both alleles are this
      }else if(obj$tab[i,c]=="2"){
        
        rubias_locus_name <- paste0(loc_name.oi)
        indiv.df[indiv.df$all_markers==rubias_locus_name, indiv_name] <- allele_name.oi
        
        rubias_locus_name <- paste0(loc_name.oi, "_1")
        indiv.df[indiv.df$all_markers==rubias_locus_name, indiv_name] <- allele_name.oi
        
        # Write to a list with this marker x allele
        indiv.list[[rubias_locus_name]] <- allele_name.oi
        
        
      }
      
    }
    
    print(paste0("Finished indiv", rownames(obj$tab)[i]))
    
    # Replace NAs with 0s (#TODO)
    
    all_data[[indiv_name]] <- indiv.df
    
    
    
  }
  
  
  
  
  
  ##### 2. Take data out of list #####
  genos <- NULL; 
  genos.df <- as.data.frame(all_data[[i]][1])
  
  for(i in 1:length(all_data)){
    
    # Genos
    genos <- all_data[[i]][2]
    
    # Indiv
    colnames(all_data[[i]])[2]
    
    # mnames
    all_data[[i]][1]
    
    genos.df <- cbind(genos.df, genos)
    
  }
  
  genos.df
  rubias.df <- as.data.frame(t(genos.df), stringsAsFactors = F)
  
  rubias.df[1:5,1:5]
  colnames(rubias.df) <- rubias.df[1,]
  
  rubias.df <- rubias.df[-1,]
  rubias.df[1:5,1:5]
  
  # Add sample type, collection, repunit, indiv columns, and then it is finished. (Plus fix the NA issue above)
  
  
  
  
}
