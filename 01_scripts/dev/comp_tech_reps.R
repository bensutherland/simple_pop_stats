# Compares samples loaded in from multiple genepops
#  Currently not generalized and requires *SNP data* output from amplitools
#  Operates on all genepop (*.gen) files in 02_input_data
#  Currently only operating on **two genepops**
# B. Sutherland (SBIO), initialized 2022-11-09

comp_tech_reps <- function(path="", format_type="amplitools", max_missing=0.5){
  
  # Provide warning
  print("** this is an in-development script and only works on **two genepops** for comparison so far. More development is planned **")
  
  # Read in all genepops in source folder
  input.FN <- list.files(path = "02_input_data/", pattern = "*.gen")
  
  genepop.list <- list()
  
  for(i in 1:length(input.FN)){
    
    genepop.list[[input.FN[i]]] <- read.genepop(file = paste0("02_input_data/", input.FN[i])
                                              , ncode = 2 # SNP data only
                                              )
    
  }
  
  # Reporting
  print(paste0("Loaded ", length(genepop.list), " genepop files to the genepop list"))
  
  ### Determine common markers to all obj
  marker.list <- list()
  for(i in 1:length(genepop.list)){
    
    marker.list[[i]]  <- locNames(genepop.list[[i]])
    
  }
  
  
  #### TODO ###
  # Stop if more than two genepops (not yet implemented)
  if(length(marker.list)>2){
    
    stop("This function cannot yet handle more than two genepops")
    
  }else if(length(marker.list)==2){
    
    print("There are two genepops in your marker list")
    
  }
  
  common_loci <- intersect(x = marker.list[[1]], y = marker.list[[2]])
  
  # Reporting
  print(paste0("There are ", length(common_loci), " in your marker list"))
  
  # Reporting
  print("Limiting each genepop to the common loci")
  
  # Limit each genepop to the common loci
  obj.1 <- genepop.list[[1]][loc=common_loci]
  obj.2 <- genepop.list[[2]][loc=common_loci]
  
  # Pool
  obj <- repool(obj.1, obj.2)
    
  obj
  
  print(indNames(obj))
  
  
  #### 01. Identify which is the best indiv to keep when replicates ####
  
  # Percent missing per individual
  percent_missing_by_ind(df = obj)
  
  # Clean up df
  rownames(missing_data.df) <- seq(1:nrow(missing_data.df))
  print(head(missing_data.df))
  
  # Separate the identifier column into the component parts
  if(format_type=="amplitools"){
    
    missing_data.df <- separate(data = missing_data.df, col = "ind", into = c("run", "barcode", "indiv")
                                , remove = F, sep = "__")
    head(missing_data.df)
    
  }else{
    
    stop("Stopped. The only individual naming format implemented so far is amplitools format.")
    
  }
  
  # Per individual, find which tech replicate has the most typed markers
  indiv <- unique(missing_data.df$indiv)
  print(paste0("Finding the best replicate for ", length(indiv), " unique individuals from "
               , nrow(missing_data.df), " total individuals")
        )
  
  # Loop
  ioi <- NULL; slice <- NULL; keep.vec <- NULL; keep <- NULL; tech_rep.keep <- NULL; tech_rep.keep.vec <- NULL
  for(i in 1:length(indiv)){
    
    # select indiv
    ioi <- indiv[i]
    slice <- missing_data.df[missing_data.df$indiv==ioi, ]
    
    # Put in descending order of ind.num.typed
    slice <- slice[order(slice$ind.num.typed, decreasing = TRUE), ]
    
    # Identify the best ind to keep (the top row)
    keep <- slice[1,"ind"]
    
    keep.vec <- c(keep.vec, keep)
    
    # Identify the best two ind to keep for tech rep comparison (below)
    if(nrow(slice) > 1){
      
      tech_rep.keep <- slice[1:2, "ind"]
      
      tech_rep.keep.vec <- c(tech_rep.keep.vec, tech_rep.keep)
      
    }else{
      
      print("Only one individual, not keeping as tech rep")
      
    }
    
  }
  
  print("The following is the list of samples with the most typed markers that will be the individual to keep")
  print(keep.vec)
  
  # Retain only the best from the obj
  print("Retaining only the best from the obj")
  obj.best <- obj[i=keep.vec]
  
  # Clear the population attribute
  pop(obj.best) <- rep("unkn", times = nInd(obj.best))
  obj.best
  
  assign(x = "obj_nr_best", value = obj.best, envir = .GlobalEnv)
  print("obj_nr_best has now been assigned to the global environment and can be used as a genepop input elsewhere")
  
  
  #### Keeping Replicates ####
  print("The following is the list of samples with the two best tech reps to keep for comparing technical replicates")
  print(tech_rep.keep.vec)
  
  # Filter samples if one has more than the allowable missing data
  print(paste0("Checking replicates to ensure both have less than ", max_missing * 100 , "% missing values" ))
  missing_data_reps.df <- missing_data.df[ missing_data.df$ind %in% tech_rep.keep.vec, ]
  
  head(missing_data_reps.df)
  
  ## Optional plotting
  #hist(missing_data_reps.df$ind.num.typed)
  #plot(missing_data_reps.df$ind.per.missing, missing_data_reps.df$ind.num.typed) # note these are directly 1:1
  
  ind_to_drop <- missing_data_reps.df[missing_data_reps.df$ind.per.missing > max_missing, "indiv"]
  # use shortly
  
  # Retain only the best two from the obj
  print("Retaining the best two for tech rep comparison")
  obj.tech.rep <- obj[i=tech_rep.keep.vec]
  
  # Clear the population attribute
  pop(obj.tech.rep) <- rep("unkn", times = nInd(obj.tech.rep))
  obj.tech.rep
  
  # Remove pairs where at least one of the pair has high percent missing
  indNames_to_drop <- grep( paste(ind_to_drop, collapse = "|"), indNames(obj.tech.rep), value = T)
  print(paste0("Removing ", length(indNames_to_drop)/2 , " pairs of individuals due to high missing"))
  indNames_to_keep <- setdiff(x = indNames(obj.tech.rep), y = indNames_to_drop)
  obj.tech.rep <- obj.tech.rep[i=indNames_to_keep]
  
  assign(x = "obj.tech.rep", value = obj.tech.rep, envir = .GlobalEnv)
  print("obj.tech.rep has now been assigned to the global environment and can be used as a genepop input elsewhere")
  
  
  #### 02. Compare genotypes of technical replicates ####
  print(obj.tech.rep)
  
  # Convert to df
  obj.df <- genind2df(obj.tech.rep)
  
  # Observe the data
  print(obj.df[1:5, 1:5])
  
  # Create an indiv col
  obj.df$indiv <- rownames(obj.df)
  
  # Remove lengthy rownames
  rownames(obj.df) <- seq(1:nrow(obj.df))
  
  dim(obj.df)
  
  # Observe data again
  print(obj.df[1:10, 577:587])
  ## note: this could be improved to be more flexible for different numbers of markers
  
  # Provide warning
  print("Warning: this system is currently depending on the amplitools format for sample IDs, ")
  print("...which means indiv are named by <run>__<barcode>__<indiv>")
  
  if(format_type=="amplitools"){
    
    # Separate the indiv ID into components
    obj.df <- separate(data = obj.df, col = "indiv", into = c("run", "barcode", "indiv"), sep = "__", remove = F)
    
    dim(obj.df)
    print(obj.df[1:10, 577:589])
    ## note: this could be improved to be more flexible for different numbers of markers
    
    # Identify the indiv IDs that have a technical replicate present
    #tech_rep_indivs <- dimnames(table(obj.df$indiv)==2)[[1]] 
    tech_rep_indivs <- names(table(obj.df$indiv)==2) # easier
    
  }else{
    
    stop("Error: only currently supported is the amplitools format")
    
  }
  
  
  #### Compare genotypes ####
  # Loop to count matching genotypes per marker per individual
  soi <- NULL; slice <- NULL; result.list <- list(); all_result.df <- NULL; 
  num_false <- NULL; num_true <- NULL; percent_true <- NULL; num_typed_in_both <- NULL
  
  for(i in 1:length(tech_rep_indivs)){
    
    print(i)
    
    # Identify the sample of interest
    soi <- tech_rep_indivs[i]
    print(paste0("Comparing genotypes of ", soi))
    
    # Obtain the genotype and sample ID data for the sample of interest
    slice <- obj.df[obj.df$indiv==soi, ]
    #dim(slice)
    #slice[, 1:5]
    
    # Drop annot cols
    slice <- slice[grep(pattern = "run|barcode|indiv|pop", x = colnames(slice), invert = T)] # drop cols
    #dim(slice)
    #slice[, 1:5]
    
    # Drop column if any NA (need both vals to be able to see if it is equal)
    slice <- slice[ , colSums(is.na(slice))==0]
    
    # Compare the genotypes in an NA-safe way
    num_true  <- as.numeric(table(mapply(identical, slice[1,], slice[2,]))[2])
    num_false <- as.numeric(table(mapply(identical, slice[1,], slice[2,]))[1])
    
    # What is the percentage? 
    num_typed_in_both <- (num_true + num_false)
    percent_match <- num_true / num_typed_in_both
    
    # Make the result into a dataframe and give the column name as the sample of interest
    result.df <- as.data.frame(c(num_true, num_false, percent_match, num_typed_in_both))
    colnames(result.df) <- soi
    
    rownames(result.df) <- c("num_true", "num_false", "percent_match", "num_typed_in_both")
    
    # Build the output df
    if(i==1){
      
      all_result.df <- result.df
      
    }else if(i > 1){
      
      all_result.df <- cbind(all_result.df, result.df)
      
    }
  }
  
  
  # Format output table
  all_result.df <- t(all_result.df)
  head(all_result.df)
  all_result.df <- as.data.frame(all_result.df)
  str(all_result.df)
  
  # Scatter plot percent match by number typed in both
  print("Plotting and exporting '03_results/tech_rep_percent_match_genos_by_number_typed.pdf'")
  pdf(file = "03_results/tech_rep_percent_match_genos_by_number_typed.pdf", width = 7, height = 5)
  plot(x = all_result.df$num_typed_in_both, y = all_result.df$percent_match, ylim = c(0,1)
       , ylab = "Percentage of matching genotypes"
       , xlab = "Number loci typed in both technical replicates")
  # summary(all_result.df$percent_match)["Mean"]
  # mean(all_result.df$percent_match, na.rm = T)
  
  med.match <- median(all_result.df$percent_match, na.rm = T)
  text(paste0("median = ", round(med.match, digits = 3)), x = 400, y = 0.4)
  dev.off()
  
  # Export data
  print("Results output to '03_results/tech_rep_results.txt'")
  write_delim(x = all_result.df, file = "03_results/tech_rep_results.txt", delim = "\t")
  
}
