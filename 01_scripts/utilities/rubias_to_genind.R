# Rubias file to genind
#  read in rubias file from file and convert to adegenet genind format
#  assumes that all markers are indicated as <marker> and <marker>_1 for the two alleles
#  assumes that the column 'indiv' precedes immediately all geno cols, and no metadata after
#  assumes that markers are in order of marker 1-allele1, marker 1-allele2
#  by Ben J. G. Sutherland, with code from https://github.com/bensutherland/simple_pop_stats/blob/master/01_scripts/utilities/rubias_to_VCF.R
#  initialized 2025-10-06

rubias_to_genind <- function(rubias.FN = NULL){
  
  #### 01. Read in rubias file and ID characteristics ####
  ## Identify or select input file
  # If no file was specified in call
  if(is.null(rubias.FN)){
    
    # Manually select rubias file to convert
    rubias.FN <- choose.files(caption = "Find the Rubias file to convert to VCF format")
    
  }else if(!is.null(rubias.FN)){
    
    print(paste0("Convert from rubias format to VCF format: ", rubias.FN))
    
  }
  
  # Read in rubias file
  rubias.df <- read_tsv(rubias.FN, guess_max = 100000, na = "NA")
  rubias.df <- as.data.frame(rubias.df) # convert to df
  
  # Reporting
  print(paste0("Rubias input includes ", nrow(rubias.df), " individuals"))
  print(paste0("Rubias input includes ", length(colnames(rubias.df)[grep(pattern = "\\_1$", x = colnames(rubias.df), perl = T)]), " markers"))
  
  # Identify start column of the genotypes
  gen_start_col <- which(colnames(rubias.df)=="indiv")+1
  print(paste0("Determined start column of geno data: ", gen_start_col))
  
  # Convert rubias 1/2 allele format into standard 0/1 allele format
  #  source: https://stackoverflow.com/questions/65182275/r-subtract-the-same-value-from-multiple-columns
  #  note: use instead of gsub to allow for multi-allelic format (e.g.,  0, 1, 2)
  rubias.df[, gen_start_col:ncol(rubias.df)] <- rubias.df[, gen_start_col:ncol(rubias.df)] - 1
  
  # Identify the first column for each marker that contains the genotype
  odd_indices <- seq(from = gen_start_col, to = (ncol(rubias.df)-1), by = 2)
  
  
  #### 02. Convert from two-column alleles to a single column for each marker/ indiv ####
  # Initialize output dataframe with individual metadata
  output.df <- rubias.df[, 1:(gen_start_col-1)] # Retain cols preceding the gen_start_col
  output.df[1:5,]
  
  # Loop to combine two-column alleles into a single column genotype
  allele_1_col <- NULL; allele_2_col <- NULL; mname <- NULL; target_col_num <- NULL; 
  for(i in 1:length(odd_indices)){
    
    # Identify the source col numbers and names
    allele_1_col <- odd_indices[i]
    allele_2_col <- odd_indices[i] + 1
    mname        <- colnames(rubias.df)[allele_1_col] 
    
    # Set the column number for the new created data
    target_col_num <- (ncol(output.df) + 1)
    
    # Combine the two alleles and put into the next col in the output df
    output.df[, target_col_num] <- as.data.frame(paste0(rubias.df[, allele_1_col], "|", rubias.df[, allele_2_col]))
    
    # Update column name to marker name
    colnames(output.df)[target_col_num] <- mname
    
  }
  
  dim(output.df)
  output.df[1:5, 1:10]
  
  # Update rownames
  rownames(output.df) <- output.df$indiv
  
  # Clean colnames (i.e., remove periods)
  print("Replacing any periods in marker names with underscores for downstream compatibility")
  colnames(output.df) <- gsub(pattern = "\\.", replacement = "_", x = colnames(output.df))
  
  # Retain pop
  ordered_pop <- output.df$collection
  
  # Drop metadata from output df (keep only genetic data)
  output.df <- output.df[, gen_start_col:ncol(output.df)]
  dim(output.df)
  
  
  #### 03. Convert to genind object ####
  new_obj.gi <- df2genind(X = output.df, sep = "|", pop = ordered_pop, NA.char = "NA|NA", ncode = 1)
  
  
  #### 04. Save out ####
  assign(x = "new_obj.gi", value = new_obj.gi, envir = .GlobalEnv)
  print("The new genind object 'new_obj.gi' is available in the local enviro.")
  print(new_obj.gi)

  }