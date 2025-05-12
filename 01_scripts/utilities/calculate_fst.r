# Calculate pair-wise Fst
# format should be genind or hierfstat
# If you want a custom filename for your output csv, set cust_fn as the basename (note this will automatically go in 03_results)
# Note: bootstrapping is currently not working well, keep it as FALSE or else often get an error

calculate_FST <- function(format="genind", dat = obj_filt, separated = FALSE, cust_fn = NULL, bootstrap = FALSE){
  
  # Determine if input needs converting from genind to hierfstat
  if(format=="genind"){
    
    print("Converting genind to hierfstat")
    
    # Make output object name using the name of the input object
    out.NM <- paste0(deparse(quote(dat)), ".hf")
    
    # Convert to hierfstat
    dat.hf <- genind2hierfstat(dat = dat) # convert
    
    # Save individual names to rownames
    # note: your first column is the popnames column
    rownames(dat.hf) <- indNames(dat) # save indiv names as rownames
    
    # Now move this hf data into the dat object
    dat <- dat.hf
    
    # Save out
    print("Putting the hierfstat object into the global environment")
    assign(x = out.NM, value = dat, envir = .GlobalEnv)
    
  }else if(format=="hierfstat"){
    
    print("No conversion necessary, your data is already hierfstat")
    
  } else {
    print("Your datatype is not supported")
    return(fail) # crash if not supported
  }
  
  
  ## Calculate FST
  print("Calculating WCfst")
  
  if(bootstrap==TRUE){
    
    print("Using a bootstrapped approach")
    
    # Create variable for later use
    booted <- "booted"
    
    # Calculate bootstrapped FST
    pairwise.wc.fst <- boot.ppfst(dat = dat, nboot = 1000, quant = c(0.025, 0.975))
    
    # Print output
    print(pairwise.wc.fst)
    
    ## Collect output into a single matrix
    # For more information, see: https://stackoverflow.com/questions/54576385/is-there-an-r-function-that-populates-the-lower-or-upper-diagonal-of-a-matrix)
    
    # Create empty matrix 
    new <- matrix(NA, nrow=nrow(pairwise.wc.fst$ll), ncol=ncol(pairwise.wc.fst$ll))
    
    # Assign lower limit to upper triangle (temporarily)
    new[upper.tri(new)] <- pairwise.wc.fst$ll[upper.tri(pairwise.wc.fst$ll)]
    
    # Put the lower limit into the lower triangle
    new <- t(new)
    
    # Assign upper limit to upper triangle
    new[upper.tri(new)] <- pairwise.wc.fst$ul[upper.tri(pairwise.wc.fst$ul)]
    
    # Assign the names to the column and rows of the new matrix
    dimnames(new) <- dimnames(pairwise.wc.fst$ll)
    
    # Rename the new matrix
    pairwise_wc_fst.df <- as.data.frame(new, stringsAsFactors = FALSE)
    
    # Assign to the environment
    assign(x = "pairwise_wc_fst.df", value = pairwise_wc_fst.df, envir = .GlobalEnv)
    
    # In case user wants the raw output from program for checking
    assign(x = "pairwise_wc_fst_hfstat.list", value = pairwise.wc.fst, envir = .GlobalEnv)
    
    # Reporting
    print("Prepared output in a df for export in 'pairwise_wc_fst.df'")
    print("Raw list-form output from hierfstat in 'pairwise_wc_fst_hfstat.list'")
    
    
  }else if(bootstrap==FALSE){
  
    # Reporting
    print("Not using bootstrap approach")
    booted <- "not_booted" # save type of approach
    
    # Calculate average FST
    pairwise.wc.fst <- pairwise.WCfst(dat)
    
    # Convert to df
    pairwise_wc_fst.df <- as.data.frame(pairwise.wc.fst)
    
    # Print output to screen 
    print(pairwise_wc_fst.df)
    
    # Assign to global enviro
    assign(x = "pairwise_wc_fst.df", value = pairwise_wc_fst.df, envir = .GlobalEnv)  
    
  }
  
  # Save results
  if(separated==TRUE){
    
    fn <- paste0(result.path, "gen_diff_wcfst_", sep_by, "_by_", name_by, "_", booted, ".csv")
    
  }else{
    
    fn <- paste0(result.path, "gen_diff_wcfst_", booted, ".csv")
    
  }

  
  # Is a custom filename being used?
  if(!is.null(cust_fn)){
    
    fn <- paste0(result.path, cust_fn)
    
  } else {
    print("Not using a custom filename")
  }

  # Save out
  print(paste0("Saving output as ", fn))
  
  write.csv(x = pairwise_wc_fst.df, file = fn)
  
}
