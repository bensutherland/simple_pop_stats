# Calculate pair-wise Fst
# format should be genind or hierfstat
# If you want a custom filename for your output csv, set cust_fn as the basename (note this will automatically go in 03_results)

calculate_FST <- function(format="genind", dat = obj_filt, separated = FALSE, cust_fn = NULL){
  
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
  
  # Calculate FST
  print("Calculating WCfst")
  pairwise.wc.fst <- pairwise.WCfst(dat)
  print(pairwise.wc.fst)
  assign(x = "pairwise_wc_fst", value = pairwise.wc.fst, envir = .GlobalEnv)
  
  # Save results
  if(separated==TRUE){
    
    fn <- paste0("03_results/gen_diff_wcfst_", sep_by, "_by_", name_by, ".csv")
    
  }else{
    
    fn <- paste0("03_results/gen_diff_wcfst.csv")
    
  }

  
  # Is a custom filename being used?
  if(!is.null(cust_fn)){
    
    fn <- paste0("03_results/", cust_fn)
    
  } else {
    print("Not using a custom filename")
  }

  # Save out
  print(paste0("Saving output as ", fn))
  
  write.csv(x = pairwise.wc.fst, file = fn)
  
}
