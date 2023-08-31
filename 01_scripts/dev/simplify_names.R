# Simplify names when in specific formats
# currently designed to simplify genind files that have indiv names in amplitools format
# Ben J. G. Sutherland (2023-08-31)

simplify_names <- function(df = obj, format = "amplitools"){

  # Confirm that format is supported
  if(format=="amplitools"){
    
    print("User selected format: amplitools")
    cat("amplitools format expects three fields, sep by double underscore:\n i.e., <run>__<barcode>__<indiv.ID>\n")
    
  }else{
    
    stop("Only amplitools format has been implemented so far")
    
  }
  
  ## Obtain individual names as df
  indiv.df <- as.data.frame(indNames(df))
  colnames(indiv.df) <- "indiv"
  
  # Separate components of indiv ID (i.e., run, barcode, sample)
  print("Separating individual label into three fields.")
  indiv.df <- separate(data = indiv.df, col = "indiv", into = c("run", "barcode", "indiv"), sep = "__", remove = T)
  print(head(indiv.df))
  
  # Use reduced indiv name as indname in genind
  print("Updating indiv names to simplified names and saving as 'obj_simplified'")
  indNames(df) <- indiv.df$indiv
  assign(x = "obj_simplified", value = df, pos = .GlobalEnv)
  
  # Retain the info obtained from sample names
  print("Per sample run and barcode info is being saved as 'indiv.df'")
  assign(x = "indiv.df", value = indiv.df, pos = .GlobalEnv)

}
