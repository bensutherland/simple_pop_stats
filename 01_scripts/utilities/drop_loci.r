# Drop monomorphic loci

drop_loci <- function(drop_monomorphic = TRUE, drop_file = NULL){
  
  # Reporting
  print(paste0("The input genind has ", length(locNames(obj)), " markers"))
  
  # Drop monomorphic loci
  if(drop_monomorphic==TRUE){
    loci_to_drop <- which(nAll(obj)==1) # identify which are monomorphic
    
    # Reporting
    print(paste0("Dropping monomorphic markers, in total: ", length(loci_to_drop), " markers"))
    
    obj <- obj[loc=-loci_to_drop]
    
    print(paste0("After dropping monomorphic markers, there are ", length(locNames(obj)), " markers"))
    
  }
  
  
  # Drop specific loci
  if(!is.null(drop_file)){
    
    # Reporting
    print("Reading in list of markers to drop, from the following file: ")
    print(drop_file)
    print(paste0("Before dropping, the genind has ",  length(locNames(obj)), " markers"))
    
    drop_loc <- read.delim2(file = drop_file, header = F)
    drop_loc <- drop_loc$V1
    
    # Remove these loci
    print(paste0("Dropping user defined markers, in total: ", length(drop_names), " markers"))
    
    all_loci <- locNames(obj)
    
    keeploc <- setdiff(all_loci, drop_loc) # Identify the markers to keep
    
    obj <- obj[loc = keeploc]
    
    print(paste0("After dropping, the genind has ",  length(locNames(obj)), " markers"))
    
  }
  
  print("The output object will be saved as 'obj_filt'")
  assign(x = "obj_filt", value = obj, envir = .GlobalEnv)
  
}
