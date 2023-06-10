# Drop monomorphic loci

drop_loci <- function(df= obj, drop_monomorphic = TRUE, drop_file = NULL){
  
  # Reporting
  print(paste0("The input genind has ", length(locNames(df)), " markers"))
  
  # Drop monomorphic loci
  if(drop_monomorphic==TRUE){
    
    # Which loci are monomorphic? 
    loci_to_drop <- which(nAll(df)==1)
    
    # Which loci are completely untyped? 
    loci_all_na  <- which(is.na(nAll(df)))
    
    # Reporting
    print(paste0("Dropping ", length(loci_to_drop), " monomorphic markers"))
    
    print(paste0("Dropping ", length(loci_all_na) , " completely untyped markers"))
    
    # Combine loci to drop
    remove_loci <- c(names(loci_to_drop), names(loci_all_na))
    
    # Which loci to keep? 
    keep_loci <- setdiff(x = locNames(df), y = remove_loci)
    
    df <- df[,loc = keep_loci]
    
    print(paste0("After dropping monomorphic and untyped markers, there are ", nLoc(df), " markers remaining"))
    
  }
  
  
  # Drop specific loci
  if(!is.null(drop_file)){
    
    # Reporting
    print("Reading in list of markers to drop, from the following file: ")
    print(drop_file)
    print(paste0("Before dropping, the genind has ",  length(locNames(df)), " markers"))
    
    drop_loc <- read.delim2(file = drop_file, header = F)
    drop_loc <- drop_loc$V1
    
    # Remove these loci
    print(paste0("Dropping user defined markers, in total: ", length(drop_loc), " markers"))
    
    all_loci <- locNames(df)
    
    keeploc <- setdiff(all_loci, drop_loc) # Identify the markers to keep
    
    df <- df[loc = keeploc]
    
    print(paste0("After dropping, the genind has ",  length(locNames(df)), " markers"))
    
  }
  
  print("The output object will be saved as 'obj_filt'")
  
  assign(x = "obj_filt", value = df, envir = .GlobalEnv)
  
  assign(x = "drop_loci_file", value = drop_file, envir = .GlobalEnv)
  
}
