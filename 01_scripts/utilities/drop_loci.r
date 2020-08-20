# Drop monomorphic loci

drop_loci <- function(df= obj, drop_monomorphic = TRUE, drop_file = NULL){
  
  # Reporting
  print(paste0("The input genind has ", length(locNames(df)), " markers"))
  
  # Drop monomorphic loci
  if(drop_monomorphic==TRUE){
    loci_to_drop <- which(nAll(df)==1) # identify which are monomorphic
    
    # Reporting
    print(paste0("Dropping monomorphic markers, in total: ", length(loci_to_drop), " markers"))
    
    # Avoid an == 0 error
    if(length(loci_to_drop)>0){
        df <- df[loc=-loci_to_drop]
    }
    
    print(paste0("After dropping monomorphic markers, there are ", length(locNames(df)), " markers"))
    
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
