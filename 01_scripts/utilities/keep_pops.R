keep_pops <- function(df = obj_filt, keep_file = NULL){
  
  # Reporting
  print(paste0("The input genind has ", length(unique(pop(df))), " populations"))
  
  print("The number of individuals per population: ")
  table(pop(df))[sort(names(table(pop(df))))]
  

  # Keep specific user-defined pops
  if(!is.null(keep_file)){
    
    # Reporting
    print("Reading in list of populations to keep, from the following file: ")
    print(keep_file)
    print(paste0("Before dropping unneeded pops, the genind has ",  length(unique(pop(df))), " pops"))
    
    user_defined_keep_file <- read.delim2(file = keep_file, header = F, stringsAsFactors = F)
    user_defined_keep_pops <- user_defined_keep_file$V1
    
    # Remove these loci
    print(paste0("Requested user-defined pops to keep, in total: ", length(user_defined_keep_pops), " pops"))
    print("Requested to keep:   ")
    print(user_defined_keep_pops)
    
    all_pops <- as.character(unique(pop(df)))
    
    keep_pops_user_def <- intersect(all_pops, user_defined_keep_pops)
    
    df <- df[pop=keep_pops_user_def]
    
    print(paste0("After removing unnecessary pops, the output genind has ", length(unique(pop(df))), " pops"))
    
  }
  
  

  print("The output object will be saved as 'obj_pop_filt'")
  assign(x = "obj_pop_filt", value = df, envir = .GlobalEnv)
  assign(x = "keep_pops_file", value = keep_file, envir = .GlobalEnv)
  
}