# Drop monomorphic loci

drop_pops <- function(df = obj, drop_by_pop_size = TRUE, min_indiv = 20, 
                      drop_file = NULL, drop_unused = FALSE,
                      names.dat.FN = NULL, drop_indivs = NULL){
  
  # Reporting
  print(paste0("The input genind has ", length(unique(pop(df))), " populations"))
  
  print("The number of individuals per population: ")
  table(pop(df))[sort(names(table(pop(df))))]
  
  if(!is.null(drop_indivs)){
    user_defined_drop_indivs_file <- read.delim2(file = drop_indivs, header = F, stringsAsFactors = F)
    user_defined_drop_indivs <- user_defined_drop_indivs_file$V1
  
    print(paste0("Dropping user defined individuals, in total: ", length(user_defined_drop_indivs), " individuals"))
    print("Dropping:   ")
    print(user_defined_drop_indivs)
    
    all_indivs <- rownames(df$tab)
    
    keep_indivs_user_def <- setdiff(all_indivs, user_defined_drop_indivs)
    
    df <- df[(keep_indivs_user_def)]
    
    print(paste0("After removing user-selected individuals, the output genind has ", length(rownames(df$tab)), " individuals"))
    
  }  
    
  
  # Remove pops
  if(drop_by_pop_size==TRUE){
    pops_to_keep <- names(which(table(pop(df))[sort(names(table(pop(df))))] >= min_indiv )) # which pops to keep
    
    # Reporting
    print(paste0("Keeping pops with at least ", min_indiv , " individuals"))
    print(paste0("Dropping: ", setdiff(unique(pop(df)), pops_to_keep)))
    
    df <- df[pop=pops_to_keep]
    
    print(paste0("After filtering on low sample sizes, the output genind has ", length(unique(pop(df))), " pops"))
    
  }
  
  # Drop specific user-defined pops
  if(!is.null(drop_file)){
    
    # Reporting
    print("Reading in list of populations to drop, from the following file: ")
    print(drop_file)
    print(paste0("Before dropping, the genind has ",  length(unique(pop(df))), " pops"))
    
    user_defined_drop_file <- read.delim2(file = drop_file, header = F, stringsAsFactors = F)
    user_defined_drop_pops <- user_defined_drop_file$V1
    
    # Remove these loci
    print(paste0("Requested user-defined pops to drop, in total: ", length(user_defined_drop_pops), " pops"))
    print("Requested to drop:   ")
    print(user_defined_drop_pops)
    
    all_pops <- as.character(unique(pop(df)))
    
    keep_pops_user_def <- setdiff(all_pops, user_defined_drop_pops)
    
    df <- df[pop=keep_pops_user_def]
    
    print(paste0("After removing user-selected pops, the output genind has ", length(unique(pop(df))), " pops"))
    
  }
  
  
  # Drop specific repunits or regions
  if(drop_unused==TRUE){
    
    # Reporting
    
    # Read in stock code file
    names.dat.df <- read.delim(names.dat.FN, stringsAsFactors = FALSE,header = FALSE)
    
    user_defined_drop_file <- filter(names.dat.df,V2 %in% c(98,99))
    
    
    print("Reading in list of populations to drop, from: ")
    print(names.dat.df)
    print(paste0("Before dropping, the genind has ",  length(unique(pop(df))), " pops"))
    
    names_defined_drop_pops <- user_defined_drop_file$V1
    
    # Remove these loci
    print(paste0("Dropping user defined pops, in total: ", length(names_defined_drop_pops), " pops"))
    print("Dropping:   ")
    print(names_defined_drop_pops)
    
    all_pops <- as.character(unique(pop(df)))
    
    keep_pops_names_def <- setdiff(all_pops, names_defined_drop_pops)
    
    df <- df[pop=keep_pops_names_def]
    
    print(paste0("After removing user-selected pops, the output genind has ", length(unique(pop(df))), " pops"))
    
  }
  
  print("The output object will be saved as 'obj_pop_filt'")
  assign(x = "obj_pop_filt", value = df, envir = .GlobalEnv)
  assign(x = "drop_pops_file", value = drop_file, envir = .GlobalEnv)
  
}
