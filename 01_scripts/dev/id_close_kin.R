# Identify "close" relatives within groupings from relatedness analysis
#  can then be used to drop one individual of each pair until cutoff level of relatedness achieved
#  Ben J. G. Sutherland (2023-08-16)
# *** note: still in development ***

id_close_kin <- function(cutoff = 0.18, statistic = "ritland"){
  
  # Identify path of most recent relatedness file
  input.FN <- list.files(path = "03_results/", pattern = "pairwise_relatedness_output_all", full.names = T)
  input.FN <- input.FN[grep(pattern = ".txt$", x = input.FN)] # only keep .txt files
  input.FN <- head(sort(input.FN, decreasing = T), n = 1)     # only keep one input file, the latest
  
  # Reporting 
  print(paste0("Loading ", input.FN))
  
  # Read in input file
  input.df <- read.delim(file = input.FN, header = T, sep = "\t")
  head(input.df)
  
  # Identify unique groupings
  groups <- unique(input.df$group)
  
  # Keep only same-on-same, based on the two-letter characteristic
  groups <- groups[substr(x = groups, start = 1, stop = 2)==substr(x = groups, start = 3, stop = 4)]
  
  # Reporting
  print(paste0("Identifying pairs above the relatedness cutoff in the following groups: "))
  print(groups)
  
  
  # Identify pairs in each group
  drop.list <- list()
  
  for(i in 1:length(groups)){
    
    # Select out the group
    slice <- input.df[input.df$group==groups[i], ]
    
    # Identify which pairs are above the cutoff
    slice <- slice[slice[ ,statistic] > cutoff, ]
    
    drop.inds <- NULL
    
    # If there are no pairs above the threshold, skip this group
    if(nrow(slice)==0){
      
      print(paste0("No individuals to remove for the group ", groups[i]))
      
    # If there are any pairs above the threshold, move into the individual removal stage
    }else if(nrow(slice) > 0){
      
      
      # Remove indivs until no more over cutoff pairs remain
      for(r in 1:nrow(slice)){
        
        # If the first individual has already been designated to be removed, nothing needs to happen
        if(slice$ind1.id[r] %in% drop.inds){
          
          # This pair has been dealt with, as one of the inds is in drop.inds
          
          # If the first individual has NOT already been designated to be removed, but the second indiv has already been designated to be removed, 
          # again, nothing needs to happen
        }else if(slice$ind2.id[r] %in% drop.inds){
          
          # This pair has been dealt with, as one of the inds is in drop.inds
          
          # If neither indiv has yet been designated to be dropped, add the first individual to the drop list
        }else{
          
          # Add the first indiv to drop inds
          drop.inds <- c(drop.inds, slice$ind1.id[r])
          
        }
        
      }
      
      
      
    }
    
    
    
    drop.list[[groups[i]]] <- drop.inds
    
    
  }
  
  print("Saving out same-on-same drop list to remove one individual of each outlier pair as drop.list")
  
  assign(x = "drop.list", value = drop.list, envir = .GlobalEnv)
  
  
}