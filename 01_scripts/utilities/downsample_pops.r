# Reduce populations to a constant size; subset_method can be "chosen", "average", "minimum"

downsample_pops <- function(data = obj, subset_method = "chosen", set_sample_size = 40){
  
  # Create a list of separated populations
  sep.obj <- seppop(x = data)
  
  # Reporting
  print(paste0("You have separated a total of ", length(sep.obj), " populations from the input"))
  
  # Collect info on sample size per population
  sample_size <- NULL
  for(i in 1:length(sep.obj)){
    # Create vector of all sample sizes
    sample_size <- c(sample_size, nInd(sep.obj[[i]]))
  }
  
  
  # Determine how to decide the subset method and set the sample size accordingly
  if(subset_method=="chosen"){
    
    print(paste0("Using the set sample size of ", set_sample_size))
    
  }else if(subset_method=="average"){
    
    print("Using the average sample size of all pops")
    set_sample_size <- mean(sample_size)
    
  }else if(subset_method=="min"){
    
    print("Using the minimum sample size of all pops")
    set_sample_size <- min(sample_size)
    
  }
  
  
  # Per population, reduce the data
  # set nulls
  pop.oi <- NULL; temp_names <- NULL; keep_names <- NULL; data.list <- list(); pop.name <- NULL
  
  for(p in 1:length(sep.obj)){
    # Select the population being treated
    pop.oi <- sep.obj[[p]]
    print(paste0("Working on ", unique(pop(pop.oi)), " population"))
    
    # If a reduction is needed
    if(nInd(pop.oi) > set_sample_size){
      
      print("More than the set number of indiv, will reduce")
      
      # Reduce randomly
      temp_names <- indNames(pop.oi)
      keep_names <- sample(temp_names, size = set_sample_size)
      
      pop.oi <- pop.oi[keep_names, ]
      
    }else{
      
      print("Not reducing this population")
      pop.oi <- pop.oi
      
    }
    
    # Save the reduced pop into a list
    pop.name <- as.character(unique(pop(pop.oi)))
    
    data.list[[pop.name]] <- pop.oi
    
  }
  
  # Repool the subset data
  print("Repooling data")
  subset_data <- repool(data.list)
  
  assign(x = "obj_subset", value = subset_data, envir = .GlobalEnv)
  print("Your subset data is avail in obj_subset")
  
}
  







