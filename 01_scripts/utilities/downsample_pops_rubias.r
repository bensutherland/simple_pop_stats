# Reduce populations to a constant size; subset_method can be "chosen", "average", "minimum"

downsample_pops_rubias <- function(rubias_data = "", subset_method = "chosen", 
                                   set_sample_size = 40, output_name = "rubias_base_subset"){
  
  
  if(rubias_data == ""){
      # If not, select the file
      base.fn <- choose.files(caption = "Select a rubias formatted base file")
  
      # Load the rubias base file
      rubias_data <- read_tsv(file=base.fn)
  }
  
  # Create a list of separated populations
  sep.obj <- split(rubias_data, f=rubias_data$collection)
  
  # Reporting
  print(paste0("You have separated a total of ", length(sep.obj), " populations from the input"))
  
  # Collect info on sample size per population
  sample_size <- NULL
  for(i in 1:length(sep.obj)){
    # Create vector of all sample sizes
    sample_size <- c(sample_size, nrow(sep.obj[[i]]))
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
    print(paste0("Working on ", unique(pop.oi$collection), " population"))
    
    # If a reduction is needed
    if(nrow(pop.oi) > set_sample_size){
      
      print("More than the set number of indiv, will reduce")
      
      # Reduce randomly
      temp_names <- pop.oi$indiv
      keep_names <- sample(temp_names, size = set_sample_size)
      
      pop.oi <- filter(pop.oi,indiv %in% keep_names)
      
    }else{
      
      print("Not reducing this population")
      pop.oi <- pop.oi
      
    }
    
    # Save the reduced pop into a list
    pop.name <- as.character(unique(pop.oi$collection))
    
    data.list[[pop.name]] <- pop.oi
    
  }
  
  subset_data <- data.frame()
  
  # Repool the subset data
  print("Repooling data")
  for(p in 1:length(data.list)){
    pop.merge <- data.list[[p]]
    subset_data <- rbind(subset_data,pop.merge)
  }  
    
  assign(x = output_name, value = subset_data, envir = .GlobalEnv)
  print("Your subset data is avail in obj_subset")
  
}
  
# write.table(subset_data,file="rubias_base_subset.txt",quote = FALSE,row.names = FALSE, sep='\t')






