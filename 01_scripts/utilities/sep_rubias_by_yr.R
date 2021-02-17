# Convert rubias baseline (SNP only) to year-separated

sep_rubias_by_yr <- function(rubias_base.FN = NULL, output.obj = "rubias_year_sep"){
  
  # Read in the rubias base
  rubias_base <- read_tsv(file = rubias_base.FN, guess_max = 100000)

  # Convert to data frame
  rubias.df <- as.data.frame(rubias_base)
  
  # Split indiv column into components
  rubias_sep.df <- separate(data = rubias.df, col = "indiv", into = c("tray", "yr", "ind", "sex")
           , sep = "_", remove = T)
  
  # Create new collection_year vector
  coll_year <- paste0(rubias_sep.df$collection, "_", rubias_sep.df$yr)
  
  # Reporting
  print("Your data now has the following collections, these will replace the input collections")
  print(unique(coll_year))
  
  # Bring in the coll_year instead of the collection
  rubias_base$collection <- coll_year
  
  # Assign output object name
  
  # Reporting
  print(paste0("Your results will be saved to the '", output.obj, "' object"))
  
  # Save out result to environment
  assign(x = output.obj, value = rubias_base, envir = .GlobalEnv)
  

  }
