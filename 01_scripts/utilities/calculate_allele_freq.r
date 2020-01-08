# Calculate allele frequencies on genind using adegenet


calculate_allele_freq <- function(data = obj){
  
  # Reporting
  print("Calculating allele frequencies from the input genind file")
  
  # Convert to genpop to calculate AF per pop instead of per indiv (which is what is done on genind file)
  obj.genpop <- genind2genpop(x = data)
  
  # Calculate per population AF
  freq.df <- makefreq(x = obj.genpop)
  
  # Transpose
  freq.df <- t(freq.df)
  
  # Make df
  freq.df <- as.data.frame(freq.df, stringsAsFactors = F)
  
  # View data
  freq.df[1:6,1:6]
  
  # Assign
  print("Your data is in the obj freq.df")
  assign(x = "freq.df", value = freq.df, envir = .GlobalEnv)
  
}
