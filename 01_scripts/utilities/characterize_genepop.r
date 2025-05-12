# Characterize genepop details

characterize_genepop <- function(df = obj, pdf_width = 8, pdf_height = 5, cex_names = 0.5, main_name = FALSE, N=30 ){
  
  # View features
  print(paste0("Number of individuals: ", nInd(df)))
  
  print(paste0("Number of markers: ", nLoc(df)))
  
  print(paste0("Number of unique populations: ", nPop(df)))
  print(table(pop(df)))
  
  # Summaries
  print(paste0("Average samples per population: ", round(mean(table(pop(df))), digits = 2)))
  print(paste0("Standard deviation samples per population: ", round(sd(table(pop(df))), digits = 2)))
  
  # Plot sample size in baseline per population
  # Make filename
  if(exists("sep_by")){
    fn <- paste0(result.path, "/sample_size_per_", sep_by, "_by_", name_by, ".pdf")
  }else{
    fn <- paste0(result.path, "/sample_size_per_pop.pdf")
  }
  
  # Add main title? 
  if(main_name == TRUE){
    
    main_name <- basename(my_genepop.path)
    
  }else if(main_name == FALSE){
    
    main_name <- NULL
    
  }
  
  # Plot and save
  pdf(file = fn, width = pdf_width, height = pdf_height)
  par(mar=c(8,5,3,3))
  barplot(table(pop(df))[sort(names(table(pop(df))))]
          , las=2
          , ylab="Sample size"
          , main = main_name
  )
  abline(h = N, lty=2)
  dev.off()
  
  # Create text file output
  pop_size.df <- as.data.frame(x = table(pop(df)), stringsAsFactors = F)
  colnames(pop_size.df) <- c("population", "sample_size")
  
  # Save out a text file as well
  write.csv(x = pop_size.df, file = paste0(result.path, "per_pop_sample_size.csv"), row.names = F)
  
}
