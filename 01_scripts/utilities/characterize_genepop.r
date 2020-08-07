# Characterize genepop details

characterize_genepop <- function(df = obj, pdf_width = 8, pdf_height = 5, cex_names = 0.5, main_name = FALSE ){
  
  # View features
  print("**Number of individuals and Number of marker-alleles**")
  print(dim(df$tab))
  
  print("**Number of Markers**")
  print((dim(df$tab)/2)[2])
  
  mark_num <- (dim(df$tab)/2)[2]
  
  write.csv(x = mark_num, file = paste0(result.path, "number_of_markers.csv"), row.names = F)
  
  print("**How many populations?**")
  print(nPop(df))
  print(unique(pop(df)))
  print(table(pop(df)))
  
  table(pop(df))[sort(names(table(pop(df))))] # if numeric, this will help
  
  print(paste0("There is an average of ", round(mean(table(pop(df))), digits = 2), " samples per pop"))
  print(paste0("There is an stdev of ", round(sd(table(pop(df))), digits = 2), " samples per pop"))
  
  print("**How many alleles per marker?**")
  print("**Note: when 1 allele, means marker is monomorphic**")
  print(table(nAll(df)))
  
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
          #, xlab="Stock_code"
          , ylab="Sample size"
          #, ylim = c(0,40)
          , main = main_name
          , cex.names = cex_names
  )
  abline(h = c(30), lty=2)
  dev.off()
  
  # Save out a text file as well
  write.csv(x = table(pop(df)), file = paste0(result.path, "per_pop_sample_size.csv"), row.names = F)
  
}