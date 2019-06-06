# Characterize genepop details

characterize_genepop <- function(df = obj){
  
  # View features
  print("**Number of individuals and Number of marker-alleles**")
  print(dim(obj$tab))
  
  print("**How many populations?**")
  print(nPop(obj))
  print(unique(pop(obj)))
  print(table(pop(obj)))
  
  table(pop(obj))[sort(names(table(pop(obj))))] # if numeric, this will help
  
  print("**How many alleles per marker?**")
  print("**Note: when 1 allele, means marker is monomorphic**")
  print(table(nAll(obj)))
  
  # Plot sample size in baseline per population
  fn <- paste0("03_results/sample_size_per_", sep_by, "_by_", name_by, ".pdf")
  pdf(file = fn, width = 10, height = 5)
  par(mar=c(8,5,3,3))
  barplot(table(pop(obj))[sort(names(table(pop(obj))))]
          , las=2
          #, xlab="Stock_code"
          , ylab="Sample size"
          #, ylim = c(0,40)
          , main = basename(my_genepop.path)
  )
  abline(h = c(30), lty=2)
  dev.off()
  
}