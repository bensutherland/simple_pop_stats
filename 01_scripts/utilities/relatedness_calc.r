# Calculate relatedness for each population
# MGL_SNP means this is in the format of POP_YR_ID_SEX

relatedness_calc <- function(data = obj_pop_filt){
  
  # Convert genind to genlight using dartR
  print("Converting genind to genlight")
  obj.gl <- gi2gl(data, parallel = TRUE)
  
  # Convert genlight to demerelate.df using demerelate
  print("Converting genlight to demerelate.df")
  obj_demerelate.df <- gl2demerelate(gl = obj.gl)
  obj_demerelate.df[1:5,1:5]
  
  ## Relatedness: info on related input can be obtained here: https://rdrr.io/rforge/related/man/related-package.html
  ### genotype file should have one column of individual identifiers and 
  ### then 2 columns for each locus (one for each allele). No other columns are allowed. 
  ### Missing data should be formatted as zeros ("0").
  ### The file should NOT contain a header row.
  
  # Convert demerelate.df to related format
  print("Converting demerelate.df to related")
  
  # Remove pop column
  print("Removing column with title 'Population'")
  obj_demerelate.df <- obj_demerelate.df[, -which(colnames(x = obj_demerelate.df)=="Population")]
  
  # Convert NA to "0" for related format
  obj_demerelate.df[is.na(obj_demerelate.df)] <- 0 
  str(obj_demerelate.df[1:10,1:10])
  
  # Save out as text file to be able to read in via readgenotypedata
  print("Exporting for ease of import")
  write.table(x = obj_demerelate.df, file = "03_results/obj_demerelate.txt"
              , quote = F, sep = "\t", row.names = F, col.names = F)
  
  # Read in via readgenotypedata as 'related' format
  print("Reading in as 'related' format")
  my_data.related <- readgenotypedata(genotype.data = "03_results/obj_demerelate.txt")
  names(my_data.related)
  
  # coancestry analysis w/ related
  output <- coancestry(genotype.data = my_data.related$gdata
                       , lynchrd = 2
                       , quellergt = 2
                       , wang = 2
  ) # all settings default from website
  
  # Save out results
  date <- format(Sys.time(), "%Y-%m-%d")
  save.image(file = paste0("03_results/", "kinship_analysis_", date, ".Rdata"))
  
}
