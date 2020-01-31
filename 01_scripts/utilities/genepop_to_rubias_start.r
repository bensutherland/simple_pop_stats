# Convert genepop to rubias, but script depends on whether datatype is SNP or microsat
# Use sample_type = "reference" or "mixture"

genepop_to_rubias <- function(data = obj, sample_type = "reference"){
  
  if(datatype=="SNP"){
    
    genepop_to_rubias_SNP(data = data, sample_type = sample_type)
    
  }else if(datatype=="microsat"){
    
    genepop_to_rubias_microsat(data = data, sample_type = sample_type)
    
  }
  
}