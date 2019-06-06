# Load data from genepop
#  datatype can be set as "SNP" or "microsat"

load_genepop <- function(datatype = "SNP"){
  
  # Manually choose files (function depends windows or linux)
  if(.Platform$OS.type == "unix") {
    
    my_genepop.path <- tk_choose.files(caption = "Select a genepop file" )
    
  } else if(.Platform$OS.type == "windows") {
    
    my_genepop.path <- choose.files( default=file.path("02_raw_data/")
                                     , caption = "Select a genepop file")
    
  }
  
  
  # Set allele.code
  if(datatype=="SNP"){
    allele.code <- 2
  }else if(datatype=="microsat"){
    allele.code <- 3
  }
  
  # Read in data
  print(paste0("Loading genepop from ", my_genepop.path))
  
  obj <- read.genepop(file = my_genepop.path
                      , ncode = allele.code)
  
  print(obj)
  
  assign(x = "obj", value = obj, envir = .GlobalEnv)
  assign(x = "datatype", value = datatype, envir = .GlobalEnv)
  assign(x = "my_genepop.path", value = my_genepop.path, envir = .GlobalEnv)
  
}
