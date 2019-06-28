# Rename microsat pops

fix_pop_names <- function(df = obj){
  print("Renaming populations")
  
  # Inclusively replace everything after first space in vector
  all_pops <- as.character(pop(obj))
  all_pops <- sub(pattern = " .*", replacement = "", x = all_pops)
  
  # Rename pop value in object
  pop(obj) <- all_pops
  
  assign(x = "obj", value = obj, envir = .GlobalEnv)
  
}
  