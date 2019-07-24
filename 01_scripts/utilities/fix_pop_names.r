# Rename microsat pops

fix_pop_names <- function(df = obj){
  print("Renaming populations...")
  
  # Inclusively replace everything after first space in vector
  all_pops <- as.character(pop(obj))
  all_pops <- sub(pattern = " .*", replacement = "", x = all_pops)
  
  # Remove other characters
  all_pops <- sub(pattern = "/.*", replacement = "", x = all_pops)
  
  # Rename pop value in object
  pop(obj) <- all_pops
  
  print("Your populations are now named:   ")
  print(unique(pop(obj)))
  
  assign(x = "obj", value = obj, envir = .GlobalEnv)
  
}
  