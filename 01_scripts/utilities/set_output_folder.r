# function to change the output folder to a custom area, for when doing analyses associated with a specific project
set_output_folder <- function(result_path = NULL){
  
  # Set the result path
  if(!is.null(result_path)){
    
    result.path <- result_path
  
  # Or else choose the result path interactively  
  }else if(is.null(result_path)){
    
    # Manually create output directory (function depends windows or linux)
    print("Select your genepop from file...")
    if(.Platform$OS.type == "unix") {
      
      print("This hasn't been set up for unix yet...")
      # The following needs to be updated to choose directorys
      # my_genepop.path <- tk_choose.files(caption = "Select a genepop file" )
      
    } else if(.Platform$OS.type == "windows") {
      
      output.DIR <- choose.dir(caption = "Select a folder to output data")
      
      # Overwrite default result path with the selected one
      result.path <- output.DIR
      
    }

  }
  
  # Reporting:   
  print(paste0("Your results will now be saved within ", result.path))
  assign(x = "result.path", value = result.path, envir = .GlobalEnv)
  
}