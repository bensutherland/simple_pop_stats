# Determine whether the coding will occur on-site at PBS or off-site
setup_network <- function(){
  
  
    ans <- readline("Are you currently on the network? (y/n) or q: ")
  
    # Set TRUE/FALSE for the local variable
    if (ans=="y") {
      
      on_network=TRUE
      # Reporting
      print("Setting up paths on the network")
      
    }else if(ans=="n"){
      
      on_network=FALSE
      # Reporting
      print("Setting up paths off the network")
      
    }
    
    # Assign to global env
    assign(x = "on_network", value = on_network, pos = .GlobalEnv)
    
}
    
