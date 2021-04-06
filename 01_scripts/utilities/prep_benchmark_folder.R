# Prepare benchmark folder structure
# NOTE: SNP only so far...
prep_benchmark_folder <- function(df, mtype = "SNP", scope = "coastwide", version = "X.X.X") {
  
  # Set folder name
  folder.name <- paste0("b", two.letter.code
                        , "_", mtype
                        , "_", scope
                        , "_v.",  version
                        , "_", format(Sys.time(), "%Y-%m-%d")
                        )
  
  # Add path
  folder.path <- paste0("W:/9_PBT/01_", species, "/reference_databases/")
  folder.full.name <- paste0(folder.path, folder.name)
  
  # Create folder
  if(dir.exists(folder.full.name)){
    
    stop("This folder already exists, choose a different one")
    
  }else{
    
    print("Creating your new directory at: ")
    print(folder.full.name)
    
    # Create directory 
    dir.create(folder.full.name)
    
    # Create sub-directories
    dir.create(paste0(folder.full.name, "/Baseline_summary"))
    dir.create(paste0(folder.full.name, "/Dendrogram"))
    dir.create(paste0(folder.full.name, "/100_sims"))
    
    print("Adding the current stock code and repunit file to your benchmark directory")
    file.copy(from = sc.base, to = folder.full.name)
    file.copy(from = paste0("H:/Stock_Codes/", species, "/repunits_full.txt"), to = folder.full.name)
    
    print("Your new folder is ready to fill with your custom items")
    
  }
}
