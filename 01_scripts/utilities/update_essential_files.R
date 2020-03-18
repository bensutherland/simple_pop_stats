# Method to copy the necessary files out of a directory and update the current 00_archive
# Currently set up to run after sourcing 'simple_pop_stats_start.R'

update_essential_files <- function(){
  
  # Identify the source and target folders
  source.folder <- paste0("C:/00_GitHub/essential_files/00_GSI_essentials", "/", species)
  target.folder <- "00_archive/"

  # Identify the files within the source folder
  list.of.files <- list.files(path = source.folder)
  
  # Copy the source files to the target folder
  file.copy(from = paste0(source.folder, "/", list.of.files), to = target.folder, overwrite = TRUE)
  
}
