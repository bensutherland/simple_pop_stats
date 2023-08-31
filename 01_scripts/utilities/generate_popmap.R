# Create text file of individual names from genind file and write out for user to manually populate
#  will also simplify amplitools popnames
#  Only implemented for amplitools input so far
# Ben J. G. Sutherland, initialized 2023-08-29

generate_popmap <- function(df = obj){
 
  # Global variables
  pop_map.FN <- "00_archive/my_data_ind-to-pop.txt"
  
  # Collect individual names from input genepop
  indiv.df <- as.data.frame(indNames(obj))
  colnames(indiv.df) <- "indiv"
  
  # Add fields to manually complete
  indiv.df$pop    <- NA # population ID
  indiv.df$alt.ID <- NA # alternate sample name
  indiv.df$sex    <- NA # sex (include for potential parents for parentage applications)
  print("The form to complete is as follows: ")
  print(head(indiv.df))
  print("Note: alt.ID and sex columns are optional")

  # Reporting
  print(paste0("Writing out the population map file ", pop_map.FN, " to be completed manually."))
  
  # Write out empty file to provide pop names
  write.table(x = indiv.df, file = pop_map.FN
              , sep = "\t", col.names = T, row.names = F
              , quote = F
  )
  
}
