# Load popmap and add sample attributes
# Sutherland Bioinformatics, 2024-03-17

annotate_from_popmap <- function(df = obj, popmap.FN = "00_archive/my_data_ind-to-pop_annot.txt"
                                 , convert_to_alt_ID = F){

  ## Load annotated df
  indiv_annot.df <- read.table(file = popmap.FN, header = T, sep = "\t")
  print(head(indiv_annot.df))
  
  ## Update pop attribute
  # Obtain sample names from df and keep as indiv.df
  indiv.df <- NULL
  indiv.df <- indNames(df)
  indiv.df <- as.data.frame(indiv.df)
  colnames(indiv.df) <- "indiv"
  
  # These are the two files that will be merged
  head(indiv.df)
  head(indiv_annot.df)
  
  # Merge the ordered sample names with updated population annotation, do not sort
  indiv_annot_in_order.df <- merge(x = indiv.df, indiv_annot.df, by = "indiv"
                                   , all.x = T, sort = FALSE # very necessary line
  )
  
  head(indiv_annot_in_order.df)
  tail(indiv_annot_in_order.df)
  
  # Observe order remained same as (x) above
  print("Ensure order is matched between the initial 'indiv' column, and the second 'indiv' column with all attributes")
  print(head(cbind(indiv.df, indiv_annot_in_order.df), n = 10))
  print(tail(cbind(indiv.df, indiv_annot_in_order.df), n = 10))
  print("If order not retained, do not proceed.")
  
  # Update the pop attribute from the ordered sample metadata
  pop(df) <- indiv_annot_in_order.df[, "pop"]
  
  # Convert to alternate ID if selected
  if(convert_to_alt_ID == TRUE){
    
    # Reporting
    print("Updating identifiers to alternate IDs")
    
    # Updating to alternate identifiers
    indNames(df) <- indiv_annot_in_order.df[, "alt.ID"]
    
    # Reporting
    print("Identifier format is now: ")
    print(head(unique(indNames(df))))
    
  }else{
    
    print("Retaining original identifiers")
    
  }
  
  # Reporting
  print(table((pop(df))))
  
  
  
  
  print("Saving annotated obj as obj_annot")
  assign(x = "obj_annot", value = df, envir = .GlobalEnv)
}
