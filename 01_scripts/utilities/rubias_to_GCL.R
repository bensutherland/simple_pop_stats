# Convert to GCL style formatting
rubias_to_GCL <- function(choose_sc.fn = TRUE){
    
  # Select the baseline file
  base.fn <- choose.files(default=paste0("W:/9_PBT/01_",species,"/reference_databases/*.*"),caption = "Select a rubias formatted base file")
  
  # Load the baseline
  baseline <- read_tsv(file=base.fn,guess_max = 100000)
  
  # Choose the repunits file
  repunit_desc.FN <- choose.files(default=paste0(base.fn),caption = "Path to repunits for analysis")
  # Load repunits file specified
  print("Loading Reporting Units Detail")
  
  # Load the repunits
  repunits <- read_tsv(repunit_desc.FN)
  
  # Create a "before" baseline
  baseline.bck <- baseline
  
  # filter baseline by repunits 
  baseline <- filter(baseline,repunit %in% repunits$repunit)

  # Check to see what was filtered out by repunit
  baseline_check <- filter(baseline.bck,!(repunit %in% repunits$repunit))
  # Total number repunits filtered and names
  print(paste0("Repunits in baseline not in repunits file (and therefore filtered):"))
  print(paste0("Total missing: ", length(unique(baseline_check$repunit))))
  print(paste0((unique(baseline_check$repunit))))
  
  # Reformat the baseline to define columns in the way GCL does
  baseline_new <- baseline
  baseline_new$FK_FISH_ID <- baseline_new$indiv
  baseline_new$COLLECTION_ID <- baseline_new$collection
  baseline_new$SILLY_CODE <- baseline_new$collection
  baseline_new$PLATE_ID <- NA
  baseline_new$PK_TISSUE_TYPE <- NA
  baseline_new$CAPTURE_LOCATION <- NA
  baseline_new$CAPTURE_DATE <- NA
  baseline_new$END_CAPTURE_DATE <- NA
  baseline_new$MESH_SIZE <- NA
  baseline_new$MESH_SIZE_COMMENT <- NA
  baseline_new$LATITUDE <- NA
  baseline_new$LONGITUDE <- NA
  baseline_new$AGENCY <- NA
  baseline_new$VIAL_BARCODE <- NA
  baseline_new$DNA_TRAY_CODE <- NA
  baseline_new$DNA_TRAY_WELL_CODE <- NA
  baseline_new$DNA_TRAY_WELL_POS <- NA
  baseline_new$CONTAINER_ARRAY_RYPE_ID <- NA
  baseline_new$SillySource <- baseline_new$indiv
  
  # Remove the first four columns, now unnecessary. Assumes data starts at column
  baseline_new <- baseline_new[,-c(1:4)]
  #https://stackoverflow.com/questions/28017141/select-the-last-n-columns-of-data-frame-in-r
  move_to_start <- function(x, n) {
    x[, c(tail(seq_len(ncol(x)), n), seq_len(ncol(x) - n))]
  } 
  
  # Move the new 19 columns to the front of the file
  baseline_new <- move_to_start(baseline_new, 19)
  
  # Create an appropriate filename, put it next to baseline selected
  filename <- gsub(".gz$","",base.fn)
  filename <- gsub(".txt$",".gcl",filename)
  
  # Write out the GCL file
  write_tsv(baseline_new,filename)
  
  # Find the stock codes - if off network, or in unusual place
  if(choose_sc.fn == TRUE){
    
    # Reduce repunits
    sc.base.FN <- choose.files(caption = "Path to stock codes file for analysis")
    # Load stock code file specified
    print("Loading stock code file")
    sc.base.df <- read_tsv(sc.base.FN)
  } else {
    sc.base.df <- read_tsv(sc.base)
  }
  
  sc.base.bck <- sc.base.df
  
  # Only keep those in the gcl file
  sc.base.df <- filter(sc.base.df,collection %in% baseline$collection)
  
  
  missing.sc <- filter(baseline_new, !(COLLECTION_ID %in% sc.base.bck$collection))
  
  # Total number collections missing from stock code file
  print(paste0("Stock Codes in baseline not in Stock Code file (will cause errors if any):"))
  print(paste0("Total missing: ", length(unique(missing.sc$COLLECTION_ID))))
  print(paste0((unique(missing.sc$COLLECTION_ID))))
  
  if((length(unique(missing.sc$COLLECTION_ID))) > 0){
    stop("missing collections in stock code file, stopping")
  }
  
  
  # Reformat column names to match GCL
  names(sc.base.df)[names(sc.base.df)=="collection"] <- "silly"
  names(sc.base.df)[names(sc.base.df)=="Code"] <- "map_no"
    names(sc.base.df)[names(sc.base.df)=="StockCode"] <- "map_no"
  names(sc.base.df)[names(sc.base.df)=="YLAT"] <- "Latitude"
  names(sc.base.df)[names(sc.base.df)=="XLONG"] <- "Longitude"
  
  # Generate a counts of collection and add in
  counts <- as.data.frame(table(baseline_new$COLLECTION_ID),stringsAsFactors = FALSE)
  colnames(counts) <- c("silly","n")
  sc.base.df <- merge(sc.base.df,counts)
  
  # Use display order column in repunits file to use as a factor
  reps_reduced <- repunits[,c("repunit","Display_Order")]
  sc.base.df <- merge(sc.base.df,reps_reduced)
  
  # Rename a few more columns
  names(sc.base.df)[names(sc.base.df)=="repunit"] <- "ReportingGroup"
  names(sc.base.df)[names(sc.base.df)=="Display_Order"] <- "Groupvec"
  sc.base.df$colorvec <- NA
  sc.base.df$Location <- sc.base.df$silly
  
  # Remove any hyphens in the factors
  sc.base.df$ReportingGroup <- gsub("-","_",sc.base.df$ReportingGroup)
  sc.base.df$ReportingGroup <- gsub("\\.","_",sc.base.df$ReportingGroup)
  
  # Reorder to match
  sc.base.df <- sc.base.df[, c("silly",
                   "map_no",
                   "Groupvec",
                   "colorvec",
                   "Latitude",
                   "Longitude",
                   "Location",
                   "n",
                   "ReportingGroup")]
  
  # Write out alongside baseline file
  filename_pops <- gsub(".gcl$","_gcl_pops.txt",filename)
  write_tsv(sc.base.df,filename_pops)
}
