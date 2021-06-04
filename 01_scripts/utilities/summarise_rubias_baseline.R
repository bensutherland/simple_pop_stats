# To summarize a rubias baseline by year for publication purposes
# Assumes any necessary filtering has been done - see "clean_format_rubias" function for more on this
# Will filter by repunit if the repunit_desc file does not contain all the repunits

summarise_rubias_baseline <- function(baseline = NULL, repunit_desc = NULL
                                      , out_prefix = "rubias_base_summary", by_year=FALSE
                                      , type = "SNP",stock_code_desc = NULL){

    # Set baseline FN if defined
    base.fn <- baseline  
  
    # If the baseline is not defined, choose it
    if(is.null(base.fn)){
      
      # Select the file
      base.fn <- choose.files(default=paste0("W:/9_PBT/01_",species,"/reference_databases/*.*"),caption = "Select a rubias formatted base file")
      
    }
    
    # Load the baseline
    baseline <- read_tsv(file=base.fn,guess_max = 100000)
  
    
    # Set repunit FN if defined
    repunit_desc.FN <- repunit_desc
    
    # If the repunit is not defined, choose it
    if(is.null(repunit_desc.FN)){
      
      # Reduce repunits
      repunit_desc.FN <- choose.files(default=paste0("W:/9_PBT/01_",species,"/reference_databases/*.*"),caption = "Path to repunits for analysis")
      
    }
    
    # Load repunits file specified
    print("Loading Reporting Units Detail")
    repunits <- read_tsv(repunit_desc.FN)
    
    repunits$repunit <- gsub(" ","_",repunits$repunit)
    
    
    
    ### Can this be removed? ###
    # Create a backup of the baseline
    # baseline_bck <- baseline
    ### /END/ Can this be removed? /END/ ###
    
    # Get collection year
    if(type=="SNP"){
        
      # Split the individual ID to get the collection year
      baseline$year <- baseline$indiv %>%
                                  strsplit("_") %>%
                                  sapply( "[", 2 )
      
      baseline$stockcode <- baseline$indiv %>%
        strsplit("_") %>%
        sapply( "[", 1 )
    
    }else if(type=="microsat"){
        
        # Replace the 999 (unknown year) with a 4 digit version
        baseline$indiv <- gsub("_999_", "_9999_", baseline$indiv)
        
        # Find the 4 digit values, and keep the first as the "year"
        year.vec <- str_extract_all(string = baseline$indiv, pattern = "\\d{4}", simplify = T)
        # Add to baseline
        baseline$year <- as.data.frame(x = year.vec, stringsAsFactors = FALSE)[[1]]
        
        # If any years were missing (999 code or NA), replace with "unknown"
        baseline$year[baseline$year==""] <- "unknown"
        baseline$year[baseline$year=="9999"] <- "unknown"
        
        
        # Set stock_code FN if defined
        stock_code_desc.FN <- stock_code_desc
        
        # If the stock_code is not defined, choose it
        if(is.null(stock_code_desc.FN)){
          
          # Reduce repunits
          stock_code_desc.FN <- choose.files(default=paste0("W:/9_PBT/01_",species,"/reference_databases/*.*"),caption = "Path to stock code file created from names.dat in step 2")
          
        }
        
        # Load stock_code file specified
        print("Loading Reporting Units Detail")
        stock_code <- read_tsv(stock_code_desc.FN)
        
        names(stock_code)[names(stock_code) == "Code"] <- "stockcode"
        stock_code <- stock_code[ , -which(names(stock_code) %in% c("repunit"))]
        baseline <- merge(baseline,stock_code,by="collection",all.x=TRUE)
        
        
    }
    
    # Reduce the dataframe to the necessary columns
    cols_to_retain <- c("collection","repunit","year","stockcode")
    baseline_reduced <- subset(baseline, select = cols_to_retain)
    
    # Dcast the table to summarize by year
    base_summary <- reshape2::dcast(baseline_reduced, collection + stockcode + repunit ~ year
                                    , value.var="year"
                                    , fun.aggregate = length
                                    )

    # Count the number of fish in the "year" columns (starts counting at first year column)
    base_summary$total_N <- rowSums(base_summary[, length(cols_to_retain):ncol(base_summary)])

    # Add a column for the year string
    base_summary$Years <- NA

    # Prepare which columns need to be removed below
    cols_to_drop <- c("collection", "stockcode", "repunit", "total_N", "Years")
    
    # Create the year string per collection.
    for (i in 1:nrow(base_summary)){
      
          # Extract the line per collection
          work <- base_summary[i,]
          
          # Remove the first 2 columns repunit and collection
          work <- work[,-(which(colnames(work) %in% cols_to_drop))]
          
          # Remove any columns that are == 0
          work <- work %>% select_if(~any(. > 0))
          
          # Summarize by year if requested
          if(by_year==TRUE){
                for (j in 1:length(work)){
                      work[j] <- paste0(colnames(work)[j],"(",work[j],")")
                }
                
                base_summary$Years[i] <- paste(work, collapse=", ")
          } else {
            
            # Create a string of remaining column headers
            base_summary$Years[i] <- paste(colnames(work),collapse=", ")           
            
          }
            
    }

    # Collecting all results (SNP)
    if(type=="SNP"){
    
      # Merge the results with the repunits file
      base_summary <- merge(base_summary, repunits, by="repunit")

      # Order rows based on the Display_Order column
      base_summary <- base_summary[order(base_summary$Display_Order, base_summary$collection), ]

      # Keep the columns of interest if region is included
      if("region" %in% colnames(base_summary)){
        
        base_summary <- subset(base_summary, select = c("CU_NAME","CU","region","collection","stockcode","Years","total_N"))
      
        if(by_year==TRUE){
        
          # Rename the columns of interest
          colnames(base_summary) <- c("Repunit/Conservation Unit","CU Number","Region","Population","Stock Code","Years(N)","N")
          
        } else {
          
          # Rename the columns of interest
          colnames(base_summary) <- c("Repunit/Conservation Unit","CU Number","Region","Population","Stock Code","Years","N")
          
        }
      
        base_sum_orig <- base_summary
      
        # Blank out repeated repunit and CU numbers
        base_summary$`Repunit/Conservation Unit`[duplicated(base_summary$`Repunit/Conservation Unit`)] <- ""
        base_summary$`CU Number`[duplicated(base_summary$`CU Number`)] <- ""
        base_summary$`Region`[duplicated(base_sum_orig$`CU Number`)] <- ""
      
      # Keep the columns of interest if region is not included in base_summary
      } else {
      
        base_summary <- subset(base_summary, select = c("CU_NAME","CU","collection","stockcode","Years","total_N"))
        
        # If retained by-year counts
        if(by_year==TRUE){
          
          # Rename the columns of interest
          colnames(base_summary) <- c("Repunit/Conservation Unit","CU Number","Population","Stock Code","Years(N)","N")
        } else {
          
          # Rename the columns of interest
          colnames(base_summary) <- c("Repunit/Conservation Unit","CU Number","Population","Stock Code","Years","N")
        }
      
        # Blank out repeated repunit and CU numbers
        base_summary$`Repunit/Conservation Unit`[duplicated(base_summary$`Repunit/Conservation Unit`)] <- ""
        base_summary$`CU Number`[duplicated(base_summary$`CU Number`)] <- ""
      
      }
      
    # # Collecting all results (microsat)
    } else if(type=="microsat"){

      # Merge the results with the repunits file
      base_summary <- merge(base_summary, repunits, by="repunit")
      
      # Order based on the Display_Order column
      base_summary <- base_summary[order(base_summary$Display_Order, base_summary$collection), ]
      
      # Keep only the necessar cols
      base_summary <- subset(base_summary, select = c("repunit","collection","stockcode","Years","total_N"))
      
      # Rename cols depending on if count by year is on
      if(by_year==TRUE){
        # Rename the columns of interest
        colnames(base_summary) <- c("Region","Population","Stock Code","Years(N)","N")
      } else {
        # Rename the columns of interest
        colnames(base_summary) <- c("Region","Population","Stock Code","Years","N")
      }
      
      # Blank out repeated repunit and CU numbers
      base_summary$`Region`[duplicated(base_summary$`Region`)] <- ""

      
    }

    
    # Set output filename
    output.FN <- paste0(result.path, out_prefix,".baseline_summary.txt")
    
    # Reporting
    print(paste0("Exporting results to: ", output.FN))
    
    # Output the dataframe
    write.table(base_summary,file=output.FN
                , quote=FALSE, sep="\t", row.names=FALSE
                )

}
