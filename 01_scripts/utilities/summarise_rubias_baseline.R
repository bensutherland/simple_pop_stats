# To summarize a rubias baseline by year for publication purposes
# Assumes any necessary filtering has been done - see "clean_format_rubias" function for more on this
# Will filter by repunit if the repunit_desc file does not contain all the repunits


#repunit_desc <- read.delim(file="H:/Stock_Codes/chinook/repunits_full.txt", stringsAsFactors = FALSE)


summarise_rubias_baseline <- function(out_prefix = "rubias_base_summary",
                               by_year=FALSE){
  library(readr)
    

    
    # If not, select the file
    base.fn <- choose.files(caption = "Select a rubias formatted base file")
    
    # Load the rubias base file
    baseline <- read_tsv(file=base.fn)
  
  
  
  

    
    # Reduce repunits
    repunit_desc.FN <- choose.files(getwd(),caption = "Path to repunits for analysis")
    
    # Load repunits file specified in config
    print("Loading Reporting Units Detail")
    repunits <- read_tsv(repunit_desc.FN)
    
  
  
  # Load necessary libraries
    library(dplyr)
    library(reshape2)
   
    # Create a backup of the baseline
    baseline_bck <- baseline

    # Split the individual ID to get the collection year
    baseline$year <- baseline$indiv %>%
                              strsplit("_") %>%
                              sapply( "[", 2 )
  
    # Reduce the dataframe to the necessary columsns
    baseline_reduced <- subset(baseline, select = c("collection","repunit","year"))
    
    # Dcast the table to summarize by year
    base_summary <- dcast(baseline_reduced,collection+repunit~year,value.var="year",fun.aggregate = length)

    # Count the number of fish in the "year" columns
    base_summary$total_N <- rowSums(base_summary[,3:length(colnames(base_summary))])

    # Add a column for the year string
    base_summary$Years <- NA

    # Create the year string per collection.
    for (i in 1:length(row.names(base_summary))){
      
          # Extract the line per collection
          work <- base_summary[i,]
          
          # Remove the first 2 columns repunit and collection
          work <- work[,3:length(work)]
          
          # Remove the last 2 columns Years and total_N
          work <- work[,-(length(work))]
          work <- work[,-(length(work))]
          
          # Remove any columns that are == 0
          work <- work %>% select_if(~any(. > 0))
          
          if(by_year==TRUE){
                for (j in 1:length(work)){
                      work[j] <- paste0(colnames(work)[j],"(",work[j],")")
                }
                
                base_summary$Years[i] <- paste(work,collapse=", ")
          } else {
            
            # Create a string of remaining column headers
            base_summary$Years[i] <- paste(colnames(work),collapse=", ")           
            
          }
            
    }

    # Merge the results with the repunits file
    base_summary <- merge(base_summary,repunits,by="repunit")

    # Order based on the Display_Order column
    base_summary <- base_summary[order(base_summary$Display_Order,base_summary$collection),]

    # Keep the columns of interest
    if("region" %in% colnames(base_summary)){
      base_summary <- subset(base_summary, select = c("CU_NAME","CU","region","collection","Years","total_N"))
      
      if(by_year==TRUE){
        # Rename the columns of interest
        colnames(base_summary) <- c("Repunit/Conservation Unit","CU Number","Region","Population","Years(N)","N")
      } else {
        # Rename the columns of interest
        colnames(base_summary) <- c("Repunit/Conservation Unit","CU Number","Region","Population","Years","N")
      }
      
      base_sum_orig <- base_summary
      # Blank out repeated repunit and CU numbers
      base_summary$`Repunit/Conservation Unit`[duplicated(base_summary$`Repunit/Conservation Unit`)] <- ""
      base_summary$`CU Number`[duplicated(base_summary$`CU Number`)] <- ""
      base_summary$`Region`[duplicated(base_sum_orig$`CU Number`)] <- ""
      
      
    } else {
      
      base_summary <- subset(base_summary, select = c("CU_NAME","CU","collection","Years","total_N"))
      if(by_year==TRUE){
        # Rename the columns of interest
        colnames(base_summary) <- c("Repunit/Conservation Unit","CU Number","Population","Years(N)","N")
      } else {
        # Rename the columns of interest
        colnames(base_summary) <- c("Repunit/Conservation Unit","CU Number","Population","Years","N")
      }
      
      # Blank out repeated repunit and CU numbers
      base_summary$`Repunit/Conservation Unit`[duplicated(base_summary$`Repunit/Conservation Unit`)] <- ""
      base_summary$`CU Number`[duplicated(base_summary$`CU Number`)] <- ""
      
    }
    

    


    # Output the dataframe
    write.table(base_summary,file=paste0("03_results/",out_prefix,".baseline_summary.txt")
                ,quote=FALSE,sep="\t",row.names=FALSE)

}
