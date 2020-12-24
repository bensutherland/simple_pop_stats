


highest_tray <- function(){
  
  
  library(readr)
  
  # Select the baseline with pop-up box - rubias format
  base.fn <- choose.files(caption = "Select a rubias formatted base file")
  
  # Load the rubias base file
  baseline <- read_tsv(file=base.fn,guess_max = 100000)
  
  # Load the baseline extraction sheet
  base.ES <- read.delim(ES.base,header=TRUE,sep="\t",quote="",stringsAsFactors = FALSE)
  
  
  
  # Turn the sex into letter code 
   base.ES$Sex <- gsub(1,"M",base.ES$Sex)
   base.ES$Sex <- gsub(2,"F",base.ES$Sex)
   base.ES$Sex <- gsub(0,"U",base.ES$Sex)
   base.ES$Sex[is.na(base.ES$Sex)] <- "U"
   
  # Create the indiv column
   base.ES$indiv <- paste0(base.ES$StockCode,"_",
                           base.ES$Year,"_",
                           base.ES$Fish,"_",
                           base.ES$Sex)
  
  # Keep only individuals in the baseline
  base.ES <- filter(base.ES, indiv %in% baseline$indiv)
  
  # Remove any trays that have been "9000" out
  base.ES <- filter(base.ES,Tray < c(9000))
  
  # Find the maximum tray number
  max_tray <- as.data.frame(max(base.ES$Tray))
  uniq_tray <- as.data.frame(unique(base.ES$Tray))
  
  # Give it a column name
  colnames(max_tray) <- "Highest_Tray_Number"
  colnames(uniq_tray) <- "Unique_Tray_Numbers"
  
  # Write out a summary table
  write_delim(max_tray,path=paste0(result.path,two.letter.code,"_highest_tray_number.txt"))
  write_delim(uniq_tray,path=paste0(result.path,two.letter.code,"_all_tray_numbers.txt"))
}
