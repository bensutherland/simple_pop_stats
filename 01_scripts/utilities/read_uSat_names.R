# This reads in the formatted Usat names file into a data frame
# Usage: 
# 1. Function to browse for stockfile
#     stocks<-read_uSat_names()
# 2. If user knows location of stockfile
#     stocks<-read_USat_names(FN="H:/stock_codes/sockeye/names.dat")
#
read_USat_names<-function(FN="") {
col_names<-c("StockName","rCode","sCode","RegionName","Comment")
    if (FN == "") FN <- choose.files()
      
    sFile<-read.fwf(FN,
                    widths = c(15,2,4,32,100),
                    col.names = col_names,
                    na ="")
# Trim spaces from data  
  sFile<-sFile %>% mutate_if(is.character, trimws)
# remove "NA"   
  sFile[sFile == "NA"]<-""
  return(sFile)
}
