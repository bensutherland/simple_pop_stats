# This writes a fixed formatted uSat names file from a data frame
# Data frame must have 5 columns:
#    col_names<-c("StockName","rCode","sCode","RegionName","Comment")

write_USat_names<-function (x, FN = "", append = FALSE, quote = FALSE, widths = c(15,2,4,32,100),
                             sep = "", na = "", rownames = FALSE, colnames = FALSE) 
                   {
                  if (!(is.data.frame(x) || is.matrix(x))) 
                       stop("'x' must be a data.frame or matrix")
                     if (length(na) > 1) 
                       stop("only single value can be defined for 'na'")
                     nRow <- nrow(x)
                     nCol <- ncol(x)
                     widthNULL <- is.null(widths)
# If widths not entered use defined width of column
                                          if (!widthNULL && length(widths) != nCol) {
                       warning("recycling 'widths'from file")
                       widthOld <- widths
                       widths <- integer(length = nCol)
                       widths[] <- widthOld
                     }

# remove "NA"   
                     x[x == "NA"]<-na

                     x$StockName<-stringr::str_pad(x$StockName ,widths[1] ,side = "right")
                     x$rCode<-stringr::str_pad(x$rCode ,widths[2] ,side = "left")
                     x$sCode<-stringr::str_pad(x$sCode ,widths[3] ,side = "left")

## The Region name field has space as first two Character
                     for (i in 1:nRow) {
                       x$RegionName[i]<-stringr::str_pad(x$RegionName[i],widths[4]-2, side="right")
                       x$RegionName[i]<-stringr::str_pad(x$RegionName[i],widths[4], side="left")
                     }
                     
                     write.table(x,
                                 FN,
                                 sep="",
                                 row.names = rownames,
                                 col.names = colnames,
                                 quote = F
                                 )
}                   