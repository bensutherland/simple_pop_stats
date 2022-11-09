# Calculate per sample percent missing data

percent_missing_by_ind <- function(df= obj){
  
  # How many markers are present? (this becomes the total used below for calc % missing)
  n.markers <- length(locNames(df))
  
  # Reporting
  print(paste0("The input genind has ", n.markers, " markers"))
  
  
  print("Per sample, now determining percent missing and total markers typed")
  
  # Convert to df
  obj.df <- genind2df(df)
  dim(obj.df)
  obj.df[1:5,1:5]
  
  # Replace pop ID with indiv ID
  obj.df$pop <- rownames(obj.df)
  
  # Add vector of percent missing per ind
  obj.df$ind.per.missing <- NA
  obj.df$ind.num.typed <- NA
  obj.df$tot.markers.present <- n.markers
  
  # Calculate per individual the percentage of missing data, and the total number of typed markers
  for(i in 1:(nrow(obj.df))){
    
    # Note: missing data are NA in this case, as result of genind2df()
    obj.df[i,"ind.per.missing"] <-  sum(is.na(obj.df[i,])) / n.markers
    obj.df[i,"ind.num.typed"]   <-  n.markers - sum(is.na(obj.df[i,]))
    
    
  }
  
  dim(obj.df)
  
  obj.df <- obj.df[,c("pop", "ind.per.missing", "ind.num.typed")]
  colnames(obj.df)[colnames(x = obj.df)=="pop"] <- "ind"
  head(obj.df)
  tail(obj.df)
  
  print("The output object will be saved as 'missing_data.df'")
  assign(x = "missing_data.df", value = obj.df, envir = .GlobalEnv)
  
}
