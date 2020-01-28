# Custom; assumes obj is loaded from simple_pop_stats

data <- obj$tab

data[1:5,1:7]

# Order columns alphabetically
data <- data[ , order(colnames(data))]

data[1:5,1:7]

# Keep only the first allele
one_allele_data <- data[, grep(pattern = ".01", x = colnames(data), fixed = T)]

one_allele_data[1:5,1:5]

# Replace
for(i in 1:ncol(one_allele_data)){
  one_allele_data[,i] <- gsub(pattern = "0", replacement = "homo.alt", x = one_allele_data[,i])
  one_allele_data[,i] <- gsub(pattern = "1", replacement = "het", x = one_allele_data[,i])
  one_allele_data[,i] <- gsub(pattern = "2", replacement = "homo.ref", x = one_allele_data[,i])
  
  # NAs
  one_allele_data[is.na(one_allele_data[,i]),i] <- "missing"
  
}

one_allele_data[1:15,1:10]

# Remove the .01 in the name
colnames(one_allele_data) <- gsub(pattern = "\\.01$", replacement = "", perl = T, x = colnames(one_allele_data))

one_allele_data[1:15,1:10]

# Convert to rubias table
for(i in 1:ncol(one_allele_data))
one_allele_data[,i] <- ifelse(test = one_allele_data[,i]=="homo.ref", yes = "1 1"
                , no = ifelse(test = one_allele_data[,i]=="het", yes = "1 2"
                , no = ifelse(test = one_allele_data[,i]=="homo.alt", yes = "2 2"
                , no = ifelse(test = one_allele_data[,i]=="missing", yes = "NA NA"
                , no = "NA NA"
))))
one_allele_data[1:15,1:10]

#one_allele_data.bck <- one_allele_data
# one_allele_data <- one_allele_data.bck
one_allele_data[1:15,1:10]

# Convert to df
one_allele_data <- as.data.frame(one_allele_data, stringsAsFactors = F)
one_allele_data[1:15,1:10]

# Identify column names
colnames_to_change <- colnames(one_allele_data)

# Split into two cols in rubias format
colname1 <- NULL; colname2 <- NULL; 
for(i in 1:length(colnames_to_change)){
  
  # Set colnames for the column
  colname1 <- colnames_to_change[i]
  colname1 <- paste0(colname1)
  colname2 <- paste0(colname1, "_1")
  
  # Separate the data
  one_allele_data <- separate(data = one_allele_data
                              , col = which(colnames(one_allele_data)==colname1)
                              , into = c(colname1, colname2)
                              , sep = " "
                              , remove = T)
  
}

# output is in one_allele_data, and this is in rubias format minus the first five cols, which will just use the stock code file
