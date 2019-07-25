obj_pop_filt_renamed$tab[1:5,1:5]
unique(pop(obj_pop_filt_renamed))

# Get stock IDs
# gdata.df <- pop(obj_pop_filt_renamed)
# gdata.df <- as.data.frame(gdata.df, stringsAsFactors = F)
# head(gdata.df)
# colnames(gdata.df) <- "indiv"
# head(gdata.df)

# Get genotypes, will be one column per marker currently
data.hf <- genind2hierfstat(obj_pop_filt_renamed)
data.hf[1:5, ]

# Make six character
for(i in 2:ncol(data.hf)){
  data.hf[,i] <- str_pad(string = data.hf[,i], width = 6, side = "left", pad = "0")
}

data.hf[1:5,]

# split into two 
data_out <- NULL; data_all <- NULL

for(c in ncol(data.hf):2){
  data.hf <- data.hf %>%
    separate(col = colnames(data.hf)[c]
             , into = c(colnames(data.hf)[c], paste0(colnames(data.hf)[c], "_1")
             )
             , sep = 3
    )
  # print(head(data_out))
  # 
  # data_section <- cbind(data_out[,c], data_out[,c+1])
  # print(head(data_section))
  # colnames(data_section) <- c(colnames(data_out)[c], colnames(data_out[c]))
  # 
  # data_all <- cbind(data_section, data_all)
}

head(data.hf)
all_data.hf <- data.hf

all_data.hf$pop <- as.character(all_data.hf$pop)

# Convert to stock codes
# Read in stock codes
print("Reading in stock code file")
stock_codes.df <- read.delim2(file = sc.base)

# Change name slightly to remove underscores
rosetta <- merge(x = all_data.hf, y = stock_codes.df, by.x = "pop", by.y = "collection", sort = F, all.x = T)

all_data.hf <- rosetta
tail(all_data.hf)

# Missing data to 0
all_data.hf[is.na(all_data.hf)] <- 0
head(all_data.hf)

all_data.hf <- dplyr::select(all_data.hf, -c("pop", "repunit", "ProvState", "YLAT", "XLONG"))
require("dplyr")
all_data.hf <- select(all_data.hf, Code, everything())
head(all_data.hf)


# Write out
write.table(x = all_data.hf, file = "03_results/Demerelate_input.txt", quote = F, sep = "\t", col.names = F, row.names = F)

# Read in via readgenotypedata as 'related' format
print("Reading in as 'related' format")
my_data.related <- readgenotypedata(genotype.data = "03_results/Demerelate_input.txt")
names(my_data.related)

str(my_data.related$gdata)

# Need to make 'group' a two-digit value instead of having single digits, due to the way related handles it
my_data.related$gdata[,1] <- str_pad(string = my_data.related$gdata[,1], width = 2, side = "left", pad = "0")
str(my_data.related$gdata)

# coancestry analysis w/ related
output <- coancestry(genotype.data = my_data.related$gdata
                     , lynchrd = 2
                     , quellergt = 2
                     , wang = 2
) # all settings default from website


# Save out results
date <- format(Sys.time(), "%Y-%m-%d")
save.image(file = paste0("03_results/", "kinship_analysis_", date, ".Rdata"))
