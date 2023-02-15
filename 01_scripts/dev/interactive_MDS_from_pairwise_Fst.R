#install.packages("graph4lg")
#install.packages("car")
#install.packages("rgl")


# Filter for specific reporting units or regions
filter_df=FALSE

# Specify regions in a vector
#filter_region <- c("SEAK","BritishColumbia","Washington")


# Load a genepop - step 1 simple pop stats
load_genepop()



# update pop names - step 2 simple pop stats - don't add CUs to names
update_pop_names(sep_by = "collection", name_by = "stockname")


# Load in repunits and stock codes. If you want to change repunits/regions, use a custom repunits file
repunits <- read.delim(file="W:/9_PBT/01_eulachon/reference_databases/beu_SNP_coastwide_v.2.0.0_2021-02-17/eu_repunits_full.txt")
stockcodes<- read.delim(file="W:/9_PBT/01_eulachon/reference_databases/beu_SNP_coastwide_v.2.0.0_2021-02-17/euStockCodesCU.txt")

#Join 2 so all in same place
joined <- merge(repunits,stockcodes,by="repunit",all.y=TRUE)


#IF YOU ARE ADDING CUSTOM COLORS TO PLOT ON, SPECIFY THEM SOMEHOW IN THE REPUNITS OR JOINED FILE FOR PLOTTING BELOW

# If it is being filtered, (TRUE above). Change "region" below if you are filtering by another value (eg. repunit)
if(filter_df==TRUE){
  
  joined <- filter(joined, region %in% filter_region) #Change "region" to "repunit" to filter by repunit
  all_pops <- as.character(unique(pop(obj)))
  keep_pops_user_def <- intersect(all_pops, joined$collection)
  obj <- obj[pop=keep_pops_user_def]
  
}

library(graph4lg)
library(plotly)

# Run basic FST
start_time = Sys.time()
fst_obj <- mat_pw_fst(x=obj)

#saveRDS(fst_obj,file="sk_fst_v0.6.0.rds")
#fst_obj <- readRDS(file="sk_fst_v0.6.0.rds")
end_time = Sys.time()

# Run basic mds
fst_mds <- cmdscale(d=fst_obj, k=3)

#Convert to DF, format table
fst_mds_df <- as.data.frame(fst_mds,stringsAsFactors = FALSE)
fst_mds_df$collection <- rownames(fst_mds_df)

# Add the extra datalayer from repunits/stockcodes file
fst_mds_df <- merge(fst_mds_df,joined,by="collection",all.x=TRUE)


# Plot figure, choose column on which to color
# If you added a custom colour file above, presumably you would call that column in the line below I assume. 
fig <- plot_ly(fst_mds_df,x=~V1,y=~V2,z=~V3,text=~collection,color = ~repunit) #colour by "region" or "repunit. 
fig <- fig %>% add_markers()
fig
