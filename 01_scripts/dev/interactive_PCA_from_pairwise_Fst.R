#install.packages("graph4lg")
#install.packages("car")
#install.packages("rgl")


filter_df=TRUE
filer_region <- c("SEAK","BritishColumbia","Washington")



load_genepop()

update_pop_names(sep_by = "collection", name_by = "stockname")


repunits <- read.delim(file="W:/9_PBT/01_chum/reference_databases/bcm_SNP_coastwide_v.5.0.1_2022-12-09_IYS/repunits_full_AK-IA.txt")
stockcodes<- read.delim(file="W:/9_PBT/01_chum/reference_databases/bcm_SNP_coastwide_v.5.0.1_2022-12-09_IYS/cmStockCodesCU.txt")

joined <- merge(repunits,stockcodes,by="repunit",all.y=TRUE)



if(filter_df==TRUE){
  
  joined <- filter(joined, region %in% filer_region)
  all_pops <- as.character(unique(pop(obj)))
  keep_pops_user_def <- intersect(all_pops, joined$collection)
  obj <- obj[pop=keep_pops_user_def]
  
}

library(graph4lg)
library(plotly)


start_time = Sys.time()
fst_obj <- mat_pw_fst(x=obj)

saveRDS(fst_obj,file="chum_fst_reduced_5.0.1.rds")
#fst_obj <- readRDS(file="chum_fst_reduced_5.0.1.rds")
end_time = Sys.time()


fst_mds <- cmdscale(d=fst_obj, k=3)


fst_mds_df <- as.data.frame(fst_mds,stringsAsFactors = FALSE)
fst_mds_df$collection <- rownames(fst_mds_df)


fst_mds_df <- merge(fst_mds_df,joined,by="collection",all.x=TRUE)



fig <- plot_ly(fst_mds_df,x=~V1,y=~V2,z=~V3,text=~collection,color = ~region)
fig <- fig %>% add_markers()
fig
