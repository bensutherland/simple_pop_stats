#install.packages("graph4lg")
#install.packages("car")
#install.packages("rgl")





load_genepop()


update_pop_names(sep_by = "collection", name_by = "stockname")


drop_loci(drop_monomorphic = TRUE)


keep_pops(keep_file = "W:/9_PBT/01_eulachon/reference_databases/beu_SNP_coastwide_v.2.0.0_2021-02-17/keep_pops.txt")
#drop_pops(df = obj_filt, drop_by_pop_size = TRUE, min_indiv = 35)

# 
pca_from_genind(data = obj_pop_filt
                , PCs_ret = 3
                , plot_eigen=TRUE
                , plot_allele_loadings=TRUE
                , colour_file = NULL
                , retain_pca_obj = TRUE
)       




repunits <- read.delim(file="W:/9_PBT/01_eulachon/reference_databases/beu_SNP_coastwide_v.2.0.0_2021-02-17/eu_repunits_full.txt")
stockcodes<- read.delim(file="W:/9_PBT/01_eulachon/reference_databases/beu_SNP_coastwide_v.2.0.0_2021-02-17/euStockCodesCU.txt")


joined <- merge(repunits,stockcodes,by="repunit",all.y=TRUE)


fst_mds <- as.data.frame(pca.obj$scores)
test <- genind2df(obj_pop_filt)
test$pop
fst_mds$collection <- as.vector(test$pop)

# if(filter_df==TRUE){
#   
#   joined <- filter(joined, region %in% filer_region)
#   all_pops <- as.character(unique(pop(obj)))
#   keep_pops_user_def <- intersect(all_pops, joined$collection)
#   obj <- obj[pop=keep_pops_user_def]
#   
# }
# 
library(graph4lg)
library(plotly)


# start_time = Sys.time()
# fst_obj <- mat_pw_fst(x=obj)
# 
# #saveRDS(fst_obj,file="sk_fst_v0.6.0.rds")
# #fst_obj <- readRDS(file="sk_fst_v0.6.0.rds")
# end_time = Sys.time()
# 
# 
# fst_mds <- cmdscale(d=fst_obj, k=3)


fst_mds_df <- as.data.frame(fst_mds,stringsAsFactors = FALSE)
#fst_mds_df$collection <- rownames(fst_mds_df)


fst_mds_df <- merge(fst_mds_df,joined,by="collection",all.x=TRUE)



fig <- plot_ly(fst_mds_df,x=~PC1,y=~PC2,z=~PC3,text=~collection,color = ~collection)
fig <- fig %>% add_markers()
fig
