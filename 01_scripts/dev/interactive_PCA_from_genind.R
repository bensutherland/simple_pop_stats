#install.packages("graph4lg")
#install.packages("car")
#install.packages("rgl")

library("graph4lg")


load_genepop()

sc.base <- "W:/9_PBT/01_sockeye/reference_databases/bsk_snp_coastwide_v.2.0.0_2025-01-07/Alsek/skStockCodesCU.txt"
repunits.base <- "W:/9_PBT/01_sockeye/reference_databases/bsk_snp_coastwide_v.2.0.0_2025-01-07/Alsek/repunits_full.txt"
update_pop_names(sep_by = "collection", name_by = "stockname",add_CU = TRUE)


drop_loci(drop_monomorphic = TRUE)


#keep_pops(df=obj_pop_filt,keep_file = "W:/9_PBT/01_sockeye/reference_databases/bsk_snp_fraser_v.0.0.2_2023-10-04/barkley_hend_keep.txt")
drop_pops(df = obj_filt, drop_by_pop_size = TRUE, min_indiv = 20)

#downsample_pops(data=obj_pop_filt,subset_method = "chosen",set_sample_size = 250)
#obj_pop_filt <- obj_subset
#make_tree(bootstrap = TRUE, boot_obj = obj_pop_filt, nboots = 10000, dist_metric = "edwards.dist", separated = TRUE)
#

#calculate_FST(dat=obj_pop_filt)
#calculate_FST(dat=obj_pop_filt,bootstrap = TRUE)

#make_tree(bootstrap = TRUE, boot_obj = obj_pop_filt, nboots = 10000, dist_metric = "nei.dist", separated = TRUE)

#drop_pops(df = obj_pop_filt, drop_file = "W:/9_PBT/01_chinook/reference_databases/bch_SNP_coastwide_v.5.0.0_2024-09-25/Skeena/drop_pops2.txt")

pca_from_genind(data = obj_pop_filt
                , PCs_ret = 3
                , plot_eigen=TRUE
                , plot_allele_loadings=TRUE
                , colour_file = NULL
                , retain_pca_obj = TRUE
)       




repunits <- read.delim(file=repunits.base)
stockcodes<- read.delim(file=sc.base)


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
fig <- fig %>% layout(scene=list(xaxis=list(title=paste0('PC1 (', round(pca.obj$eig[1] / sum(pca.obj$eig) * 100, digits = 1), '%)')),
                      yaxis=list(title=paste0('PC2 (', round(pca.obj$eig[2] / sum(pca.obj$eig) * 100, digits = 1), '%)')),
                      zaxis=list(title=paste0('PC3 (', round(pca.obj$eig[3] / sum(pca.obj$eig) * 100, digits = 1), '%)'))))       
fig
