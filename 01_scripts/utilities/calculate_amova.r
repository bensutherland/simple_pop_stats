
# genind.data <- obj_pop_filt
# 
# library("poppr")
# 
# # Example
# data("Aeut")
# str(Aeut$other)
# 
# strata(Aeut) <- data.frame(other(Aeut)$population_hierarchy)
# table(strata(Aeut, ~Pop))
# table(strata(Aeut, ~Pop/Subpop, combine = FALSE))
# 
# 
# 
# # My data
# unique_pops <- as.data.frame(unique(pop(genind.data)), stringsAsFactors = F)
# colnames(unique_pops) <- "collection"
# unique_pops
# 
# # make a file
# write.table(x = unique_pops, file = "00_archive/unique_pops.txt", row.names = F, col.names = T, quote = F)
# 
# # Then manually edit the tab-delim file to include repunits
# strat.FN <- "00_archive/unique_pops.txt"
# 
# strat_obj <- read.table(file = strat.FN, header = T, sep = "\t")
# 
# 
# # Add a strat obj to the genind
# pops <- as.data.frame(pop(genind.data), stringsAsFactors = F)
# head(pops)
# colnames(pops) <- "collection"
# 
# strat_and_pops <- merge(x = pops, y = strat_obj, by= "collection", sort = F, all.x = T)
# tail(strat_and_pops)
# tail(pops)
# 
# genind.data
# 
# genind.data$strata <- strat_and_pops
# head(genind.data$strata, n = 20)
# 
# table(strata(genind.data, ~collection))
# #table(strata(genind.data, ~collection/repunit, combine = FALSE))
# table(strata(genind.data, ~repunit/collection, combine = FALSE))
# 
# obj.amova <- poppr.amova(x = genind.data, hier = ~repunit/collection)
# 
# # Significance testing
# set.seed(1999)
# #obj.amova_signif <- 