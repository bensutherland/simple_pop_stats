
genind.data <- obj_pop_filt

library("poppr")

# # Example
# data("Aeut")
# str(Aeut$other)
# 
# strata(Aeut) <- data.frame(other(Aeut)$population_hierarchy)
# table(strata(Aeut, ~Pop))
# table(strata(Aeut, ~Pop/Subpop, combine = FALSE))

# My data
unique_pops <- as.data.frame(unique(pop(genind.data)), stringsAsFactors = F)
colnames(unique_pops) <- "collection"
unique_pops

# make a file
write.table(x = unique_pops, file = "00_archive/unique_pops.txt", row.names = F, col.names = T, quote = F)

# Then manually edit the tab-delim file to include repunits, save as "00_archive/unique_pops_w_repunits.txt
strat.FN <- "00_archive/unique_pops_w_repunit.txt"

strat_obj <- read.table(file = strat.FN, header = T, sep = ",")

# Add a strat obj to the genind
pops <- as.data.frame(pop(genind.data), stringsAsFactors = F)
head(pops)
colnames(pops) <- "collection"

strat_and_pops <- merge(x = pops, y = strat_obj, by= "collection", sort = F, all.x = T)
tail(strat_and_pops)
tail(pops)

genind.data

genind.data$strata <- strat_and_pops
unique(genind.data$strata)

table(strata(genind.data, ~collection))
#table(strata(genind.data, ~collection/repunit, combine = FALSE))
table(strata(genind.data, ~repunit/collection, combine = FALSE))

# example with within = TRUE
# obj.amova <- poppr.amova(x = genind.data, hier = ~repunit/collection)

# within: logical. When this is set to TRUE (Default), variance within individuals are calculated as well. 
# If this is set to FALSE, The lowest level of the hierarchy will be the sample level. See Details below.
obj.amova.2 <- poppr.amova(x = genind.data, hier = ~repunit/collection, within = FALSE)
#obj.amova.3 <- poppr.amova(x = genind.data, hier = ~collection/repunit, within = FALSE) # FAILS, hierarchy wrong

obj.amova.pegas <- poppr.amova(x = genind.data, hier = ~repunit/collection, within = FALSE, method = "pegas")

obj.amova.collection.only <- poppr.amova(x = genind.data, hier = ~collection)

obj.amova.repunit.only <- poppr.amova(x = genind.data, hier = ~repunit)

# Significance testing
#set.seed(1999)
#obj.amova_signif <-