library(adegenet)
library(HWxtest)
library(dartR)
library(StAMPP)
# To install SNPRelate
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("SNPRelate")
BiocManager::install("qvalue")

#PBTdata.genid<-read.genepop(file="S:/01_chinook/PBT/2019/PID20190012_Yukon_Juv/stats/PID20190012_V2_2019-07-12.gen")
#source("C:/00_Github/simple_pop_stats/01_scripts/simple_pop_stats_start.R")

file.sources = list.files("C:/00_Github/simple_pop_stats/01_scripts//utilities/", pattern="\\.r$",
                          full.names=TRUE, ignore.case=TRUE) 
# Source functions
for (pn in file.sources) {
  print(paste0("Sourcing ", pn))
  source(pn) 
}
rm(pn)

load_genepop(datatype = "SNP")

PBTdata.genid<-obj
PBTdata.genlight<-gi2gl(PBTdata.genid)
# Use StamPP to generate Fst Matrix
PBTdata.StamPP.FST<-stamppFst(PBTdata.genlight,nboots = 0)
# Use melt from libary reshape to make into pairwise
PBTdata.FST.Pair<-reshape::melt(PBTdata.StamPP.FST)
PBTdata.FST.Pair$X1<-as.character(PBTdata.FST.Pair$X1)
PBTdata.FST.Pair$X2<-as.character(PBTdata.FST.Pair$X2)

temp1<- select(PBTdata.FST.Pair,c(X1,X2,value))
names(temp1)<-c("Collection","P2","Fst")
temp2<- select(PBTdata.FST.Pair,c(X2,X1,value))              
names(temp2)<-c("Collection","P2","Fst")

PBTdata.FST.all<-rbind(temp1,temp2)
PBTdata.FST.all<- PBTdata.FST.all %>%
              drop_na(Fst) %>% ungroup(.)
    
rm(temp1,temp2)


PBTdata.FST<- PBTdata.FST.all %>%
                  group_by(Collection) %>%
                  summarise(Fst.sd=sd(Fst),Fst=mean(Fst))  %>%
                  ungroup(.)




#To test whether these numbers fit the Hardy-Weinberg proportions, call the function
PBTdata.hwx <- hwx.test(PBTdata.genid)

PBTdata.Hobs<-summary(PBTdata.genid)

PBTdata.genpop<-genind2genpop(PBTdata.genid)
PBTdata.freq<-makefreq(PBTdata.genpop,missing=0)


barplot(PBTdata.Hobs$Hexp-PBTdata.Hobs$Hobs, main="Heterozygosity: expected-observed", ylab="Hexp - Hobs")

PBTdata.H.out<-data.frame(PBTdata.Hobs$Hobs,PBTdata.Hobs$Hexp,PBTdata.Hobs$Hexp-PBTdata.Hobs$Hobs)
names(PBTdata.H.out)<-c("Hobs","Hexp","Hexp-Hobs")

library(pegas)
  ## conversion to pegas's format
  PBTdata.loci<-as.loci(PBTdata.genid)
  ## use Fst from pegas
  PBTdata.fst_by_locus <- Fst(PBTdata.loci)

  PBTdata.He_FST<-merge(PBTdata.H.out,PBTdata.fst_by_locus,by="row.names")
  PBTdata.He_FST[1:6,]
  