library(adegenet)
library(HWxtest)

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
  