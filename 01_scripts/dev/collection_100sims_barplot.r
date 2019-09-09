# Reads in a text file with results from 100% simulations
# Creates a horizontal bar graph limiting output to no more than 50/ page 
# outputs "round(NROW/50)" pages
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)

FN.in<-"C:/Users/wallacecg/Desktop/PBT_temp/COHO_BL/Coho_100_Sims_2019-09-03.txt"
Results.dir<-
collection_100<-read_tsv(FN.in)
out.dir<-dirname(FN.in)

N<- nrow(collection_100) 
npages<-round(N/50) # print no more than 50 collections to a gage
out.date<-format(Sys.Date(),"%Y-%m-%d")

par.save<-par()
# Fitting Labels 
par(las=2) # make label text perpendicular to axis
par(mar=c(5,15,4,2)) # increase y-axis margin.
collection_order<-collection_100 %>%
                  arrange(-sort_Order,desc(collection) )

collection_factor_order<-collection_order$collection

collection_order$split<-cut(1:N,npages,label=1:npages) # a manual process which looks best for now

for (i in 1:npages) {
plot.data <-collection_order %>%
            filter(split == i) %>%
            select(CU,collection,to_repunit,to_collection) %>%
            melt(measure.vars=c("to_repunit","to_collection")) 

plot.data$collection<-factor(plot.data$collection,levels=collection_factor_order)
tmp<-collection_order$CU
                     

# Print out each image - Data in reverse so fix Page label 
png(file=paste0(out.dir,"/03_results/FIG-Coho_100_P",npages+1-i,"_",out.date,".png"),
    width = 480,height=720)

P<-ggplot(data = plot.data,
            aes(x = collection,
            y = value,
            fill = variable,
            label = CU)
       ) +
        labs(title="", x="Population", y="Percent accurancy") +
        theme_classic() +  # No background or borders
        scale_y_continuous(expand = c(0, 0), limits = c(0,100),breaks = seq(0,100,10)) +  # Axis limits
        geom_col(position = "identity", # not stacked bars - overlayed
                 color="black", # outline bars
                 size=.2, # line widthin MM
                 show.legend=F) + # turn off legend
        scale_fill_manual(values=c('grey','white')) + 
        coord_flip() + # Horizontal bars
        geom_text(position = "identity", y=0,stat = "identity",size=2,hjust=-0.1) +
        geom_hline(yintercept=90,linetype = "dashed" )

print(P) 
dev.off() 
}
par(par.save)
