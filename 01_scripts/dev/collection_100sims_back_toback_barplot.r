library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)

FN.in<-"C:/Users/wallacecg/Desktop/PBT_temp/COHO_BL/Coho_100_Sims_2019-09-03.txt"
collection_100<-read_tsv(FN.in)
out.dir<-dirname(FN.in)

N<- nrow(collection_100) 

par.save<-par()
# Fitting Labels 
par(las=2) # make label text perpendicular to axis
par(mar=c(5,15,4,2)) # increase y-axis margin.
collection_order<-collection_100 %>%
                  arrange(-sort_Order,desc(collection) )

collection_factor_order<-collection_order$collection

collection_order$split<-cut(1:N,7,label=1:7) # a manual process which looks best for now

for (i in 7:1) {
plot.data <-collection_order %>%
            filter(split == i) %>%
            mutate(to_repunit=-to_repunit) %>%
            select(CU,collection,to_repunit,to_collection) %>%
            melt(measure.vars=c("to_repunit","to_collection")) 

plot.data$collection<-factor(plot.data$collection,levels=collection_factor_order)
tmp<-collection_order$CU
                     

# Print out ieach image
png(file=paste0(out.dir,"/03_results/FIG-Coho_100_P",i,"_2019-09-03.png"),
    width = 480,height=720)

breaks<-seq(-100,100,10)
names(breaks)<-abs(breaks)

ggplot(data = plot.data,
            aes(x = collection,
            y = value,
            fill = variable,
            label = CU)
       ) + 
#        theme_classic() +  # No background or borders
#        scale_y_continuous(expand = c(0, 0),breaks = breaks,labels=names(breaks)) +  # Axis limits
        geom_col(#position = "identity", # not stacked bars - overlayed
                 color="black", # outline bars
                 size=.2, # line widthin MM
                 show.legend=F) + # turn off legend
        scale_fill_manual(values=c('black','white')) + 
        geom_blank(aes(y = value * 1.05)) +
        coord_flip()# + # Horizontal bars
        facet_grid(~variable, scales = "free_x") +
#        geom_text(position = "identity", y=0,stat = "identity",size=2,hjust=-0.1) +
        geom_hline(yintercept=90)

print(P)
dev.off()
}

par(par.save)
