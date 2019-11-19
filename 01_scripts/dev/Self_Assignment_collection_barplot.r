# Reads in a text file with results from 100% simulations
# Creates a horizontal bar graph limiting output to no more than 50/ page 
# outputs "round(NROW/50)" pages
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)

SA.FN<-"C:/Users/wallacecg/Desktop/PBT_temp/COHO_BL/100_Sims/Coho_100_Sims_2019-09-03.txt"
RU.FN<-"H:/Stock_Codes/Coho/repunits_full.txt"
#SA.FN<-"C:/Users/wallacecg/Desktop/PBT_temp/COHO_BL/sa_summary_2019-09-12.txt"

RU.desc<-read_tsv(RU.FN)
SA_data<-read_tsv(SA.FN)
Results.dir<-paste0(dirname(SA.FN),"/03_results")

out.dir<-dirname(SA.FN)
SA_data<-inner_join(SA_data,RU.desc)

N<- nrow(SA_data) 
npages<-round(N/50) # print no more than 50 collections to a gage
out.date<-format(Sys.Date(),"%Y-%m-%d")

par.save<-par()
# Fitting Labels 
par(las=2) # make label text perpendicular to axis
par(mar=c(5,15,4,2)) # increase y-axis margin.
#SA_data$collection_scaled_like<-100*SA_data$collection_scaled_like
#SA_data$mean_repunit_li<-100*SA_data$mean_repunit_li

SA_order<-SA_data %>%
                  arrange(-Display_Order,desc(collection) )

SA_factor_order<-SA_order$collection

SA_order$split<-cut(1:N,npages,label=1:npages) # a manual process which looks best for now

SA_order$collection<-factor(SA_order$collection,levels=SA_factor_order)


pdf(file=paste0(out.dir,"/03_results/FIG-Coho_SA_P",npages,"_",out.date,".pdf"))

for (i in npages:1) {
plot.data <-SA_order %>%
            filter(split == i) %>%
            select(CU,collection,mean_repunit_li,collection_scaled_like) %>%
            melt(measure.vars=c("mean_repunit_li","collection_scaled_like")) 

plot.data$collection<-factor(plot.data$collection,levels=SA_factor_order)
tmp<-SA_order$CU

P<-ggplot(data = plot.data,
            aes(x = collection,
            y = value,
            fill = variable,
            label = CU)
       ) +
        labs(title="", x="Population", y="Percent accuracy") +
        theme_classic() +  # No background or borders
        scale_y_continuous(expand = c(0, 0), limits = c(0,105),breaks = seq(0,100,10)) +  # Axis limits
        geom_col(position = "identity", # not stacked bars - overlayed
                 color="black", # outline bars
                 size=.25, # line widthin MM
                 show.legend=F) + # turn off legend
        scale_fill_manual(values=c('grey','white')) + 
        coord_flip() + # Horizontal bars
        geom_text(position = "identity", y=0,stat = "identity",size=2,hjust=-0.1) +
        geom_hline(yintercept=90,linetype = "dashed" )

print(P) 
}
dev.off() # When printing to PDF

SA_order<-SA_data %>%
  arrange(-Display_Order,desc(collection) )

SA_factor_CU_order<-unique(SA_order$CU)

SA_order$CU<-factor(SA_order$CU,levels=SA_factor_CU_order)
# plot as above -rolled up to region
plot.data <-SA_order %>%
  select(CU,collection,mean_repunit_li,collection_scaled_like) %>%
  melt(measure.vars=c("mean_repunit_li","collection_scaled_like")) 

plot.d2 <- plot.data %>%
             group_by(CU,variable) %>%
             summarise(value=mean(value),n=n())
plot.d2$n<-ifelse(plot.d2$variable=="collection_scaled_like","",plot.d2$n)



#png(file=paste0(out.dir,"/03_results/FIG-Coho_SA_RU_",out.date,".png"),
#    width = 1920,height=3500)
pdf(file=paste0(out.dir,"/03_results/FIG-Coho_SA_RU_",out.date,".pdf"))
P<-ggplot(data = plot.d2,
          aes(x = CU,
              y = value,
              fill = variable,
              label = CU)
) +
  labs(title="", x="Conservation Unit", y="Percent accuracy") +
  theme_classic() +  # No background or borders
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,105),
                     breaks = seq(0,100,10)) +  # Axis limits
  geom_col(position = "identity", # not stacked bars - overlayed
           color="black", # outline bars
           size=.25, # line widthin MM
           show.legend=F) + # turn off legend
  scale_fill_manual(values=c('grey','white')) + 
#  theme(text = element_text(size=rel(8))) +
  coord_flip() + # Horizontal bars
  geom_text( label = plot.d2$n,
             size=2,
             nudge_y = 2,
             color="black") +
  geom_hline(yintercept=90,
             linetype = "dashed",
             color="black"
             ) 


print(P)
dev.off()


