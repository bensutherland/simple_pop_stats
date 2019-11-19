# Reads in a text file with results from 100% simulations
# Creates a horizontal bar graph limiting output to no more than 50/ page 
# outputs "round(NROW/50)" pages
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)

FN.in<-"C:/Users/wallacecg/Desktop/PBT_temp/COHO_BL/100% Sims/collection_sample_size_2019-09-10.txt"
plot.data<-read_tsv(FN.in)
out.dir<-dirname(FN.in)

out.date<-format(Sys.Date(),"%Y-%m-%d") # To include in output
plot.data$post_mean_pi_mean<-100*plot.data$post_mean_pi_mean
par.save<-par()
# Fitting Labels 
par(las=2) # make label text perpendicular to axis
par(mar=c(5,15,4,2)) # increase y-axis margin.

# Print out each image - Data in reverse so fix Page label 
pdf(file=paste0(out.dir,"/03_results/FIG-Coho_sim_model_P_",out.date,".pdf"))

# Limit 700
P<-ggplot(data = plot.data,
            aes(x = Reps,# * 100
            y = post_mean_pi_mean,
            group = collection)
       ) +
        labs(title="", x="Sample size", y="Percent accurancy") +
        theme_classic() +  # No background or borders
        geom_line(size=1) +
        geom_point(aes(shape=collection),size=8) +
        scale_shape_manual(values=c(1,3,16)) +
        scale_size_manual(values=c(15,15,15)) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,730),breaks = seq(0,700,100) ) +  # Axis limits
        theme(legend.position="none",text=element_text(size=40),axis.line = element_line(colour = 'black', size = 1.5))
      #  geom_text(position = "identity", y=0,stat = "identity",size=2,hjust=-0.1) +
      #  geom_hline(yintercept=90,linetype = "dashed" )

print(P) 
dev.off() 
#}
par(par.save)
