#axis_label options include = "CU","CU_NAME","repunit"



plot_summarize_LOO_sim <- function(axis_label="repunit",repunits_file = TRUE,
                                   plot_prefix = "summarize_LOO",plot_colls=FALSE,
                                   pdf_png = "pdf"){

    # Find simulation files - to collection 
    LOO_coll.fn <-  choose.files( default=file.path("03_results/")
                                        , caption = "Select all_pops file")
    
    # Load in Sims - collection    
    LOO_coll_sims <- read_tsv(LOO_coll.fn )

    
    
    # Find simulation files - to repunit
    LOO_repu.fn <-  choose.files( default=file.path("03_results/")
                             , caption = "Select all_pops file")
    
    # Load in Sims - collection    
    LOO_repu_sims <- read_tsv(LOO_repu.fn)
    
    
    
    # 
    # # Is there a repunits file - only really necessary if there is a regions header.
     if(repunits_file == TRUE){
 
         # Find repunits file - can be used to filter results
         repunit_desc.FN <- choose.files(getwd(),caption = "Path to repunits for analysis")
     
         #Load repunits file specified in config
         print("Loading Reporting Units Detail")
         repunits <- read_tsv(repunit_desc.FN)
         
     }  
    
    
    # Only keep lines where Collection == collection_scenario
    # Contains all necessary info for plotting.
    LOO_coll_sims <- LOO_coll_sims[LOO_coll_sims$collection == LOO_coll_sims$inferred_collection,]
    
    # Keep only lines where repunit = repunit
    LOO_repu_sims <- LOO_repu_sims[LOO_repu_sims$repunit == LOO_repu_sims$inferred_repunit,]
    
    # Rename the column names so that we can identify origin of column to repunits summary
    colnames(LOO_repu_sims) <- paste("Repu", colnames(LOO_repu_sims), sep = "_")
    
    # Merge the two dataframes
    red_merged_sim <- merge(LOO_coll_sims,LOO_repu_sims,by.x="collection",by.y="Repu_collection",all.y=TRUE)
    
    # If the "N" count was NA (eg. zero) because none of the fish were assigned to correcct collecction, replace with a 0. 
    red_merged_sim$n[is.na(red_merged_sim$n)] <- 0
    red_merged_sim$freq[is.na(red_merged_sim$freq)] <- 0
    
    
    # Choose to write a table? Option this in later?
    #write.table(red_merged_sim,file="simulation_results.txt",sep="\t",quote=FALSE,row.names=FALSE)
    
    if (plot_colls == FALSE){
        
        # Summarize to repunit
        plot_temp <- aggregate(cbind(freq,Repu_freq) 
                              ~ repunit, data=red_merged_sim,mean)
        
        # Count the number of collections per repunit
        plot_count <- aggregate(collection~repunit, data=red_merged_sim,FUN=length)
        
        # Rename the columns
        colnames(plot_count) <- c("repunit","count")
        
        # Add the counts back to the table
        plot_temp <- merge(plot_temp,plot_count)
        
        
    } else {
        
        red_merged_sim$repunit <- red_merged_sim$Repu_repunit
      
        # Summarize to collection - this really doesn't "aggregate" so much as reduce the dataframe
        plot_temp <- aggregate(cbind(freq,Repu_freq) 
                              ~ collection + repunit, data=red_merged_sim,mean)
      
    }
    
    
    # Change the plotting so it just adds on the difference
    plot_temp$Repu_freq_2 <- plot_temp$Repu_freq - plot_temp$freq
    
    # If the result goes down (eg. to collection is lower than to repunit (eg. coll A = 40, B + C =30 + 30 = 60))
    plot_temp$Repu_freq_2[plot_temp$Repu_freq_2<0] <- 0

    
     if(repunits_file==TRUE && plot_colls == FALSE){

        # Default axis_label
        axis_label <- "repunit"

        # Remove the axis_label column
        plot_temp2 <- plot_temp[,!(colnames(plot_temp)==axis_label)]
        
        # Add it back into the rownames for plotting
        rownames(plot_temp2) <- plot_temp[,(colnames(plot_temp)==axis_label)]
        
        # Add the rownames back in as "repunit"
        plot_temp2$repunit <- rownames(plot_temp2)
        
        # Merge the repunits file into the results
        plot_temp2 <- merge(plot_temp2,repunits,by.x="repunit")
        
        # Replace rownames lost during merge
        rownames(plot_temp2) <- plot_temp2$repunit
        
        # Re-order by display order (highest to lowest)
        plot_temp2 <- plot_temp2[(order(-plot_temp2$Display_Order)),]
     
     } else if (repunits_file==TRUE && plot_colls == TRUE){
       plot_temp2 <- plot_temp
       rownames(plot_temp2) <- plot_temp[,(colnames(plot_temp)=="collection")]
       
       reduced_reps <- repunits %>% select(Display_Order,repunit)
       
       
       plot_temp2 <- merge(plot_temp2,reduced_reps)
       rownames(plot_temp2) <- plot_temp2$collection
       
       # Sort by display order 
       if ("Display_Order" %in% colnames(plot_temp2)){
         plot_temp2 <- plot_temp2[(order(plot_temp2$Display_Order)),]
       } else {
         plot_temp2 <- plot_temp2[(order(plot_temp2$repunit)),]
       }
       
       # Split the data into units of 50, such that it doesn't compress the data too much. 
       plot_temp2 <- split(plot_temp2,rep(1:ceiling(nrow(plot_temp2)/50),each=50))
       
       # Reverse sort the lists - because the data plots from bottom up, which looks weird in the plot. 
       for (i in 1:length(plot_temp2)){
         #plot_temp2[[i]] <- plot_temp2[[i]][(order(-rownames(plot_temp2[[i]]))),]
         
         plot_temp2[[i]]<-plot_temp2[[i]][nrow(plot_temp2[[i]]):1,]
       }
     
     }

       
    if (plot_colls == TRUE){
      if (pdf_png == "pdf"){
            pdf(file = paste0("03_results/",plot_prefix,"_plot.pdf"),width=8,height=11)
      }
      
      # Set plotting margins
      par(mar=c(4,16,1,2))
    } else {
      
      if (pdf_png == "pdf"){
          pdf(file = paste0("03_results/",plot_prefix,"_plot.pdf"),width=8,height=11)
      } else if (pdf_png == "png"){
          png(filename=paste0("03_results/",plot_prefix,"_plot.png"),width = 8, height=11,units = "in",res=400)
      }
      
      # Set plotting margins
      par(mar=c(4,9,1,2))
      
    }
    
    if(plot_colls==FALSE){
        # If region doesn't exist, plot this way
        # Create the main plot, retain y-values
        yy <- barplot(as.matrix(t(subset(plot_temp2, select=c("freq",
                                                              "Repu_freq_2"))))
                      ,xlab = "", ylab = "",horiz=TRUE,col=c("white","grey")
                      ,axes = FALSE, las=1,xlim=c(0,100),cex.names=0.6,space=0.02)
    
        # Determine where counts should go. 
        xx <- plot_temp2$Repu_freq_2 + plot_temp2$freq + 1.5
        
        # Add in the collection counts
        text(x=xx, y = yy, label = plot_temp2$count,cex = 0.6,xpd=TRUE)
    
        # Add in a 80% threshold line
        segments(x0=80,y0=0,y1=max(yy),lty=2,xpd=FALSE)
    
        # Add in x-axis and labels
        axis(side=1, at=seq(0,100,10),labels = seq(0,100,10),pos = 0,cex.axis=0.8)
    
        # Add in y-axis
        axis(side=2,labels=FALSE,tcl=0,pos=0)
    
        # Add the X-axis title
        mtext("Percent accuracy",side=1, line=1, las=1)
    
        # Add the y-axis title
        mtext("CU or reporing group", side=2, line=8,las=0)

    } else {
      
      j <- 0
      
      for (i in plot_temp2){
        
        j <- j + 1
        
        if(pdf_png=="png"){
            png(filename=paste0("03_results/",plot_prefix,"_",j,"_plot.png")
                ,width = 8, height=11,units = "in",res=400)
            par(mar=c(4,16,1,2))
        }
        
        yy <- barplot(as.matrix(t(subset(i, select=c("freq",
                                                     "Repu_freq_2"))))
                      ,xlab = "", ylab = "",horiz=TRUE,col=c("white","grey")
                      ,axes = FALSE, las=1,xlim=c(0,100),ylim=c(0,50),width=0.84,cex.names=0.75)
        
        # Determine where counts should go. 
        xx <- i$Repu_freq_2 + i$freq + 1.5
        
        # Add in the collection counts
        #text(x=xx, y = yy, label = plot_temp2$count,cex = 0.6,xpd=TRUE)
        
        # Add in a 80% threshold line
        segments(x0=80,y0=0,y1=max(yy),lty=2,xpd=FALSE)
        
        # Add in x-axis and labels
        axis(side=1, at=seq(0,100,10),labels = seq(0,100,10),pos = 0,cex.axis=0.8)
        
        # Add in y-axis
        axis(side=2,labels=FALSE,tcl=0,pos=0)
        
        # Add in the repunit
        text(x=1, y=yy, label=i$repunit,cex=0.6,xpd=FALSE,adj=0)
        
        
        # Add the X-axis title
        mtext("Percent accuracy",side=1, line=1, las=1)
        
        # Add the y-axis title
        mtext("Collection", side=2, line=13,las=0)
        
        if(pdf_png=="png"){
          dev.off() 
        }
      }
    }    
    dev.off()
    
    }
    