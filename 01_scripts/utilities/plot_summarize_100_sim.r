#axis_label options include = "CU","CU_NAME","repunit"



plot_summarize_100_sim <- function(axis_label="repunit",repunits_file = TRUE,
                                   plot_prefix = "summarize_100_sim",plot_colls = FALSE,
                                   width=8,height=11,pdf_png="pdf"){

    # Find simulation files. 
    coll_sims.fn <-  choose.files( default=file.path("03_results/")
                                        , caption = "Select all_pops file")
    
    # Load in Sims - collection    
    coll_sims <- read_tsv(coll_sims.fn)

    
    # Find repunit simulation files
    rep_sims.fn <-  choose.files( default=file.path("03_results/")
                              , caption = "Select all_reps file")
    # Load in Sims - repunit
    rep_sims <- read_tsv(rep_sims.fn)
    
    # Merge together the repunit summary and collection summary file. 
    merged_sims <- merge(coll_sims, rep_sims, by=c("collection_scenario","repunit"))

    # Is there a repunits file - only really necessary if there is a regions header.
    if(repunits_file == TRUE){
    
        # Find repunits file - can be used to filter results
        repunit_desc.FN <- choose.files(getwd(),caption = "Path to repunits for analysis")
  
        # Load repunits file specified in config
        print("Loading Reporting Units Detail")
        repunits <- read_tsv(repunit_desc.FN)
        
        # Merge in the repunits file, and effectively filter for only those repunits in file. 
        ### Note - the way this works, "Region" will only calculate for retained repunits
        ### So, if you want to have a true representationof "region, 
        ### don't drop any repunits that aren't in the region
        ### This is a flaw, and probably best solved in future by moving the region calculation
        ### Directly to full_sim
        red_merged_sim <- merge(merged_sims,repunits,by="repunit")
        
    } else {
      
        # Otherwise, just rename the dataframe
        red_merged_sim <- merged_sims
      
    }
    
    
    # Only do the following if your repunits file has a "region" column - calculates a "to region" probability
    
    if(repunits_file==TRUE){
    
      if("region" %in% colnames(repunits)){

           # Aggregate the to-region probabilities per collection scenario
           df_temp <- aggregate(coll_post_mean_pi~region+collection_scenario,data=red_merged_sim,sum)
           
           # Rename the columns so that "reg_post_mean_pi" is properly defined.
           colnames(df_temp) <- c("region","collection_scenario","reg_post_mean_pi")
           
           # Merge it back into the simulation results so that reg_post_mean_pi is there
           red_merged_sim <- merge(red_merged_sim,df_temp,by=c("region","collection_scenario"))
        
      }
    }
    
    
    # Only keep lines where Collection == collection_scenario
    # Contains all necessary info for plotting.
    red_merged_sim <- red_merged_sim[red_merged_sim$collection_scenario == red_merged_sim$collection,]
    
    # Choose to write a table? Option this in later?
    #write.table(red_merged_sim,file="simulation_results.txt",sep="\t",quote=FALSE,row.names=FALSE)
    
    # TBD to incorporate Standard Error bars. I have the formula here, but not actually incorporated into script.
    # Function for standard error. 
    st.err <- function(x, na.rm=TRUE) {
      if(na.rm==TRUE) x <- na.omit(x)
      sd(x)/sqrt(length(x))
    }
    
    # If summarizing by repunit, do this. 
    if (plot_colls == FALSE){
        
        # If repunits file, look for regions. 
        if(repunits_file ==TRUE){
            # If the region column is there, roll-up the necessary columns. 
            if ("region" %in% colnames(repunits)){
                plot_temp <- aggregate(cbind(coll_post_mean_pi,repunit_post_mean_pi,reg_post_mean_pi) 
                                  ~ repunit, data=red_merged_sim,mean)
            } else {
              
              # Just roll up to repunit as originally planned. 
              plot_temp <- aggregate(cbind(coll_post_mean_pi,repunit_post_mean_pi) 
                                    ~ repunit, data=red_merged_sim,mean)
            }
        # If not, just do up to repunit as originally planned
        # This duplicates above, could probably be replaced by if(X and Y)
        } else {
            plot_temp <- aggregate(cbind(coll_post_mean_pi,repunit_post_mean_pi) 
                                  ~ repunit, data=red_merged_sim,mean)
      
        }
      
        # Count the number of collections per repunit
        plot_count <- aggregate(collection~repunit, data=red_merged_sim,FUN=length)
      
        # Rename the columns
        colnames(plot_count) <- c("repunit","count")
      
        # Add the counts back to the table
        plot_temp <- merge(plot_temp,plot_count)
    
    # If summarizing by collection, do this. 
    } else if (plot_colls == TRUE){
          
          # If repunits file, look for regions.
          if(repunits_file ==TRUE){
            
            # If the region column is there, roll-up the necessary columns. 
            # Also keeps the Display_Order for sorting downstream
            if ("region" %in% colnames(repunits)){
              plot_temp <- aggregate(cbind(coll_post_mean_pi,repunit_post_mean_pi,reg_post_mean_pi) 
                                    ~ collection + repunit + Display_Order, data=red_merged_sim,mean)
            } else {
          
              # If not, just do up to repunit as originally planned
              # Also keeps the Display_Order for sorting downstream
              plot_temp <- aggregate(cbind(coll_post_mean_pi,repunit_post_mean_pi) 
                                    ~ collection + repunit + Display_Order, data=red_merged_sim,mean)
            }
            # If not, just do up to repunit, no display order for sorting. 
          } else {
              plot_temp <- aggregate(cbind(coll_post_mean_pi,repunit_post_mean_pi) 
                                    ~ collection + repunit, data=red_merged_sim,mean)
        
          }
      
    }
    
    
    # Change the plotting so it just adds on the difference. This allows for plotting the "extra gain" from plotting the repunits. 
    plot_temp$repunit_post_mean_pi_extra <- plot_temp$repunit_post_mean_pi - plot_temp$coll_post_mean_pi
    
    # Same as above, but for region if it exists
    if(repunits_file==TRUE){
        if("region" %in% colnames(repunits)){
      
            plot_temp$reg_post_mean_pi_extra <- plot_temp$reg_post_mean_pi - plot_temp$repunit_post_mean_pi
        }
    }
    
    # Choose sorting method from options in Repunits file is a) summarizing by repunit, b) repunits is available. 
    if(repunits_file==TRUE && plot_colls == FALSE){
    
        # Merge the results with the repunits file
        plot_temp <- merge(plot_temp, repunits)
    
        # Use the preferred column as ploting name
        plot_temp2 <- plot_temp[,!(colnames(plot_temp)==axis_label)]
        rownames(plot_temp2) <- plot_temp[,(colnames(plot_temp)==axis_label)]
        
        plot_temp2 <- plot_temp2[(order(-plot_temp2$Display_Order)),]
    
    #Otherwise, just sort by repunit name.     
    } else if(repunits_file==FALSE && plot_colls == FALSE){
      
      axis_label <- "repunit"
      plot_temp2 <- plot_temp[,!(colnames(plot_temp)==axis_label)]
      rownames(plot_temp2) <- plot_temp[,(colnames(plot_temp)==axis_label)]
      
      plot_temp2 <- plot_temp2[(order(rownames(plot_temp2))),]
    
    # If plotting by collection, use the Display order that was added in earlie for sorting if possible.    
    } else if(plot_colls == TRUE){
      
      # Format the data for plotting.
      plot_temp2 <- plot_temp[,!(colnames(plot_temp)=="collection")]
      rownames(plot_temp2) <- plot_temp[,(colnames(plot_temp)=="collection")]
      
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
          
          plot_temp2[[i]]<-plot_temp2[[i]][nrow(plot_temp2[[i]]):1,]}
      }

    # Create the plot name using the prefix
    
    
    #pdf(file = paste0("03_results/",plot_prefix,"_plot.pdf"),width=width,height=height)
    
    # Set different margins for collection plotting or repunit plotting. 
    if (plot_colls == TRUE){
        # Set plotting margins
        if(pdf_png == "pdf"){
            pdf(file = paste0("03_results/",plot_prefix,"_plot.pdf"),width=width,height=height)
            par(mar=c(4,15,1,2))
        } 
    } else {
        # Set plotting margins
        if(pdf_png == "pdf"){
          pdf(file = paste0("03_results/",plot_prefix,"_plot.pdf"),width=width,height=height)
          
      } else if (pdf_png =="png"){
          png(filename=paste0("03_results/",plot_prefix,"_plot.png"),width = width, height=height,units = "in",res=400)
      }
      
        par(mar=c(4,9,1,2))
      
    }
    
    
    if(plot_colls==FALSE){
    if(repunits_file==TRUE){
      
      if("region" %in% colnames(repunits)){
          # If region exists, plot this way/
          # Create the main plot, retain y-values
          yy <- barplot(as.matrix(t(subset(plot_temp2, select=c("coll_post_mean_pi",
                                                                "repunit_post_mean_pi_extra",
                                                                "reg_post_mean_pi_extra"))))
                        ,xlab = "", ylab = "",horiz=TRUE,col=c("white","grey","dark grey")
                        ,axes = FALSE, las=1,xlim=c(0,1),cex.names=0.6)
          
          # Determine where counts should go. 
          xx <- plot_temp2$reg_post_mean_pi + 0.015
          
      
      } else {
          # If region doesn't exist, plot this way
          # Create the main plot, retain y-values
          yy <- barplot(as.matrix(t(subset(plot_temp2, select=c("coll_post_mean_pi",
                                                                "repunit_post_mean_pi_extra"))))
                        ,xlab = "", ylab = "",horiz=TRUE,col=c("white","grey")
                        ,axes = FALSE, las=1,xlim=c(0,1),cex.names=0.6)
          
          # Determine where counts should go. 
          xx <- plot_temp2$repunit_post_mean_pi + 0.015
        
      }
    } else {
    
      # If region doesn't exist, plot this way
      # Create the main plot, retain y-values
      yy <- barplot(as.matrix(t(subset(plot_temp2, select=c("coll_post_mean_pi",
                                                            "repunit_post_mean_pi_extra"))))
                    ,xlab = "", ylab = "",horiz=TRUE,col=c("white","grey")
                    ,axes = FALSE, las=1,xlim=c(0,1),cex.names=0.6)
      
      # Determine where counts should go. 
      xx <- plot_temp2$repunit_post_mean_pi + 0.015
    
    }
      
    # Add in a 90% threshold line
    segments(x0=0.9,y0=0,y1=max(yy),lty=2,xpd=FALSE)
    
    # Add in the collection counts
    text(x=xx, y = yy, label = plot_temp2$count,cex = 0.6,xpd=TRUE)
    
    # Add in x-axis and labels
    axis(side=1, at=seq(0,1,0.1),labels = seq(0,100,10),pos = 0,cex.axis=0.8)
    
    # Add in y-axis
    axis(side=2,labels=FALSE,tcl=0)
    
    # Add the X-axis title
    mtext("Percent accuracy",side=1, line=1, las=1)
    
    # Add the y-axis title
    mtext("CU or reporing group", side=2, line=6,las=0)

    
    } else if (plot_colls == TRUE){
      
      j <- 0
      for (i in plot_temp2){
        j <- j + 1
        
        if(pdf_png=="png"){
          png(filename=paste0("03_results/",plot_prefix,"_",j,"_plot.png")
              ,width = width, height=height,units = "in",res=400)
          par(mar=c(4,16,1,2))
        }
        
      if(repunits_file==TRUE){
        
        if("region" %in% colnames(repunits)){
          
          # Create the main plot, retain y-values
          yy <- barplot(as.matrix(t(subset(i, select=c("coll_post_mean_pi",
                                                                "repunit_post_mean_pi_extra",
                                                                "reg_post_mean_pi_extra"))))
                        ,xlab = "", ylab = "",horiz=TRUE,col=c("white","grey","dark grey")
                        ,axes = FALSE, las=1,xlim=c(0,1),cex.names=0.75)
          
          # Determine where counts should go. 
          xx <- i$reg_post_mean_pi + 0.015
        } else {
          # If region doesn't exist, plot this way
          # Create the main plot, retain y-values
          yy <- barplot(as.matrix(t(subset(i, select=c("coll_post_mean_pi",
                                                                "repunit_post_mean_pi_extra"))))
                        ,xlab = "", ylab = "",horiz=TRUE,col=c("white","grey")
                        ,axes = FALSE, las=1,cex.names=0.75, ylim=c(0,50),width=0.84)
          
          # Determine where counts should go. 
          xx <- i$repunit_post_mean_pi + 0.015
          
        }
      } else {
        
        # If region doesn't exist, plot this way
        # Create the main plot, retain y-values
        yy <- barplot(as.matrix(t(subset(i, select=c("coll_post_mean_pi",
                                                              "repunit_post_mean_pi_extra"))))
                      ,xlab = "", ylab = "",horiz=TRUE,col=c("white","grey")
                      ,axes = FALSE, las=1,xlim=c(0,1),cex.names=0.75, ylim=c(0,50),width=0.84)
        
        # Determine where counts should go. 
        xx <- i$repunit_post_mean_pi + 0.015
        
      }
      
      # Add in the collection counts
      #text(x=xx, y = yy, label = plot_temp2$count,cex = 0.6,xpd=TRUE)
      
      # Add in a 90% threshold line
      segments(x0=0.9,y0=0,y1=max(yy),lty=2,xpd=FALSE)
      
      # Add in x-axis and labels
      axis(side=1, at=seq(0,1,0.1),labels = seq(0,100,10),pos = 0,cex.axis=0.8)
      
      # Add in y-axis
      axis(side=2,labels=FALSE,tcl=0,pos=0)
      
      text(x=0.01, y=yy, label=i$repunit,cex=0.6,adj=0)
      
      
      # Add the X-axis title
      mtext("Percent accuracy",side=1, line=1, las=1)
      
      # Add the y-axis title
      #("Collection", side=2, line=6,las=0)
      
      if(pdf_png=="png"){
        dev.off() 
      }
      
      }
    
    }
    dev.off()
    
    }

