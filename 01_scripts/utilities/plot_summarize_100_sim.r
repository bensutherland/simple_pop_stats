#axis_label options include = "CU","CU_NAME","repunit"



plot_summarize_100_sim <- function(axis_label="repunit",repunits_file = TRUE,
                                   plot_prefix = "summarize_100_sim"){

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

    # Function for standard error. 
    st.err <- function(x, na.rm=TRUE) {
      if(na.rm==TRUE) x <- na.omit(x)
      sd(x)/sqrt(length(x))
    }
    
    
    # If the region column is there, roll-up the necessary columns. 
    if(repunits_file ==TRUE){
    
        if ("region" %in% colnames(repunits)){
            plot_temp <- aggregate(cbind(coll_post_mean_pi,repunit_post_mean_pi,reg_post_mean_pi) 
                              ~ repunit, data=red_merged_sim,mean)
        } else {
          
          plot_temp <- aggregate(cbind(coll_post_mean_pi,repunit_post_mean_pi) 
                                 ~ repunit, data=red_merged_sim,mean)
        }
    # If not, just do up to repunit
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
    
    # Change the plotting so it just adds on the difference
    plot_temp$repunit_post_mean_pi_extra <- plot_temp$repunit_post_mean_pi - plot_temp$coll_post_mean_pi
    
    # Same as above, but for region if it exists

    if(repunits_file==TRUE){
        if("region" %in% colnames(repunits)){
      
            plot_temp$reg_post_mean_pi_extra <- plot_temp$reg_post_mean_pi - plot_temp$repunit_post_mean_pi
        }
    }
    
    if(repunits_file==TRUE){
    
        # Merge the results with the repunits file
        plot_temp <- merge(plot_temp, repunits)
    
        # Use the preferred column as ploting name
        plot_temp2 <- plot_temp[,!(colnames(plot_temp)==axis_label)]
        rownames(plot_temp2) <- plot_temp[,(colnames(plot_temp)==axis_label)]
        
        plot_temp2 <- plot_temp2[(order(-plot_temp2$Display_Order)),]
        
    } else {
      
      axis_label <- "repunit"
      plot_temp2 <- plot_temp[,!(colnames(plot_temp)==axis_label)]
      rownames(plot_temp2) <- plot_temp[,(colnames(plot_temp)==axis_label)]
      
      plot_temp2 <- plot_temp2[(order(rownames(plot_temp2))),]
      
    }
    # Re-order by display order (highest to lowest)

    
    pdf(file = paste0("03_results/",plot_prefix,"_plot.pdf"))
    
    # Set plotting margins
    par(mar=c(4,9,1,2))
    
    # If region exists, plot this way/
    
    if(repunits_file==TRUE){
    
      if("region" %in% colnames(repunits)){
      
          # Create the main plot, retain y-values
          yy <- barplot(as.matrix(t(subset(plot_temp2, select=c("coll_post_mean_pi",
                                                                "repunit_post_mean_pi_extra",
                                                                "reg_post_mean_pi_extra"))))
                        ,xlab = "", ylab = "",horiz=TRUE,col=c("white","grey","dark grey")
                        ,axes = FALSE, las=1,xlim=c(0,1),cex.names=0.75)
          
          # Determine where counts should go. 
          xx <- plot_temp2$reg_post_mean_pi + 0.015
      } else {
          # If region doesn't exist, plot this way
          # Create the main plot, retain y-values
          yy <- barplot(as.matrix(t(subset(plot_temp2, select=c("coll_post_mean_pi",
                                                                "repunit_post_mean_pi_extra"))))
                        ,xlab = "", ylab = "",horiz=TRUE,col=c("white","grey")
                        ,axes = FALSE, las=1,xlim=c(0,1),cex.names=0.75)
          
          # Determine where counts should go. 
          xx <- plot_temp2$repunit_post_mean_pi + 0.015
        
      }
    } else {
    
      # If region doesn't exist, plot this way
      # Create the main plot, retain y-values
      yy <- barplot(as.matrix(t(subset(plot_temp2, select=c("coll_post_mean_pi",
                                                            "repunit_post_mean_pi_extra"))))
                    ,xlab = "", ylab = "",horiz=TRUE,col=c("white","grey")
                    ,axes = FALSE, las=1,xlim=c(0,1),cex.names=0.75)
      
      # Determine where counts should go. 
      xx <- plot_temp2$repunit_post_mean_pi + 0.015
    
    }
    
    # Add in the collection counts
    text(x=xx, y = yy, label = plot_temp2$count,cex = 0.6,xpd=TRUE)
    
    # Add in a 90% threshold line
    cex.names = abline(v=0.9,lty=2)
    
    # Add in x-axis and labels
    axis(side=1, at=seq(0,1,0.1),labels = seq(0,100,10),pos = 0,cex.axis=0.8)
    
    # Add in y-axis
    axis(side=2,labels=FALSE,tcl=0)
    
    # Add the X-axis title
    mtext("Percent accuracy",side=1, line=2, las=1)
    
    # Add the y-axis title
    mtext("CU or reporing group", side=2, line=6,las=0)

    dev.off()
    
    }
    