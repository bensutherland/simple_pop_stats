
genetic_repunits <- function(threshold_value = 0.85, manual_review_contrib = 7){

    library(dplyr)
  
    threshold_value = 0.80
    manual_review_contrib = 4

    setwd("W:/9_PBT/01_chinook/reference_databases/2020_V1.4_2020-10-07_baseline/100_sims/")
    getwd()
    
    repunits <- read.table(file="H:/Stock_Codes/Chinook/repunits_full.txt", sep='\t',quote = "", comment.char = "", header=TRUE, stringsAsFactors = FALSE)

    df <- read.table(file="collection_100_stats_all_pops_2020-10-13.txt", sep='\t',quote = "", comment.char = "", header=TRUE, stringsAsFactors = FALSE)
    
    # Create an empty dataframe
    reduced_df <- data.frame()
    too_many <- data.frame()
    
    for (coll in unique(df$collection_scenario)){
        
        # Reduce dataframe to collections
        df_temp <- filter(df, collection_scenario %in% coll)
        
        # Sort largest to smallest
        df_temp_sort <- df_temp[with(df_temp,order(-coll_post_mean_pi)),]
        
        # Find the index at which it exceeds threshold
        df_temp_sort<- df_temp_sort[unique(c(1:min(which(cumsum(df_temp_sort$coll_post_mean_pi) >= threshold_value)),which(df_temp_sort$coll_true_pi==1))),]

        if (nrow(df_temp_sort) < manual_review_contrib){
            # Add to a reduced dataframe
            reduced_df <- bind_rows(reduced_df,df_temp_sort)
        } else {
            too_many <- bind_rows(too_many,df_temp_sort)
        }
    }
    
    max(table(reduced_df$collection_scenario))
    reduced_df$gen_repunit_group <- NA
    length(unique(too_many$collection_scenario))
    
    
      
    for (i in 1:length(unique(reduced_df$collection_scenario))){  
    #for (i in 1:156){     
      
        contrib_pops <- NA
        coll <- NA
        rep_group <- NA
      
      
        coll <- unique(reduced_df$collection_scenario)[i]
        
        contrib_pops <- reduced_df$collection[reduced_df$collection_scenario == coll]
        
        if (length(contrib_pops) >= manual_review_contrib){
          
            reduced_df$gen_repunit_group[reduced_df$collection_scenario == coll] <- "man_review"
            
        } else if (all(is.na(reduced_df$gen_repunit_group[reduced_df$collection_scenario == coll]))){
          
            reduced_df$gen_repunit_group[reduced_df$collection %in% contrib_pops] <- i
            
        } else {
          
          rep_group <- unique(unlist(strsplit(as.character(na.omit(unique(reduced_df$gen_repunit_group[reduced_df$collection %in% contrib_pops]))),split = ",")))
          
          if(length(rep_group)==1){
              reduced_df$gen_repunit_group[reduced_df$collection %in% contrib_pops] <- rep_group
              reduced_df$gen_repunit_group[reduced_df$collection_scenario %in% coll] <- rep_group
          } else {
              
              for (j in 1:length(rep_group)){
                  reduced_df$gen_repunit_group[reduced_df$gen_repunit_group %in% rep_group[j]] <- i
                  reduced_df$gen_repunit_group[reduced_df$collection %in% contrib_pops] <- i
                  reduced_df$gen_repunit_group[reduced_df$collection_scenario %in% coll] <- i
              }  
          
          }
        }
        
    }
    
    
    for (k in 1:nrow(too_many)){
      if(!identical(unique(unlist(filter(reduced_df,collection %in% too_many$collection[k])[c("gen_repunit_group")])),character(0))){
        too_many$gen_repunit_group[k] <- unique(unlist(filter(reduced_df,collection %in% too_many$collection[k])[c("gen_repunit_group")]))
      } else {
        too_many$gen_repunit_group[k] <- NA
      }
        
    }
   

    
    new_groups <- unique(reduced_df[c("collection_scenario","gen_repunit_group")])
    too_many_groups <- unique(too_many[c("collection_scenario")])
    
    full_df <- merge(df,new_groups,by.x="collection",by.y="collection_scenario",all.x=TRUE)
    full_df_conf <- filter(full_df,collection_scenario %in% new_groups$collection_scenario)
    full_df_conf$class <- "auto"
    full_df_unsure <- filter(full_df,collection_scenario %in% too_many_groups$collection_scenario)
    full_df_unsure$class <- "man_review"
    max_repunit <- vector()
    
    for (coll in unique(full_df_unsure$collection_scenario)){
      df_temp <- filter(full_df, collection_scenario %in% coll)
      max_repunit <- unique(df_temp$gen_repunit_group)
      max_repunit <- as.data.frame(max_repunit)  
      for (i in 1:nrow(max_repunit)){
          sum_tmp <- filter(df_temp,gen_repunit_group %in% max_repunit$max_repunit[i])
          max_repunit$sum[i] <- sum(sum_tmp$coll_post_mean_pi)
      } 
      
      max_repunit<- na.omit(max_repunit)
      likely_rep <- as.numeric(as.character(max_repunit$max_repunit[which.max(max_repunit$sum)]))
      likely_rep_sum <- as.numeric(as.character(max_repunit$sum[which.max(max_repunit$sum)]))
      full_df_unsure$gen_repunit_group[full_df_unsure$collection == coll] <- likely_rep
      
    }
    
    full_df_conf <- rbind(full_df_conf,full_df_unsure)
    
    
    gu_df <- data.frame()
    
    for (coll in unique(full_df_conf$collection_scenario)){
      
      # Reduce dataframe to collections
      df_temp <- filter(full_df_conf, collection_scenario %in% coll)
      rep_group_temp <- df_temp$gen_repunit_group[df_temp$collection == coll]
      
      # Sort largest to smallest
      df_temp_sort <- df_temp[with(df_temp,order(-coll_post_mean_pi)),]
      
      # Find the index at which it exceeds threshold
      df_temp_sort$gu_pos_mean_pi <- max(cumsum(na.omit(df_temp_sort$coll_post_mean_pi[df_temp_sort$gen_repunit_group==rep_group_temp])))
      
      
      
      gu_df <- rbind(gu_df,df_temp_sort)
        
    }
    

     
    gu_df <- merge(gu_df,repunits,all.x=TRUE)
   
    gu_df_reduced <- filter(gu_df,coll_post_mean_pi >= 0.01)
    
    
    write.table(gu_df,file="collection_100_stats_all_pops_2020-07-03_gen_rep.txt",sep='\t',row.names = FALSE,quote = FALSE)
    write.table(gu_df_reduced,file="collection_100_stats_all_pops_2020-07-03_gen_rep_reduced.txt",sep='\t',row.names = FALSE,quote = FALSE)
    
    
    summary_gu <- gu_df[gu_df$coll_true_pi == 1,]
    
    summary_gu <- summary_gu[c("gen_repunit_group","coll_post_mean_pi","gu_pos_mean_pi","collection_scenario","CU_NAME","Display_Order")]
    summary_gu$Mean_Display_Order <- NA
    summary_gu$num_of_colls_in_gu <- NA
    
    for (row in 1:nrow(summary_gu)){
        
        mean_do <- filter(summary_gu,gen_repunit_group %in% summary_gu$gen_repunit_group[row])
        num_of_colls_in_gu <- nrow(mean_do)
        mean_do <- mean(mean_do$Display_Order)
        summary_gu$Mean_Display_Order[row] <- mean_do
        summary_gu$num_of_colls_in_gu[row] <- num_of_colls_in_gu
        
    }
    
    # Sort largest to smallest
    summary_gu <- summary_gu[with(summary_gu,order(Mean_Display_Order,gen_repunit_group)),]
    
    write.table(summary_gu,file="collection_100_stats_all_pops_2020-07-03_gen_rep_summary_gu.txt",sep='\t',row.names = FALSE,quote = FALSE)
    
    
    df_2 <- reshape2::dcast(df,collection_scenario~collection,value.var = "coll_post_mean_pi")
    row.names(df_2) <- df_2[,1]
    df_2 <- df_2[,-1]
    
    
    df_3 <- 1- df_2
    fit <- hclust(df_3[,1:100], method="ward")
    is.na(df_3)
} 
