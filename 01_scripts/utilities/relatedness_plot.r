# Plot relatedness metric
#  requires that relatedness_calc() function has been run, and a relatedness object is present (.Rdata)

relatedness_plot <- function(file = "relative_path"
                             , plot_by_group = TRUE
                             , same_pops = TRUE
                             , plot_by = "codes"
                             
                             , pdf_width = 7, pdf_height = 5){
  
  # Read in data
  print(paste0("Loading data from ", file))
  load(file)
  
  # Record date that this function was run
  date <- format(Sys.time(), "%Y-%m-%d")
  
  ## Note: the results are in the following formats: 
  #      relatedness: df w/ all pairwise est. of relatedness
  #      output$relatedness: pair.no, ind1.id, ind2.id, group, trioml, wang, lynchli, lynchrd, ritland, quellergt, dyadml
  #      output$delta7 (or delta8): df w/ delta7 estimator
  #      output$inbreeding: df w/ inbreeding est. per indiv., as used in relatedness est (one row per indiv)
  
  # See what variables are available for relatedness output
  #head(output$relatedness, n = 5)
  
  # Extract the necessary parts
  print("Extracting sections of the output data")
  rel.wang      <- output$relatedness[["wang"]]
  rel.ritland   <- output$relatedness[["ritland"]]
  rel.quellergt <- output$relatedness[["quellergt"]]
  
  # Per individual group designation (i.e., combination string of the two individuals)
  group <- output$relatedness$group
  
  # Make the extracted components into a df
  output.df <- as.data.frame(cbind(rel.wang, rel.ritland, rel.quellergt, group), stringsAsFactors = F)
  head(output.df)
  
  # Ensure numeric
  output.df$rel.wang      <- as.numeric(output.df$rel.wang)
  output.df$rel.ritland   <- as.numeric(output.df$rel.ritland)
  output.df$rel.quellergt <- as.numeric(output.df$rel.quellergt)
  #str(output.df)
  
  # Make two vectors showing the pops in the contrast
  print("These are the types of group comparisons: ")
  print(unique(output.df$group))
  output.df <- separate(data = output.df, col = group, into = c("pop1", "pop2"), sep = 2, remove = F) # split the four character into two separate two characters
  str(output.df)
  
  # Compare pop1 and pop2 to identify same-on-same comparisons
  output.df$same <- output.df$pop1==output.df$pop2
  head(output.df)
  dim(output.df)
  
  # Are all pairwise comparisons to be kept, or only same pop-on-same pop?
  if(same_pops==TRUE){
    
    # Only keep same-on-same comparisons
    print("Keeping only pairs for which both individuals are from the same population")
    output.df <- output.df[output.df$same==TRUE, ]

  }else if(same_pops==FALSE){
    
    print("Keeping all pairwise comparisons")
    
  }
  
  print(paste0("Number of pairwise comparisons to plot: ", nrow(output.df)))
  
  # If datatype is SNP, there are different plotting opts. If datatype is microsat, plotting opts are limited
  if(datatype == "SNP"){
    
    print("Datatype is SNP, continue")

      # Plot with code or stock name?
      if(plot_by == "names" & same_pops==TRUE){
    
      # Reporting
      print("Since both options of keeping only same pops and displaying as names were selected")
      print("...the stock codes for the same-on-same will be converted to names for display purposes")
    
      # Read in stock codes
      print("Reading in stock code file")
      stock_codes.df <- read.delim2(file = sc.base)
    
      # Change name slightly to remove underscores
      output.df$Code <- gsub(output.df$pop1, pattern = "_", replacement = "")
      head(output.df)
    
      # Merge
      rosetta <- merge(x = stock_codes.df, y = output.df, by = "Code", sort = F, all.y = T)
      rosetta$collection <- droplevels(rosetta$collection)
    
      output.df <- rosetta[, c("rel.wang", "rel.ritland", "rel.quellergt", "same", "Code", "collection")]
    
      colnames(output.df)[which(colnames(output.df)=="collection")] <- "group"
      head(output.df)
    
      } else if(plot_by=="codes" | same_pops=="FALSE"){
      
        print("Keeping population IDs as provided")
        
      }
    
  } else if(datatype=="microsat"){
    
    print("Your data is in microsat format... ")
    
    # Plot with code or stock name?
    if(plot_by == "names" & same_pops==TRUE){
      
      # Reporting
      print("Since both options of keeping only same pops and displaying as names were selected")
      print("...the stock codes for the same-on-same will be converted to names for display purposes")
      
      # Read in stock codes
      print("Reading in stock code file")
      stock_codes.df <- read.delim2(file = sc.base, stringsAsFactors = F)
      
      # Extract the two-digit code from the group vector
      output.df$Code <- substr(x = output.df$group, start = 1, stop = 2)
      output.df$Code<- as.numeric(output.df$Code)
      head(output.df)
      
      # Merge
      rosetta <- merge(x = stock_codes.df, y = output.df, by = "Code", sort = F, all.y = T)
      output.df <- rosetta[, c("rel.wang", "rel.ritland", "rel.quellergt", "same", "Code", "collection")]
      
      colnames(output.df)[which(colnames(output.df)=="collection")] <- "group"
      head(output.df)
      
    } else if(plot_by=="codes" | same_pops=="FALSE"){
      
      print("Keeping stock codes")
      
    }
    
  }
  
  
  ## Set up a wrapper to plot all types
  relatedness_metrics <- c("wang", "ritland","quellergt")
  
  # Report
  print(paste0("Plotting relatedness, output will be in ", result.path))
  
  # Give blank line
  cat("\n")
  
  ## Plot each metric
  metric <- NULL
  for(i in 1:length(relatedness_metrics)){
    
    # Set metric
    metric <- relatedness_metrics[i]
    
    pdf(file = paste0(result.path, "relatedness_", metric, "_", date, ".pdf")
        , width = pdf_width, height = pdf_height)
    par(mar=c(7,6,3,3))
    
    
    # Plotting a single boxplot distribution or by group? 
    if(plot_by_group==FALSE){
      
      print("Keeping all pairs together, not separating by group.")
      
      boxplot(output.df[, paste0("rel.", metric)]
              #, col = comp_names.df$colour
              , las = 2
              , ylab = paste0("Relatedness ("
                              , toupper(substring(metric, 1, 1)), substring(metric, 2)
                              , ")")
              , xlab = ""
      )
      abline(h = 0, lty = 2)
      
      # Reporting
      print(paste0("Summary statistics for ", toupper(substring(metric, 1, 1)), substring(metric, 2), " metric."))
      print(summary(output.df[, paste0("rel.", metric)]))
      print(sd(output.df[, paste0("rel.", metric)]))
      
    }else if(plot_by_group==TRUE){
    
      print("Plotting and summarizing by group.")
      
      boxplot(output.df[, paste0("rel.", metric)] ~ output.df$group
            #, col = comp_names.df$colour
            , las = 2
            , ylab = paste0("Relatedness ("
                            , toupper(substring(metric, 1, 1)), substring(metric, 2)
                            , ")")
            , xlab = ""
      )
      abline(h = 0, lty = 2)
      
      # Reporting
      print(paste0("Summarizing ***", toupper(substring(metric, 1, 1)), substring(metric, 2), "*** metric: "))
      
      pairing <- NULL; slice.df <- NULL
      for(g in 1:length(unique(output.df$group))){
        
        pairing <- unique(output.df$group)[g]
        
        print(paste0("Summary statistics for group: ", pairing))
        
        # Subset to this pairing type
        slice.df <- output.df[output.df$group==pairing, ]
        
        print(summary(slice.df[, paste0("rel.", metric)]))
        print(paste0("s.d. = ", round(sd(slice.df[, paste0("rel.", metric)]), digits = 4)))
        print(paste0("Number pairs in this group, n = ", nrow(slice.df)))
        
      }
      
    }
    
    dev.off()
    
    cat("\n")
    
  }
  
}
