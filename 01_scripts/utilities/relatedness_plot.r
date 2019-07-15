# Calculate relatedness for each population

relatedness_plot <- function(data = "kinship_analysis_2019-07-15.Rdata", same_pops = TRUE, plot_by = "names"){
  
  # Read in data
  input.FN <- paste0("03_results/", data)
  print(paste0("Loading data from ", input.FN))
  load(input.FN)
  
  ## The results are in the following formats
  # relatedness: df w/ all pairwise est. of relatedness
  # 1. integer for pair #; 2. indiv 1 ID; 3. indiv 2 ID; 4. group assignment; 5-11. relatedness estim.
  # delta7 & delta8: df w/ delta7 & delta8 est. for relatedness estim. that use it
  # inbreeding: df w/ inbreeding est. per indiv., as used in relatedness est (one row per indiv)
  
  # See what variables are available for relatedness output
  head(output$relatedness, n = 5)
  
  # Extract the necessary parts
  print("Extracting relevant sections of output data")
  rel.wang <- output$relatedness[["wang"]]
  rel.ritland <- output$relatedness[["ritland"]]
  rel.quellergt <- output$relatedness[["quellergt"]]
  
  group <- output$relatedness$group
  
  # Make into df
  output.df <- as.data.frame(
    cbind(  rel.wang
            , rel.ritland
            , rel.quellergt
            , group
    )
    , stringsAsFactors = F)
  
  # Format sections of df
  output.df$rel.wang <- as.numeric(output.df$rel.wang)
  output.df$rel.ritland <- as.numeric(output.df$rel.ritland)
  output.df$rel.quellergt <- as.numeric(output.df$rel.quellergt)
  
  str(output.df)
  
  # Make two vectors showing the pops in the contrast
  output.df <- separate(data = output.df, col = group, into = c("pop1", "pop2"), sep = 2, remove = F) # split
  str(output.df)
  
  # compare pop1 and pop2 to identify if same
  output.df$same <- output.df$pop1==output.df$pop2
  

  # Are all pop comparisons to be kept, or only same-on-same?
  if(same_pops==TRUE){
    
    # Only keep same-on-same comparisons
    print("Keeping only same-on-same comparisons")
    output.df <- output.df[output.df$same==TRUE,]

  }else if(same_pops==FALSE){
    print("Keeping all population comparisons")
  }
  
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
    print("Keeping stock codes")
  }
  
  
  ## Set up a wrapper to plot all types
  datatypes <- c("wang", "ritland","quellergt")
  
  ## Loop to plot
  for(i in 1:length(datatypes)){
    datatype <- datatypes[i]
    
    pdf(file = paste0("03_results/", "relatedness_", datatype, "_", date, ".pdf"), width = 20, height = 8)
    par(mar=c(7,3,3,3))
    boxplot(output.df[, paste0("rel.", datatype)] ~ output.df$group
            #, col = comp_names.df$colour
            , las = 2
            , ylab = paste0("relatedness (", datatype, ")")
    )
    abline(h = 0, lty = 2)
    dev.off()
    
  }
  
}
