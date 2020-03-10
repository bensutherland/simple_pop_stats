# Construct dendrograms based on genetic differentiation
# Note, if following readme, use boot_obj as "obj_filt"
# Note, if following readme, use matrix as "pairwise_wc_fst"

make_tree <- function(matrix = NULL
                              , tree_method = "NJ"
                              , bootstrap = FALSE, nboots = 10000, boot_obj = NULL
                              , dist_metric = "edwards.dist"
                              , separated = FALSE){
  
  if(tree_method=="NJ" && bootstrap==FALSE){
    
    # Construct unrooted tree with NJ
    nj.tree <- NJ(matrix)
    
    # Save out, create filename
    if(separated==TRUE){
      
      fn.plot <- paste0(result.path, "gen_diff_tree_nj_unrooted_", sep_by, "_by_", name_by, ".pdf")
      fn.tree <- paste0(result.path, "gen_diff_tree_nj_unrooted_", sep_by, "_by_", name_by, ".tre")
      
    }else{
      
      fn.plot <- paste0(result.path, "gen_diff_tree_nj_unrooted.pdf")
      fn.tree <- paste0(result.path, "gen_diff_tree_nj_unrooted.tre")
      
    }
    
    # Save plot
    pdf(file = fn.plot)
    plot(nj.tree, "unrooted"
         #, main=""
         #, cex = 1.3
    )
    dev.off()
    
    # Save tree
    write.tree(nj.tree, file=fn.tree)
    
  }
  
  if(tree_method=="NJ" && bootstrap==TRUE){
    
    if(separated==TRUE){
      
      fn.plot <- paste0(result.path, "gen_diff_tree_nj_unrooted_", sep_by, "_by_", name_by, "_", dist_metric, "_", nboots, "_boots.pdf")
      fn.tree <- paste0(result.path, "gen_diff_tree_nj_unrooted_", sep_by, "_by_", name_by, "_", dist_metric, "_", nboots, "_boots.tre")
      
    }else{
      
      fn.plot <- paste0(result.path, "gen_diff_tree_nj_unrooted_", dist_metric, "_", nboots, "_boots.pdf")
      fn.tree <- paste0(result.path, "gen_diff_tree_nj_unrooted_", dist_metric, "_", nboots, "_boots.tre")
      
    }
    
    # Save plot
    pdf(file = fn.plot)
    bootstrapped_tree <- aboot(x = boot_obj, dist = dist_metric, sample = nboots, strata = pop(boot_obj)
                               , tree = tree_method)
    dev.off()
  }else if(tree_method=="njs" && bootstrap==TRUE){
    
    if(separated==TRUE){
      
      fn.plot <- paste0(result.path, "gen_diff_tree_nj_unrooted_", sep_by, "_by_", name_by, "_", dist_metric, "_", nboots, "_boots.pdf")
      fn.tree <- paste0(result.path, "gen_diff_tree_nj_unrooted_", sep_by, "_by_", name_by, "_", dist_metric, "_", nboots, "_boots.tre")
      
    }else{
      
      fn.plot <- paste0(result.path, "gen_diff_tree_nj_unrooted_", dist_metric, "_", nboots, "_boots.pdf")
      fn.tree <- paste0(result.path, "gen_diff_tree_nj_unrooted_", dist_metric, "_", nboots, "_boots.tre")
      
    }
    
  }
    
  
  
    # Generate tree
    bootstrapped_tree <- aboot(x = boot_obj, dist = dist_metric
                               , sample = nboots, strata = pop(boot_obj)
                             , tree = tree_method
                             )
    
    # Save tree
    write.tree(phy = bootstrapped_tree, file = fn.tree)
    assign(x = "bootstrapped_tree", value = bootstrapped_tree, envir = .GlobalEnv)
    
  }
  








