rubias_to_vcf <- function(output.fn = "03_results/convert_from_rubias.vcf"){
  
  # Select Rubias File to Convert
  rubias.FN <- choose.files()
  
  # Read in Rubias File
  rubias.df <- read_tsv(rubias.FN, guess_max = 100000,na = "NA")
  
  rubias.df <- as.data.frame(rubias.df)
  
  # Identify start column of the genotypes
  gen_start_col <- which(colnames(rubias.df)=="indiv")+1 
  
  # Thank you: https://stackoverflow.com/questions/65182275/r-subtract-the-same-value-from-multiple-columns
  rubias.df[gen_start_col:ncol(rubias.df)] <- rubias.df[gen_start_col:ncol(rubias.df)] -1
  
  # Identify the first column for each that contains the genotype
  odd_indices <- seq(gen_start_col, (ncol(rubias.df)-1), 2)
  
  # Start a null system
  j <- NULL; counter <- 0; as.data.frame(data_combined <- NULL)
  
  # Start the output dataframe with the the individual
  output.df <- rubias.df[c("collection","sample_type","repunit","indiv")]
  
  
  for(i in odd_indices){
    
    # Identify the even column
    j <- i + 1
    
    # Combine the two genotypes - Note, indiv should be last metadata column
    output.df[, (which(odd_indices==i)+gen_start_col-1)] <- as.data.frame(paste0(rubias.df[,i], "|", rubias.df[,j]))
    
    # Add in the colname of the odd column for the marker
    colnames(output.df)[(which(odd_indices==i)+gen_start_col-1)] <- colnames(rubias.df)[i]
    
  }
  
  output.df <- output.df[,-which(names(output.df) %in% c("collection","sample_type","repunit"))]
  
  names <- output.df$indiv
  output.df.t <- as.data.frame(t(output.df[,-1]))
  colnames(output.df.t) <- names
  output.df.t$ID <- rownames(output.df.t)
  
  output.df.t[1:10,1:10]
  
  hotspot <- read_tsv(file=hotspot.file,guess_max=10000)
  hotspot <- hotspot[,which(names(hotspot) %in% c("SNPFinal","Amplicon","Position","Ref","Variant"))]
  
  vcf_start <- merge(output.df.t,hotspot,by.y="SNPFinal",by.x="ID")
  
  vcf_start$QUAL <- "."
  vcf_start$FILTER <- "."
  vcf_start$INFO <- "."
  vcf_start$FORMAT <- "GT"
  
  vcf_end <- vcf_start %>% select(Amplicon,Position,ID,Ref,Variant,QUAL,FILTER,INFO,FORMAT,everything())
    
  names(vcf_end)[names(vcf_end) == 'Amplicon'] <- '#CHROM'
  names(vcf_end)[names(vcf_end) == 'Position'] <- 'POS'
  names(vcf_end)[names(vcf_end) == 'Ref'] <- 'REF'
  names(vcf_end)[names(vcf_end) == 'Variant'] <- 'ALT'
  
  format_string <- as.data.frame('##fileformat=VCFv4.2')
  date_string <- as.data.frame(paste0('##fileDate=',format(Sys.time(), "%Y-%m-%d")))
  file_string <- as.data.frame(paste0('##reference=',rubias.FN))
  source_string <- as.data.frame(paste0('##source=rubias_to_VCF,simple_pop_stats'))
  format_string <- as.data.frame('##FORMAT=<ID=GT,Number=1,Type=String,Description="Genotype">')
  
  gen_start_col_vcf <- which(colnames(vcf_end)=="FORMAT")+1 
  vcf_end[gen_start_col_vcf:ncol(vcf_end)][vcf_end[gen_start_col_vcf:ncol(vcf_end)] == "NA|NA"] <- ".|."
  
  
  write_tsv(format_string,output.fn,col_names = FALSE)
  write_tsv(date_string,output.fn,append=TRUE,col_names = FALSE)
  write_tsv(file_string,output.fn,append=TRUE,col_names = FALSE)
  write_tsv(source_string,output.fn,append=TRUE,col_names = FALSE)
  write_tsv(format_string,output.fn,append=TRUE,col_names = FALSE)
  write_tsv(vcf_end,output.fn,append=TRUE,col_names = TRUE)
  
}
