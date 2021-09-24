rubias_to_vcf <- function(output.fn = "03_results/convert_from_rubias.vcf"){
  
  # Select Rubias File to Convert
  rubias.FN <- choose.files(caption = "Find the Rubias File you wish to convert")
  
  # Read in Rubias File
  rubias.df <- read_tsv(rubias.FN, guess_max = 100000,na = "NA")
  
  # Covert to dataframe - needed for the loop later
  rubias.df <- as.data.frame(rubias.df)
  
  # Identify start column of the genotypes
  gen_start_col <- which(colnames(rubias.df)=="indiv")+1 
  
  # Thank you: https://stackoverflow.com/questions/65182275/r-subtract-the-same-value-from-multiple-columns
  # converts the 1/2 into 0/1 for the VCF. Use this instead of gsub to allow for multi-allelic format (eg.  0,1,2)
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
  
  # Remove unneeded columns - do this here to avoid shifting the indices above
  output.df <- output.df[,-which(names(output.df) %in% c("collection","sample_type","repunit"))]
  
  # Transpose the dataframe and format the data
  names <- output.df$indiv
  output.df.t <- as.data.frame(t(output.df[,-1]))
  colnames(output.df.t) <- names
  output.df.t$ID <- rownames(output.df.t)
  
  # Just check that it worked.
  output.df.t[1:10,1:10]
  
  # We need the "bed" file, because we need the anchor to deal with insertions and deletions
  # you'll probably find this in the panels folder in W:\9_PBT
  hotspot_bed.FN <- choose.files(caption = "The hs.bed file used by the proton, NOT the hotspot.txt file - Check the W:/9_PBT/Panels folder")
  
  # Read in the file
  hotspot <- read_tsv(file=hotspot_bed.FN,guess_max=10000)
  
  # Add some headers - use opportunity to start some of the naming for VCF.
  # Note, I'm purposely choosing "ChromStart" even though it is off by one - will deal with this below in for loop
  # But this is a better way to deal with multi-base alleles. 
  colnames(hotspot) <- c("CHROM","POS","chromEnd","name","score","strand","alleles","other_name")
  
  # Read in the "normal" hotspot; this is needed because of bad reasons, mostly due to the way things have been iterated. 
  Hotspot_df <- read_tsv(hotspot.file,guess_max=1000)
  
  # Keep only needed columns
  Hotspot_df <- Hotspot_df[,which(names(Hotspot_df) %in% c("Allele Name","SNPFinal","CurrentSNP"))]
  
  # Merge it with the normal hotspot.bed
  hotspot <- merge(hotspot,Hotspot_df,by.x="name",by.y="Allele Name")
  
  # Keep only current SNPs
  hotspot <- filter(hotspot,CurrentSNP %in% TRUE)
  
  # Use the SNPFinal name as the ID
  names(hotspot)[names(hotspot) == 'SNPFinal'] <- 'ID'

  # Separate into the alleles; keep the anchor for now
  hotspot <-separate(data=hotspot,col=alleles,into=c("REF","ALT","ANCHOR"),sep=";")
  
  # Remove the naming from the alleles
  hotspot$REF <- gsub("REF=","",hotspot$REF)
  hotspot$ALT <- gsub("OBS=","",hotspot$ALT)
  hotspot$ANCHOR <- gsub("ANCHOR=","",hotspot$ANCHOR)
  
  # Deal with deletions and insertions format in VCF by using the ANCHOR.
  for (i in 1:nrow(hotspot)){
    
    
    if (hotspot$REF[i]==""){
      
      #If the deletion is in the REF
      hotspot$REF[i] <- hotspot$ANCHOR[i]
      hotspot$ALT[i] <- paste0(hotspot$ANCHOR[i],hotspot$ALT[i])
      # Don't change the POS - it is now correct!
      
    } else if (hotspot$ALT[i]==""){
      
      #If the deletion is in the ALT
      hotspot$ALT[i] <- hotspot$ANCHOR[i]
      hotspot$REF[i] <- paste0(hotspot$ANCHOR[i],hotspot$REF[i])
      # Don't change the POS - it is now correct!
      
    } else {
      
      # If the anchor didn't need to be added, the "ChromStart" needs to be changed from 0-base to 1-base
      hotspot$POS[i] <- hotspot$POS[i] + 1
      
    }
    
  }
  
  # Remove the hotspot columns we no longer need
  hotspot <- hotspot[,-which(names(hotspot) %in% c("chromEnd","score","strand","other_name","name","CurrentSNP","ANCHOR"))]
  
  # Merge it with the genotyping df to start the VCF
  vcf_start <- merge(output.df.t,hotspot,by="ID")
  
  # Add in some dummy columns needed in VCF format
  vcf_start$QUAL <- "."
  vcf_start$FILTER <- "."
  vcf_start$INFO <- "."
  vcf_start$FORMAT <- "GT"
  
  # Sort columns so they are in VCF format
  vcf_end <- vcf_start %>% select(CHROM,POS,ID,REF,ALT,QUAL,FILTER,INFO,FORMAT,everything())
  
  # Add the number sign to the CHROM header to match VCF format - doing it here to avoid having to use the "#" in things above  
  names(vcf_end)[names(vcf_end) == 'CHROM'] <- '#CHROM'

  # Create the VCF headers
  format_string <- as.data.frame('##fileformat=VCFv4.2')
  date_string <- as.data.frame(paste0('##fileDate=',format(Sys.time(), "%Y-%m-%d")))
  file_string <- as.data.frame(paste0('##reference=',rubias.FN))
  source_string <- as.data.frame(paste0('##source=rubias_to_VCF,simple_pop_stats'))
  format_string <- as.data.frame('##FORMAT=<ID=GT,Number=1,Type=String,Description="Genotype">')
  
  # Format the "missing data" entries
  gen_start_col_vcf <- which(colnames(vcf_end)=="FORMAT")+1 
  vcf_end[gen_start_col_vcf:ncol(vcf_end)][vcf_end[gen_start_col_vcf:ncol(vcf_end)] == "NA|NA"] <- ".|."
  
  # Write out the VCF file!
  write_tsv(format_string,output.fn,col_names = FALSE)
  write_tsv(date_string,output.fn,append=TRUE,col_names = FALSE)
  write_tsv(file_string,output.fn,append=TRUE,col_names = FALSE)
  write_tsv(source_string,output.fn,append=TRUE,col_names = FALSE)
  write_tsv(format_string,output.fn,append=TRUE,col_names = FALSE)
  write_tsv(vcf_end,output.fn,append=TRUE,col_names = TRUE)
  
}
