# Convert vcf data (SNP) to GEMMA inputs
#  dev based on code used here: https://raw.githubusercontent.com/bensutherland/ms_cgig_chr8_oshv1/refs/heads/main/01_scripts/chr8_oshv1_amp_02_vcf_to_gemma.R
#  Ben J.G. Sutherland (SBIO), 2025-04-09
#  note: will only include individuals in output that are included in the provided phenotype file
vcf_to_gemma <- function(vcf_path = "", pheno_path = "", pheno_type = "binary"
                                  ){
  # Reporting
  print("Converting VCF file (SNP) to GEMMA input format")
  
  # Load input VCF
  print(paste0("Loading VCF file from ", vcf_path)) # reporting
  my_vcf <- read.vcfR(file = vcf_path)
  print(my_vcf)
  
  # Extract genotypes
  geno <- extract.gt(x = my_vcf, element = "GT")
  #geno[1:5, 1:5]
  
  # Extract positional info
  positional_info.df <- my_vcf@fix
  positional_info.df <- as.data.frame(positional_info.df)
  head(positional_info.df)
  
  # Use positional info (CHROM and POS) from the VCF fix fields as a locus.id
  locus.id <- paste0(positional_info.df$CHROM, "__", positional_info.df$POS)
  rownames(geno) <- locus.id # use the new ID as the row names
  rm(locus.id)
  #geno[1:5, 1:5]
  
  # Convert genotypes to numeric allele dosage
  geno[geno=="0/0"] = 0
  geno[geno=="0/1"] = 1
  geno[geno=="1/1"] = 2
  #geno[1:5, 1:5]
  
  # Transpose
  geno = t(geno) # new format: rows = samples, cols = loci
  #geno[1:5, 1:5]
  
  # Convert to numeric
  mode(geno) = "numeric"
  #geno[1:5,1:5]
  
  ## Prepare phenotypes
  print(paste0("Loading phenotype file from: ", pheno_path))
  print("Note: only indiv. included in the phenotype file will be included in output")
  
  # Read in phenotype file
  pheno.df <- read.delim(file = pheno_path, header = F, sep = "\t")
  colnames(pheno.df) <- c("sample.id", "pheno")
  pheno.df <- as.data.frame(pheno.df)
  
  # Remove any individuals from the genotype matrix that are not included in the phenotype file
  print(paste0("Loaded number of individuals in geno: ", nrow(geno)))
  print("Only keeping individuals with provided phenotypes")
  geno <- geno[rownames(geno) %in% pheno.df$sample.id, ]
  print(paste0("Current number of individuals in geno that had pheno: ", nrow(geno)))
  
  # Obtain sample order
  samples_and_pheno.df <- rownames(geno)
  samples_and_pheno.df <- as.data.frame(samples_and_pheno.df)
  colnames(samples_and_pheno.df)[1] <- "sample.id"
  samples_and_pheno.df$data.order <- seq(1:nrow(samples_and_pheno.df)) # add numeric col for order
  
  # Combine with phenos and put back in order as needed
  samples_and_pheno.df <- merge(x = samples_and_pheno.df, y = pheno.df, by = "sample.id", all.x = T, sort = F)
  samples_and_pheno.df <- samples_and_pheno.df[order(samples_and_pheno.df$data.order), ]
  
  # Convert pheno as needed
  if(pheno_type=="binary"){
    
    print("Converting binary phenotype to numeric values")
    samples_and_pheno.df$original.pheno <- samples_and_pheno.df$pheno
    samples_and_pheno.df$pheno <- as.numeric(as.factor(samples_and_pheno.df$pheno))
    
  }
  
  print(samples_and_pheno.df) 
  
  # Save as vector
  pheno <- samples_and_pheno.df$pheno
  
  ## Prepare genotypes
  gwasgeno = t(geno) # new format: rows = variants; cols = individuals
  #gwasgeno[1:5,1:5]
  
  # Add locus name as vector in df
  gwasgeno <- cbind(rownames(gwasgeno), "X","Y", gwasgeno)
  #gwasgeno[1:5,1:5]
  
  # Write outputs
  write.table(x = pheno, file = paste0(result.path, "gwas_pheno.txt"), row.names = F, col.names = F) # pheno
  write.table(x = gwasgeno, file = paste0(result.path, "gwas_geno.txt"), row.names = F, col.names = F, quote = F) # geno
  
  print("The output will be in the results folder, 'gwas_geno.txt' and 'gwas_pheno.txt'")
  print("See README for steps to take in GEMMA")
  
}
