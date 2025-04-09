# Convert vcf data (SNP) to GEMMA inputs
#  dev based on code used here: https://raw.githubusercontent.com/bensutherland/ms_cgig_chr8_oshv1/refs/heads/main/01_scripts/chr8_oshv1_amp_02_vcf_to_gemma.R
#  Ben J.G. Sutherland (SBIO), 2025-04-09
vcf_to_gemma <- function(vcf_path = ""
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
  
  # Use positional info as the locus.id
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
  
  ## TODO: add option for subsetting a specific set of individuals
  
  ## TODO: add option for creating phenotype file
  
  ## Prepare genotypes
  gwasgeno = t(geno) # new format: rows = variants; cols = individuals
  #gwasgeno[1:5,1:5]
  
  # Add locus name as vector in df
  gwasgeno <- cbind(rownames(gwasgeno), "X","Y", gwasgeno)
  #gwasgeno[1:5,1:5]
  
  # Write outputs
  #write.table(x = pheno, file = paste0(result.path, "gwas_pheno.txt"), row.names = F, col.names = F) # pheno (TODO)
  write.table(x = gwasgeno, file = paste0(result.path, "gwas_geno.txt"), row.names = F, col.names = F, quote = F) # geno
  
  print("The output will be in the results folder, 'gwas_geno.txt'")
  print("See README for steps to take in GEMMA")
  
}
