# simple_pop_stats
A short analysis of population statistics given specific inputs

Requirements:     
adegenet     
tcltk (linux only)     
hierfstat    


## Setup ##
I suggest putting a genepop into `02_input_data`, but there is an interactive selection of files, so it can really be anywhere on your machine.     

Source the main script `01_gen_diff.R`. This will source all of the functions within this repo.    
When this script is sourced, you will be prompted to select a species from the options. This will set up a lot of your file names.         


## Loading Data ##
Load a genepop with the following, using 'SNP' or 'microsat':    
`load_genepop(datatype = "SNP")`     
Your data will be put into the 'obj', which is a genind object.    

To see what populations you have:     
`unique(pop(obj))`      


## Renaming Data ##
If you are working with MGL_GSI_SNP data, where the sample IDs are all in the composition of:    
`stockcode_year_indivID_sex`, e.g. `2_2000_9_U`      
...you can use the following script to rename your populations:    
`update_pop_names(sep_by = "collection", name_by = "stockname")`    

Other options:     
`sep_by`    
* collection     
* collection_and_year    
* none      
`name_by`     
* stockname     
* none     


## Characterize Data ##
To find the number of samples, markers, alleles, and sample size per population, use the following:    
`characterize_genepop(df = obj)`


## Drop loci ##
To remove loci, use the following script that can allow you to remove monomorphic loci, or remove loci using a tab-delimited file with a single column with marker names that are to be removed from the object. This will end up as `obj_filt`.    
`drop_loci(drop_monomorphic = TRUE, drop_file = <path/to/drop/file.txt>)


## Genetic Differentiation ##
For this step, you will need your data prepared for analysis in hierfstat, so use the following:    
If you only have a genind saved as obj_filt in this example, and it has been separated by something above:    
`calculate_FST(format="genind", dat = obj_filt, separated = TRUE)`     
...note: if it has not been separated, run with separated = FALSE.    

If you already have a hierfstat object:     
`calculate_FST(format="hierfstat", dat = obj_filt, separated = FALSE)       
...note: same as above, you can use separated=TRUE.     




