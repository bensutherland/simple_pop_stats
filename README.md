# simple_pop_stats
A short analysis of population statistics given specific inputs

Requirements:     
adegenet     
tcltk (linux only)     
hierfstat    
phangorn       
ape      

## 00. Setup ##
I suggest putting a genepop into `02_input_data`, but there is an interactive selection of files, so it can really be anywhere on your machine.     

Source the main script `01_gen_diff.R`. This will source all of the functions within this repo.    
When this script is sourced, you will be prompted to select a species from the options. This will set up a lot of your file names.         


## 01. Loading Data ##
Load a genepop with the following, using 'SNP' or 'microsat':    
`load_genepop(datatype = "SNP")`     
Your data will be put into the 'obj', which is a genind object.    

If you are loading from an R object, e.g. a renamed genepop from `MGL_GSI_SNP` with the object name my.genepop:     
`load(<R object>)`      
`obj <- my.genepop`      

To see what populations you have:     
`unique(pop(obj))`      


## 02. Renaming Data ##
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


## 03. Characterize Data ##
To find the number of samples, markers, alleles, and sample size per population, use the following:    
`characterize_genepop(df = obj)`
Will produce a barchart of sample size per pop (`sample_size*.pdf`).     

## 04. Drop loci ##
To remove loci, use the following script that can allow you to remove monomorphic loci, or remove loci using a tab-delimited file with a single column with marker names that are to be removed from the object. This will end up as `obj_filt`.    
`drop_loci(drop_monomorphic = TRUE, drop_file = <path/to/drop/file.txt>)`       

## 05. Drop pops ##
To remove pops from a user defined minimum number of individuals, or a tab-delimited text file (change from NULL), use the following to create `obj_pop_filt`:    
`drop_pops(df = obj_filt, drop_by_pop_size = TRUE, min_indiv = 35, drop_file = NULL)`       


## 06. Genetic Differentiation ##
For this step, you will need your data prepared for analysis in hierfstat, so use the following:    
If you only have a genind saved as obj_filt in this example, and it has been separated by something above:    
`calculate_FST(format="genind", dat = obj_filt, separated = TRUE)`     
...note: if it has not been separated, run with separated = FALSE.    

If you already have a hierfstat object:     
`calculate_FST(format="hierfstat", dat = obj_filt, separated = FALSE)`       
...note: same as above, you can use separated=TRUE.     
...note: if you used the above to format from genind to hf your hf will be the obj_filt.hf     

This will output your results as `pairwise_wc_fst`, and save to the `03_results` folder.      


## 07. Build a tree ##
You can build a tree using the previous genetic differentiation object:      
`make_tree(matrix = pairwise_wc_fst, tree_method = "NJ", separated = TRUE)`         

...or you can build a new tree using bootstrap with the filtered genind file:        
`make_tree(bootstrap = TRUE, boot_obj = obj_filt, nboots = 10000, dist_metric = "edwards.dist", separated = TRUE)`      

