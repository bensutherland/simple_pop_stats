# simple_pop_stats
A short analysis of population statistics given specific inputs

Requirements:     
<to do>

## 00. Setup ##
The StockCodes file <sp>StockCodesCU.txt must be in:     
`00_archive/` (Linux)       
`H:/Stock_Codes/<species>/` (Windows/MGL)     

I suggest putting a genepop into `02_input_data`, but there is an interactive selection of files, so it can really be anywhere on your machine.     

Source the main script `01_scripts/simple_pop_stats_start.R` to activate all functions.    
Select a species from the options to set up most variable names.        


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
If you are working with *MGL_GSI_SNP* data, where the sample IDs are all in the composition of:    
`stockcode_year_indivID_sex`, e.g. `2_2000_9_U`      
...you can use the following script to rename your populations:    
`update_pop_names(sep_by = "collection", name_by = "stockname")`    

Other options:     
`sep_by`    
* collection     
* collection_and_year    
* none (do nothing)      

`name_by`     
* stockname     
* none (do nothing to keep as stock code)     

Made a mistake? Don't worry, just recover your original genind:      
`obj <- obj.bck`      

If you are working with *MGL_GSI* microsat data, and want to clean your population names, use the following to drop all characters after the first space:      
`fix_pop_names(df = obj)`      

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

## 05.5 Rename microsat pops to SNP pop names
In case you want to use downstream applications for microsat data, you need to replace microsat pop names with SNP pop names. So create a crosswalk file, with the following format:     
datatype1, datatype2
popname_microsat, popname_SNP

...save it, and then load it using:    
`connect_two_datatypes(df = obj_pop_filt, crosswalk.FN = "path/to/crosswalk/file")`    


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

Note: if you want to have a custom filename for your FST csv file, use the argument `cust_fn` for your basename, which will automatically save into `03_results`.      

## 07. Build a tree ##
You can build a tree using the previous genetic differentiation object:      
`make_tree(matrix = pairwise_wc_fst, tree_method = "NJ", separated = TRUE)`         

...or you can build a new tree using bootstrap with the filtered genind file:        
`make_tree(bootstrap = TRUE, boot_obj = obj_filt, nboots = 10000, dist_metric = "edwards.dist", separated = TRUE)`      

## 08. Run multidimensional scaling techniques
Conduct PCA on your sample using the following:     
`pca_from_genind(data = obj_pop_filt, PCs_ret = 3, plot_eigen=TRUE, plot_allele_loadings=TRUE)`       
This will output results into `03_results`    
In the future, there will be an option to upload a colour file for your populations.     

After the PCA, you can run a DAPC, which will use the cols matching those from your PCA. 
`dapc_from_genind(data = obj_pop_filt, plot_allele_loadings = TRUE)`      

## 09. Calculate relatedness
First, convert data (SNP or microsat) from genind to relatedness format and calculate relatedness values:      
`relatedness_calc(data = obj_pop_filt, datatype = "SNP")`        
...this will output to `03_results/kinship_analysis_<date>.Rdata`      

Note: if you are using microsat data, this will depend that you have run 'Rename microsat pops to SNP pop names' above, so that the pop names can be converted to stock codes.        

Second, plot your results:      
`relatedness_plot(file = "03_results/kinship_analysis_<date>.Rdata", same_pops = TRUE, plot_by = "names")`     
...where you can use either "names" or "codes" if using only same-on-same.      
...and if you set `same_pops` to FALSE, you will get all pops pairwise comparisons. (but can't use names)      

## 10. Compare geographic and genetic distance
If you have GPS coordinates in the stock code file, you can automatically calculate the distance between all pairs of locations using the following script:     
`calculate_physical_dist()`      
...which will output `03_results/physical_distance.txt`     

Using this file, along with an earlier calculated FST (output of `calculate_FST()` above), use the following:       
`compare_phys_genet_dist()`       
...which will put your results into `03_results/pairwise_fst_v_physical_dist.pdf`      

## 11. Run AMOVA
AMOVA will use repunits and collections to see where the variance exists in your data.    
To create a repunit file _de novo_, run:      
`calculate_AMOVA(data = obj_pop_filt, build_file = TRUE)`       
This will output `00_archive/unique_pops.csv`, and fill this out with a new column entitled `repunit` to show the higher level groupings in your data.     

Once you have a file describing the repunits, run the following:      
`calculate_AMOVA(data = obj_pop_filt, build_file = FALSE, missing_treat = mean)`      
The results will be output into `obj_amova` and `obj_amova.pegas`.     
Other options:      
* mean = impute missing
* ignore = do nothing
* zero = convert NA to 0
* genotype = drop indiv w/ missing

## 12. Convert pop to repunit
Once you have built the file for the AMOVA with repunits, you can use this file to re-calculate FST using repunits instead of collections.      

