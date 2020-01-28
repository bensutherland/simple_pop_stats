# simple_pop_stats
A short analysis of population statistics given specific inputs

Requirements:     
<to do>
Installation of related requires some special instructions on Windows. Follow tutorial:     
https://github.com/timothyfrasier/related

## 00. Setup ##
#### Files:    ####
Necessary auxillary files must be either accessed through the network, or placed into `00_archive`, including:     
- <sp>StockCodesCU.txt or <sp>mixCodes.txt (base/mix stock codes)        
- <species>_PBT_ES.txt or <species>_mix_PBT_ES.txt (base/mix ES)     
- <species>_hotspot_detail.txt (hotspot file)     

Input file (genepop or rubias) can be accessible anywhere, but it is suggested to put into `02_input_data` for ease of access.     

#### Start ####
Source the main script `01_scripts/simple_pop_stats_start.R` to activate all functions.    
Select whether you are on the network or not (i.e., running off network on local machine)     
Select a species from the options to set up most variable names, this will set path variables.        


## 01. Loading Data ##
Load a genepop with the following, using 'SNP' or 'microsat':    
`load_genepop(datatype = "SNP")`     
Your data will be put into the 'obj', which is a genind object.    

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

New: the option `add_CU` can be used to append the CU from the stock code file to the population if you use the following command (only compatible with the following):      
`update_pop_names(sep_by = "collection", name_by = "stockname", add_CU = TRUE)`

Made a mistake? Don't worry, just recover your original genind:      
`obj <- obj.bck`      

If you are working with *MGL_GSI* microsat data, and want to clean your population names, use the following to drop all characters after the first space:      
`fix_pop_names(df = obj)`      

## 03. Characterize Data ##
To find the number of samples, markers, alleles, and sample size per population, use the following:    
`characterize_genepop(df = obj, pdf_width = 30, pdf_height = 6, cex_names = 0.3)`     
Will produce a barchart of sample size per pop (`sample_size*.pdf`).     
Set the parameters based on your plotting requirements, the above is an example for a very large number of pops.     

## 04. Drop loci ##
To remove loci, use the following script that can allow you to remove monomorphic loci, or remove loci using a tab-delimited file with a single column with marker names that are to be removed from the object. This will end up as `obj_filt`.    
`drop_loci(drop_monomorphic = TRUE, drop_file = <path/to/drop/file.txt>)`       

## 05. Drop pops ##
To remove pops from a user defined minimum number of individuals, or a tab-delimited text file (change from NULL), use the following to create `obj_pop_filt`:    
`drop_pops(df = obj_filt, drop_by_pop_size = TRUE, min_indiv = 35, drop_file = NULL)`       

## 05.1 Reduce populations by sample size
`downsample_pops(data = obj_filt, subset_method = "chosen", set_sample_size = 40)`      
Other opts: "chosen", "average", "min"

## 05.2 Rename microsat pops to SNP pop names
In case you want to use downstream applications for microsat data, you need to replace microsat pop names with SNP pop names. So create a crosswalk file, with the following format:     
(Note: first line is header names, keep as shown, following lines are custom):     
datatype1, datatype2        
popname_microsat, popname_SNP

...save it, and then load it using:    
`connect_two_datatypes(df = obj_pop_filt, crosswalk.FN = "path/to/crosswalk/file")`    


## 06. Genetic Differentiation ##
For this step, you will need your data prepared for analysis in hierfstat, so use the following:    
If you only have a genind saved as obj_pop_filt in this example, and it has been separated by something above:    
`calculate_FST(format="genind", dat = obj_pop_filt, separated = TRUE)`     
...note: if it has not been separated, run with separated = FALSE.    

If you already have a hierfstat object:     
`calculate_FST(format="hierfstat", dat = obj_pop_filt, separated = FALSE)`       
...note: same as above, you can use separated=TRUE.     
...note: if you used the above to format from genind to hf your hf will be the obj_pop_filt.hf     

This will output your results as `pairwise_wc_fst`, and save to the `03_results` folder.      

Note: if you want to have a custom filename for your FST csv file, use the argument `cust_fn` for your basename, which will automatically save into `03_results`.      

## 07. Build a tree ##
You can build a tree using the previous genetic differentiation object:      
`make_tree(matrix = pairwise_wc_fst, tree_method = "NJ", separated = TRUE)`         

...or you can build a new tree using bootstrap with the filtered genind file:        
`make_tree(bootstrap = TRUE, boot_obj = obj_pop_filt, nboots = 10000, dist_metric = "edwards.dist", separated = TRUE)`      

## 08. Run multidimensional scaling techniques
Conduct PCA using:     
`pca_from_genind(data = obj_pop_filt, PCs_ret = 3, plot_eigen=TRUE, plot_allele_loadings=TRUE, colour_file = NULL)`       
This will output results into `03_results`    

Conduct DAPC using:      
`dapc_from_genind(data = obj_pop_filt, plot_allele_loadings = TRUE, colour_file = NULL)`      

To use custom colours, set the path to a csv file with header 'collection' and 'colour' containing the population name and colour name to set your custom colours.     

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
`compare_phys_genet_dist(FST_file = "03_results/<your_FST_file>.csv")`       
...which will put your results into `03_results/pairwise_fst_v_physical_dist.pdf`      

## 11. Run AMOVA
AMOVA will use repunits and collections to see where the variance exists in your data.    
To create a repunit file _de novo_, run:      
`calculate_AMOVA(data = obj_pop_filt, build_file = TRUE)`       
This will output `00_archive/unique_pops.csv`, and fill this out with a new column entitled `repunit` to show the higher level groupings in your data.     

Once you have a file describing the repunits, run the following:      
`calculate_AMOVA(data = obj_pop_filt, build_file = FALSE, missing_treat = "mean")`      
The results will be output into `obj_amova` and `obj_amova.pegas`.     
Other options:      
* mean = impute missing
* ignore = do nothing
* zero = convert NA to 0
* genotype = drop indiv w/ missing

## 12. Generate allele frequency table
Calculate allele frequencies per population using the following:    
`calculate_allele_freq()`    
This will output 'freq.df', but to add the actual genotype alleles to the file, use the following:    
`build_allele_freq_table(freq_file = freq.df)`    
Note: this assumes that the hotspot file that is currently active is the same as the one that has been used to score all of the samples in your dataset.      

## 13. Convert genepop to rubias format
Want to use a rubias format file but don't want to go back to MGL_GSI_SNP? Or maybe you want to be able to use functions such as downsampling within simple_pop_stats and pipe to rubias? Use the following:    
`genepop_to_rubias(data = obj, sample_type = "reference")`      
This will use the datatype variable to determine SNP / microsat and run a conversion to produce a rubias output.      

If you are generating a microsat genepop to rubias, then you need a conversion table specified. This will be in the format:     
```
collection\t repunit
Bella_Coola\t CC
...
```

## Extra. Convert pop to repunit
*update*: this is no longer suggested, as it skews allele frequencies towards the population with the largest sample size that is being grouped into the repunit.     
Once you have built the file for the AMOVA with repunits, you can use this file to re-calculate FST using repunits instead of collections.      
`pop_to_repunit(data = obj_pop_filt)`     

Now you can go back to the FST calculation above and calculate with your repunit merged:    
`calculate_FST(format="genind", dat = obj_repunit, separated = TRUE, cust_fn = "gen_diff_wcfst_repunit_by_stockname.csv")`       
...make sure to use a custom filename, otherwise this will re-write over existing FST calculations.   
