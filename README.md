# simple_pop_stats
A short analysis of population statistics given specific inputs

**Note: this repo is only meant to be used for the authors' purposes and is not meant for broader use. The main purpose is to increase reproducibility for the authors' manuscripts that use this repo. There are no guarantees of usefulness beyond this use.**

Also see Appendices for additional specific use-case instructions:     
[Appendix A - Baseline Summary Benchmark](README_benchmark_appendix.md)

Requirements:     
<to do>
Installation of related requires some special instructions on Windows. Follow tutorial:     
https://github.com/timothyfrasier/related

## 00. Setup ##
#### A. Prepare required files:    ####
Required files are automatically identified on network, or if off network:       
```
# Put essential files in 00_archive, including     
# Extraction sheets: 
<species>_PBT_ES.txt or <species>_mix_PBT_ES.txt (base/mix ES)     
# Hotspot file:    
<species>_hotspot_detail.txt (hotspot file)     
# Stock code file:  
SNP only: <sp>StockCodesCU.txt or <sp>mixCodes.txt (base/mix stock codes)        
microsat only: <sp>StockCodes_microsat.txt

# The input file can be anywhere on your computer, but you can also put it in 02_input_data
```

Note: new experimental (caution! will overwrite existing files!) function to update files from a source location to a target location (specifically your 00_archive):        
`update_essential_files()`       
...where your new essential files (those above) are in "C:/00_GitHub/essential_files/00_GSI_essentials/<species>"     


#### B. Source functions and set variables ####
Source `01_scripts/simple_pop_stats_start.R` to activate all functions.    
Select if you are on the DFO network or running local.          
Select the species being analyzed from the available list.        
      
#### C. Set custom output directory ####
By default, the results will go into directories within this repo, unless a custom output director is chosen:           
```
set_output_folder()   # works interactively
set_output_folder(result_path= "<your/full/path>")
# note: the directory must already exist
```

#### D. Notes on input files (microsat-specific) ####
*NOTES*: Specific observations in microsats, in regards to the input genepop file
* Please ensure that the "POP" line between collection does not contain anything other than "POP" (doesn't seem to play nice with the import)
* Please ensure that collection names do not contain trailing underscores (eg. Not `Ash_`) as the script will remove it and no longer match stock codes.
* Please ensure that the second column contains four digit year (or 4 digit number of some sort). (eg. Not `Nass_test    203   1`, rather `Nass_test     9999  1`)
* Please ensure that the collection name does not contain four digits (eg. Not `Nitinat_1997    1997  1`, rather `Nitinat97     1997  1`)
* Please ensure that the original names.dat file specifies a named region for ALL collections, not a blank (eg. Not `Bulkley NA`)

#### E. Parameter definitions or notes of clarification ####
* "Separated" - the option of `separated = TRUE` and `FALSE` is presented in downstream `calculate_FST` and `make_tree` functions. Please note - this is solely about naming the output file, and has no additional effect on the function  - if `separated = TRUE` in the `make_tree` or `calculate_FST` function, it will attempt to add the value of `sep_by` to the file-name. The value `sep_by` here is defined by the selection in the `update_pop_names` script.  As the microsatellite function does not currently output a value for `sep_by` when `fix_pop_names` is run, *separated == FALSE* for all functions downstream of Section 2. 


## 01. Loading Data ##
Load a genepop with the following, using 'SNP' or 'microsat':    
`load_genepop(datatype = "SNP")`     
Your data will be put into the 'obj', which is a genind object.    

To see what populations you have:     
`unique(pop(obj))`      




## 02. Rename Data ##
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

#### Rename microsat data ####
If working with _MGL_GSI_ microsat data, first make a stock code file similar to the SNP stock code file, by inputting a names file that has been formatted as tab-delimited:      
`prepare_stockcode_file(fix_names = TRUE, names_file.FN = "/path/to/your/tab/delim/names/file.txt")`       

Then clean up your population names using the following:        
`fix_pop_names(data = obj, append_region = TRUE, stockcode.FN = "/path/to/your/new/stockcode/file.txt")`      

note: caution, if everything before the space is not unique between populations could cause populations to be merged. There are some checks in place to prevent this, but good to be aware of it.     

## 03. Characterize Data ##
To find the number of samples, markers, alleles, and sample size per population, use the following:    
`characterize_genepop(df = obj, pdf_width = 30, pdf_height = 6, cex_names = 0.3, N=30)`     
Will produce a barchart of sample size per pop (`sample_size*.pdf`).     
Set the parameters based on your plotting requirements, the above is an example for a very large number of pops. Use `N` to set where the line will be drawn across the plot.     

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

This will assign your results as `pairwise_wc_fst` object within the R environment, and save the results to the `03_results` folder as a file called `gen_diff_wcfst_<*>.csv` by default.      

If you want to have a custom filename for your FST csv file, use the argument `cust_fn` for your basename, which will automatically save into `03_results`.      

Note - separated = FALSE for microsatellites (see [00.E](https://github.com/bensutherland/simple_pop_stats#e-parameter-definitions-or-notes-of-clarification))

## 07. Build a tree ##
To build a bootstrapped tree using the `aboot` function in poppr:

`make_tree(bootstrap = TRUE, boot_obj = obj_pop_filt, nboots = 10000, dist_metric = "edwards.dist", separated = TRUE)`      

NOTE: Above bootstrapping did not work after upgrading Ape from 5.3 to 5.4. It should be fixed as of Ape 5.4-1 according to: http://ape-package.ird.fr/NEWS 


You can also build a non-bootstrapped tree using the previous genetic differentiation object and the `NJ` function in phangorn. (`bootstrap` must equal `FALSE` and `tree_method  = "NJ"` in this scenario):      
`make_tree(matrix = pairwise_wc_fst, tree_method = "NJ", separated = TRUE, bootstrap = FALSE)`

NOTE: If you need to load your `pairwise_wc_fst` from the output file produced by `calculate_FST`, please use as an example:
`pairwise_wc_fst <- as.matrix(read.table(file="03_results/gen_diff_wcfst_<*>.csv",row.names = 1, header=TRUE,sep=","))`           

     

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

## 13. Convert format (genepop to rubias) 
Convert genepop to rubias: 
`genepop_to_rubias(data = obj, sample_type = "reference")`      
Note: uses the datatype variable (SNP/microsat)            

*For microsatellite data*
To convert the microsat data to rubias, the repunits per stock code must be specified:      
```
# Prepare the following file: 00_archive/<sp>StockCodes_microsat.txt     
# With the following format: 
collection  repunit
Bella_Coola CC
Wannock     CC
Fraser      FR
```

## 14. Run simulated individual assignment test
Use a rubias baseline output by MGL_GSI_SNP or by the genepop_to_rubias() converter as a 100% simulation input to test your reporting units using the rubias function assess_reference_loo().       
`full_sim(rubias_base.FN = "03_results/rubias_output.txt", num_sim_indiv = 200, sim_reps = 100)`          
...will save output into 03_results, including:
- collection_100_stats_YYYY-MM-DD.txt (summary info of sum of all iteration assignments)
- collection_100_stats_all_reps_YYYY-MM-DD.txt (not just the top repunit assignment)
- collection_100_stats_all_pops_YYYY-MM-DD.txt (not just the top collection assignment)
- all_collection_results_YYYY-MM-DD.txt.gz (raw output of all sims)

For now, save these to a separate folder to make sure they don't get written over.    

**Alternate option**: can also run 100% simulations using Oncor for the microsatellite data.       
Use MGL_GSI README to export an oncor baseline and grouping file. Groupings can be easily edited.       

Format of the grouping file:      
```
TITLE LINE
Basin_Cr_RT\t Alsek
Bear_Slough_RT\t Taku
```

Open oncor.    
File>Open Baseline data (and load your baseline)      
File>Open Reporting Groups (and load your reporting groups)     
Mixture Analysis>100% simulations    
Set parameters (e.g., as defaults), and run the analysis.    

Note: *if you get an error*, make sure that there are no cases where a single pop has no data whatsoever for at least one marker (e.g., make sure to remove the SNPs for sockeye, if all pops do not have the SNP data).     


## 15. Plot mean assignment per repunit from 100 sim
```
plot_summarize_100_sim(axis_label="repunit",repunits_file = TRUE,
                          plot_prefix = "summarize_100_sim")
```
If you have a repunits file, use `repunits_file = TRUE`, otherwise set this to false (eg. usats). Will be chosen by interactive popup if TRUE. You can also add in "regional roll-up" by adding a column to the repunits file with the heading `region` - matches the format that Chum and Eulachon already use. 

Repunits allows you to choose the axis_label - if you didn't use a repunits file, don't change the default. Could be repunit, CU or CU_NAME, but not extensively tested. 

plot_prefix allows you to change the output file name, so you don't overwrite previous work. Will write to the 03_results folder. 

Requires 2 files produced in `full_sim`

- collection_100_stats_all_reps_YYYY-MM-DD.txt (not just the top repunit assignment)
- collection_100_stats_all_pops_YYYY-MM-DD.txt (not just the top collection assignment)

These are selected interactively. Could use the - collection_100_stats_YYYY-MM-DD.txt file probably, but would break the regional roll-up.

## 16. Summarize a rubias base for collections, years and total N
Will summarize a filtered rubias base, and export a table that reports the Repunit, CU number, Collection, years per collection and Total N. This is a commonly produced table for publication, so provided here for reproducability.

```
summarise_rubias_baseline(baseline = rubias_base,
                          out_prefix = "rubias_base_summary",
                          repunit_desc = repunit_desc,
                          by_year=FALSE)
```

Takes a filtered baseline, an output prefix and a repunit file. Will export to 03_results. Please do not provide a path in the output prefix, only a prefix for the output file. Tacked on to the prefix will be `.baseline_summary.txt` and output will be tab delimited. Example:

|                                    |           |                         |                        |     | 
|------------------------------------|-----------|-------------------------|------------------------|-----| 
| Region/Conservation Unit           | CU Number | Population              | Years                  | N   | 
| Kalum_early timing                 | 49        | CEDAR_RIVER             | 1996                   | 20  | 
| Kalum_late timing                  | 50        | KITSUMKALUM_RIVER-LOWER | 2013, 2014, 2015, 2016 | 559 | 
| Zymoetz                            | 80        | THOMAS_CREEK            | 2004, 2009, 2010       | 96  | 
| Sicintine                          | 81        | SICINTINE_RIVER         | 2010                   | 115 | 
| Middle Skeena-mainstem tributaries | 54        | BULKLEY_RIVER-LOWER     | 1999                   | 96  | 
|                                    |           | KISPIOX_RIVER           | 2004, 2006, 2008, 2010 | 98  | 
|                                    |           | KITSEGUECLA_RIVER       | 2009                   | 95  | 
|                                    |           | KITWANGA_RIVER          | 2003                   | 93  | 
|                                    |           | KULDO_CREEK             | 2008, 2009             | 95  | 

With `by_year=TRUE` it will add counts per year. Example:

|                                    |           |                     |                                      |    | 
|------------------------------------|-----------|---------------------|--------------------------------------|----| 
| Region/Conservation Unit           | CU Number | Population          | Years(N)                             | N  | 
| Middle Skeena-mainstem tributaries | 54        | BULKLEY_RIVER-LOWER | 1999(96)                             | 96 | 
|                                    |           | KISPIOX_RIVER       | 2004(59), 2006(28), 2008(3), 2010(8) | 98 | 
|                                    |           | KITSEGUECLA_RIVER   | 2009(95)                             | 95 | 
|                                    |           | KITWANGA_RIVER      | 2003(93)                             | 93 | 


The rubias file requires the headers `collection`, `repunit` and `indiv` <- should be there by default.
The repunits file requires the headers `Display_Order`, `CU`, `CU_Name`,`repunit`. 


## 17. Summarize the Juvenile column in the Extraction sheets

To create a summary of collections that are known to contain Juveniles, run:

```
juvenile_count(by_year=TRUE)
```

`by_year = TRUE` will identify specific years in the baseline, while `by_year = FALSE` will simply report the collection name.

