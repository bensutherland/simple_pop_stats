# simple_pop_stats
A short analysis of population statistics given specific inputs

**Note: this repo is only meant to be used for the authors' purposes and is not meant for broader use. The main purpose is to increase reproducibility for the authors' manuscripts that use this repo. There are no guarantees of usefulness beyond this use.**

Also see Appendices for additional specific use-case instructions:     
[Appendix A - Baseline Summary Benchmark](README_benchmark_appendix.md)

Requirements:     
<to do>
Installation of related requires special instructions; for Windows or linux follow the tutorial, and for mac there are instructions in the readme for install from .tar.gz:             
https://github.com/timothyfrasier/related

## 00. Setup ##
#### A. Prepare required files:    ####
Required files are automatically identified on network, or if off network:       
```
# Put essential files in 00_archive, including     
# Extraction sheets: Standard abbreviations for species naming: SK, CN, CO, CM, PK, and STH
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

NOTE: On first install, packages will need to be installed. They are currently commented out at the top of `01_scripts/simple_pop_stats_start.R`. Uncomment to install manually, or if using Rstudio use the yellow box at the top to install. Biocmanager and SNPrelate may require additional, manual effort eg.

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("SNPRelate")      
```

Most scripts have been evaluated on R3.6.3 and RStudio 1.2.5019 on Windows 10. Additional testing has been performed on Linux, and occasionally R4.0.2, but would recommend continuing with R3.X for now if possible. 

#### C. Set custom output directory ####
By default, the results will go into directories within this repo, unless a custom output director is chosen:           
```
set_output_folder()   # works interactively
set_output_folder(result_path= "<your/full/path>")
# note: the directory must already exist
```

#### D. Notes on input files (microsat-specific) ####
*NOTES*: Specific observations in microsats, in regards to the input genepop file, please ensure that...
* there is a "POP" line is present between collections, and only contains the string "POP" without anything else, otherwise will not import
* collection names cannot have trailing underscores (e.g., not `Ash_`) as the script will remove it and no longer match stock codes
* the second column contains a four- or three-digit numeric year separator (e.g., not `Nass_test    99   1`, but rather `Nass_test     9999  1`) (note: three digits was enabled to allow for the cod 999, which is used often as a replacement for an unknown year)
* the collection name cannot contain four digits surrounded by underscores or spaces (e.g., not `Nitinat_1997    1997  1`, rather `Nitinat97     1997  1`)
* the original names.dat file specifies a named region for ALL collections, not a blank (e.g., not `Bulkley NA`)

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

### Use custom stock codes ###  
If you are re-defining stock codes for a specific analysis in SNPs, you can't use the default stock code file. If you have an alternate file, you need to redefine `sc.base`. If you do, use:

`sc.base <- "00_archive/custom_stockcodes.txt` 

As an example.    


## 03. Characterize Data ##
To find the number of samples, markers, alleles, and sample size per population, use the following:    
`characterize_genepop(df = obj, pdf_width = 30, pdf_height = 6, cex_names = 0.3, N=30)`     
Will produce a barchart of sample size per pop (`sample_size*.pdf`).     
Set the parameters based on your plotting requirements, the above is an example for a very large number of pops. Use `N` to set where the line will be drawn across the plot.     

        
To characterize your loci, run the following to get a summary per locus including Fst, Fit, Fis, Hobs, Hexp:      
`per_locus_stats(data = obj)`      
...this will produce in `03_results/per_locus_stats_<YYYY-MM-DD>.txt`      

## 04. Drop loci ##
To remove loci, use the following script that can allow you to remove monomorphic loci, or remove loci using a tab-delimited file with a single column with marker names that are to be removed from the object. This will end up as `obj_filt`.    
`drop_loci(drop_monomorphic = TRUE, drop_file = <path/to/drop/file.txt>)`       

## 05. Drop pops ##
To remove populations based on a minimum sample size, or based on a tab-delimited text file, use the following:    
`drop_pops(df = obj_filt, drop_by_pop_size = TRUE, min_indiv = 35, drop_file = NULL,  drop_unused = FALSE, names.dat.FN = NULL, drop_indivs = NULL)`       

Change `drop_file` from NULL to a filename to drop named populations. The drop file should be tab-delimited, only one column, with no header.          
note: if you have attached regions previously, this will not work as the pattern matching is to the name in the text file.       

Change `drop_indivs` from NULL to drop specific individuals.           
note: the drop_indivs will occur prior to pop size filter; the pop size filter will operate on the remaining samples.        

New: `drop_unused = TRUE` will allow for the dropping of region codes 98 + 99 from microsatellites. If true, the `names.dat.FN` should point to the tab delimited names.dat file created for `prepare_stockcode_file` in step 2.         

This step will generate `obj_pop_filt`.        

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

## 05.3 Keep pops
To keep certain populations based on a list. Works the exact opposite to the `drop_file` from Drop pops. 

`keep_pops(df = obj_filt, keep_file = NULL)`      

Change `keep_file` from NULL to a path to a file containing names of collections to keep. The keep file should be tab-delimited, with collections in the first column, with no header. Additional columns in file beyond the first are ignored.
          
note: if you have attached regions or CUs in Step 2 previously, this will not work as the pattern matching is to the name in the text file. If you have done so, please attach the region/CU to the collection name.

eg. `Tatchun_R_MYR` instead of `Tatchun_R`

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
```
pca_from_genind(data = obj_pop_filt
                       , PCs_ret = 3
                       , plot_eigen=TRUE
                       , plot_allele_loadings=TRUE
                       , colour_file = NULL
                       , retain_pca_obj = TRUE 
                       , parallel = FALSE # set to TRUE to use parallel for pca
                       )       
```

This will output results into `03_results`, including `pca_scores_per_sample.txt` to know each sample's positions on the retained PCA axes (also see pca.obj in global environment).     

Note that you can determine variance explained per axis by accessing pca.obj$eig, and can normalize to a percentage by simply taking the eigenvalue as a percentage of the sum of all eigenvalues. [more info from adegenet](https://adegenet.r-forge.r-project.org/files/PRstats/practical-MVAintro.1.0.pdf)       

Conduct DAPC using:      
```

dapc_from_genind(data = obj_pop_filt
                , plot_allele_loadings = TRUE  # plot and export the discrim. fn. locus variance contributions? 
                , colour_file = NULL           # use custom colours 
                , n.pca = 10, n.da = 1         # number PC axes to use and discriminant functions to retain
                , scree.da = TRUE              # plot scree plot for DF
                , scree.pca = TRUE, posi.pca = "topright"     # plot PCA scree plot
                , dapc.width = 7, dapc.height = 5             # PDF filesize for scatterplot
                ) 
```

Set the number PCA and DA axes to consider as needed (see ?dapc() for more details).       
To use custom colours, set the path to a csv file with header 'collection' and 'colour' containing the population name and colour name to set your custom colours.      
Note: if you retain more than one discrimant function, the allele loading plot will plot variance contributions of the first two discriminant functions only.       


## 09.1 Calculate relatedness
First, convert data (SNP or microsat) from genind to relatedness format and calculate relatedness values:      
`relatedness_calc(data = obj_pop_filt, datatype = "SNP")`        
...this will output to `03_results/kinship_analysis_<date>.Rdata`      

Note: if you are using microsat data, this will depend that you have run 'Rename microsat pops to SNP pop names' above, so that the pop names can be converted to stock codes.        

Second, plot your results:      
`relatedness_plot(file = "03_results/kinship_analysis_<date>.Rdata", same_pops = TRUE, plot_by = "names")`     

note: if you don't want to convert the current pop designations (e.g., if you don't have a stock code file), then set `plot_by = "codes` to avoid any conversion.       

...where you can use either "names" or "codes" if using only same-on-same.      
...and if you set `same_pops` to FALSE, you will get all pops pairwise comparisons. (but can't use names)      


## 09.2 Population marker HWE evaluation summary
To run a test of whether markers are following Hardy-Weinberg proportions, use the following function to find the number and percentage of markers per population that deviate from H-W proportions:          
`hwe_eval(data = <obj>, alpha = 0.01)`          
...to produce 'HWE_result_alpha_0.01.txt'          

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

NOTE: Script only works if region was *NOT* appended in step 2 (`fix_pop_names()` or `update_pop_names()` depending on datatype).

Convert genepop to rubias (SNP):     
`genepop_to_rubias_SNP(data = obj, sample_type = "reference")`      
        
Convert genepop to rubias (microsatellite):    
`genepop_to_rubias_microsat(data = obj_pop_filt, sample_type = "reference", micro_stock_code.FN = "</path/to/stock/repunit/file>")`        

For sample_type - must be a choice of "reference" or "mixture" - in almost all cases the default "reference" will be preferred in this pipeline, for both SNPs and microsats.


*For microsatellite data* To convert the microsat data to rubias, the repunits per stock code must be specified. Use the same stock code file as was used to rename data in step 'Rename microsat data' above. It can be named anything, but make sure it has column names 'collection', and 'repunit', and the collection names should match those ones in the data.      
This will output 'rubias_output_microsat.txt' in your results folder, which can be used for simulations (below).     
Please note: this currently assumes you have stock name followed by a four digit year identifier in your individual name.    

If using custom format (i.e., not in MGL_GSI_SNP format), use the following (SNP only):            
`genepop_to_rubias_SNP(data = obj, sample_type = "reference", custom_format = TRUE, micro_stock_code.FN = micro_stock_code.FN)`         
This will require that you have a stock code file (assign the variable `micro_stock_code.FN`), as well as a tab-delim file, `02_input_data/my_data_ind-to-pop_annot.txt` in the format of:       
```
indiv   pop
101 JPN
1847    FRA
```


## 13.b. Rubias utilities
### Re-add sample name (TTTT_LL) to an allele file
Allele files are created using XXX, but in any case you won't have the sample name anymore. To add this back onto the dataframe, use:       
`add_sample_name(input_type = "allele", base = TRUE)`      
...after which you will be prompted to select an allele file (i.e., output of `rubias_to_alleles()` of MGL_GSI_Analysis.     

### Separate a rubias baseline file by year
Method to separate a rubias baseline file (SNP only) by year for downstream simulations        
`sep_rubias_by_yr(rubias_base.FN = <rubias_base.FN)>`       
Note: could use additional testing to validate method.      


## 14. Run simulated individual assignment test
Using a rubias baseline, perform simulations to test baseline power using the rubias function [assess_reference_loo()](https://rdrr.io/cran/rubias/man/assess_reference_loo.html) as follows:        

For **100% simulations**           
`full_sim(rubias_base.FN = <full path to rubias baseline>, num_sim_indiv = 200, sim_reps = 100)` ...saves output into 03_results, including:        
- collection_100_stats_YYYY-MM-DD.txt (main item: summary info of sum of all iteration assignments)
- collection_100_stats_all_pops_YYYY-MM-DD.txt (full info on collection)
- collection_100_stats_all_reps_YYYY-MM-DD.txt (full info on repunits)
- all_collection_results_YYYY-MM-DD.txt.gz (raw output of all sims)
- matrix-style table (used in specific cases, typically with reduced regional baseline analyses)
Save to a separate folder to make sure they don't get written over.    

Parallel option to speed up (**for linux only**), add flag: `ncore_linux = <# cores to use>`.        
To install follow instructions in full_sim.R because requires a 'remote' repo install.       
Assume 20 Gb per thread. Note: if it produces warnings, it is likely there will be populations that did not complete due to lack of RAM (start over with fewer threads).          

Additional rollups: 
- If `region` is specified as a header in the repunits file, can add flag: `region=TRUE` to output summary files rolled up to region in addition to repunit.
- If you had other non-repunit groupings of collections to test, can add flag: `custom_rollup=TRUE` and `custom_rollup.FN =<path/to/custom/groupings/file>`. Custom groupings file is expected to have headers `Code	collection	group	Display_Order` with group defining custom reporting units for each collection.       
Note: Additional rollups are considered one or other - cannot run both at the same time currently.        


Note: use thse following function to create a 'Display Order' sorted result file for browsing using the repunit file (do not use for plotting):        
`format_sims_output()` # and select manually your input 100% sims stats text file. This will output to your results file folder.            

#### in development ####
For **realistic fishery scenarios**, first use the following function to make a proportions template, then manually edit the file to the custom proportions:          
```
# Prepare proportion file
prep_ppn(rubias_base.FN = <rubias.FN>, ppn.FN = "ppn.txt")
# ...then edit to add proportions in <result.path>/<ppn.FN>

# Run simulation using the ppn file
full_sim(rubias_base.FN = <rubias.FN>
         , num_sim_indiv = 200
         , sim_reps = 100
         , proportions.FN = paste0(current_path_to_set, "ppn_north_orgn.txt")
         )
```

NOTE: not tested with region = TRUE, 

#### end in-development ####

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
Plot summaries of assignment per repunit based on results of the 100% simulations.      
Requires 2 files produced in `full_sim`, and these are selected interactively so they can be anywhere on your computer.     
- collection_100_stats_all_reps_YYYY-MM-DD.txt (not just the top repunit assignment)
- collection_100_stats_all_pops_YYYY-MM-DD.txt (not just the top collection assignment)

```
plot_summarize_100_sim(axis_label="repunit",repunits_file = TRUE,
                          plot_prefix = "summarize_100_sim",plot_colls = FALSE,
                          width=8,height=11,pdf_png="pdf")

# plot_colls = TRUE    # will plot by individual collections instead of summarized by repunit
# plot_prefix          # allows to change the output filename
# repunits_file = TRUE # allows for selection (interactively) of a regional roll-up if a column is added to the repunits file 'region' (beta)    
                       # also allows for choosing axis label (e.g., CU, CU_Name, repunit) (beta)
```

If microsats - `repunits_file = FALSE` is a must, or you need to create a repunits file. Minimum necessary columns are:     
 "Display_Order", "CU", "CU_NAME", and "repunit"    
With Display_Order column used to sort the figure from lowest (top) to highest (bottom). Without the repunits file it will still plot, but default to reverse alphebetical.     
    

Note: could use the - collection_100_stats_YYYY-MM-DD.txt file probably, but would break the regional roll-up.

## 16. Summarize a rubias base for collections, years and total N
Summarize a filtered rubias base (SNP or microsat), and export a table reporting the repunit, CU number, collection, years per collection, and total N.      
This is a commonly produced table for publication or baseline benchmarks.     
Both the **microsatellite and SNP versions** require a repunit file. This file should be tab-delimited, with columns "Display_Order", "CU", "CU_NAME", and "repunit".           

```
summarise_rubias_baseline(baseline = <rubias_base filename>
                          , out_prefix = "rubias_base_summary"
                          , repunit_desc = <repunits filename>
                          , by_year=<TRUE or FALSE>
                          , type = "<SNP or microsat>"
                          )

```

This function takes a filtered baseline, an output prefix and a repunit file. Will export to results path.      
Do not provide a path in the output prefix, only a prefix for the output file.       
Tacked on to the prefix will be `.baseline_summary.txt` and output will be tab delimited.      
Example:

With `by_year=FALSE`, no tally by year will be conducted:       
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

With `by_year=TRUE`, tally by year will be conducted:      

|                                    |           |                     |                                      |    | 
|------------------------------------|-----------|---------------------|--------------------------------------|----| 
| Region/Conservation Unit           | CU Number | Population          | Years(N)                             | N  | 
| Middle Skeena-mainstem tributaries | 54        | BULKLEY_RIVER-LOWER | 1999(96)                             | 96 | 
|                                    |           | KISPIOX_RIVER       | 2004(59), 2006(28), 2008(3), 2010(8) | 98 | 
|                                    |           | KITSEGUECLA_RIVER   | 2009(95)                             | 95 | 
|                                    |           | KITWANGA_RIVER      | 2003(93)                             | 93 | 





## 17. Summarize the Juvenile column in the Extraction sheets

To create a summary of collections that are known to contain Juveniles, run:

```
juvenile_count(by_year=TRUE)
```

`by_year = TRUE` will identify specific years in the baseline, while `by_year = FALSE` will simply report the collection name.

ONLY TESTED IN SNPS - CHINOOK + COHO

## 18. Determine highest tray number in Rubias baseline

To determine the highest tray number in a rubias baseline, run:

```
highest_tray()
```

It will prompt to select a rubias baseline, then match the individual IDs to the baseline extraction sheet. The highest tray number for an individual in the baseline will be exported to a file `<two.letter.code>_highest_tray_number.txt` in the folder `03_results/`

ONLY TESTED IN SNPS


## 19. Convert Rubias to VCF     

To convert a rubias file to a more common VCF format, use:     

`rubias_to_vcf(output.fn = "03_results/convert_from_rubias.vcf")`     

It will request to interactively select a `*_rubias.txt` file and a `*.hs.bed` in the Proton hotspot format. It will also use the default `<species>_hotspot_detail.txt` file to do some naming conversions. 

This file should deal properly with insertions and deletions markers. It was tested in our coho panel, but not yet in others so beware. Will only work in SNPs - uSat would require significant work to make happen here. 


## 20. VCF to GEMMA conversion and analysis
Requires: 
- GEMMA
- input VCF file (or compressed vcf.gz)   

Note: currently under development, and so the function needs to be sourced from the `01_scripts/dev` folder.    

Use the following utility in R to convert the VCF file to GEMMA input format:      
`vcf_to_gemma(vcf_path="02_input_data/my_vcf.gz")`     
Output will provide a space-separated text file in the result path titled `gwas_geno.txt`.       

GEMMA steps:        
*in development*     

