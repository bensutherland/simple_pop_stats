# Appendix A - Benchmarking document

## Produce the baseline benchmark document using Rmarkdown.
     
### If you encounter errors related to Ghostscript or TinyTex, please see this issue for the fix: https://github.com/bensutherland/simple_pop_stats/issues/29     


Walkthrough the minimum requirements for generating a summary document that describes a particular genetic baseline build. The foundation of such an assessment will be kept in a common location and **will not be modified once this document is produced**. If changes need to be made, a new time-stamped folder will be generated, and the summary document needs to be produced again.     

New: automated benchmark folder creation with:       
`prep_benchmark_folder(mtype = "SNP", scope = "coastwide", version = "X.X.X")`      
...will prepare your folder structure and will put the current network stock code and repunit file in the necessary location.       
(note: still relatively experimental, could use some more evaluation)      

## 00. Front matter: naming and versioning conventions
**Naming conventions** of the benchmark:     
```
b<sp>_<mtype>_<scope>_v.X.X.X
e.g., beu_SNP_coastwide_v.1.0.1 (SNPs) or bso_msat_skeena_v.1.0.0 (microsats)
```
note: capitals for acronyms only.      

**Versioning conventions**          
First baseline initialization for a species that would be considered "ready-for-use" will be v.1.0.0.       
Before initialization, use the v.0.x.x. designations, incrementing by one each time REGARDLESS of how much has changed. All v0.x files will be treated "as-is", with the understanding that great changes will likely occur prior to use.     

Conventions starting from the initial version v.1.0.0:      

Major (v.**X**.0.0) changes:      
* Add markers to the baseline
* Add a year's worth of broodstock to the baseline
* Create a new repunit
* Change the name of a repunit
* Collapse two repunits into one
* Remove a large number of markers (>=10) from the SNP baseline (or >=1 from uSat)

Minor (v.0.**X**.0) changes:     
* Move pops across repunits that already exist
* Add a new collection to an existing repunit
* Remove a collection, or a significant number of individuals within a collection, from the baseline
* Remove a handful (<10) of poorly performing SNP markers from the baseline
* Addition of significant number of individuals to existing collection (roughly n>=50 or 50% increase)

Very Minor (v.0.0.**X**) changes:
* Change the spelling of a collection name
* Add or remove small numbers of individuals from the baseline (<50)

Reduced baseline scope changes:    
* Change the `<scope>` to something appopriate that reflects the change from Coastwide, but keep versioning number from the source baseline (eg. Fraser_v.3.0.0 would be a Fraser-scope baseline created from the v.3.0.0 coastwide.    
	

## 01. Create file structure and collect inputs
### 01.(A) Folder structure
Create a new folder at `W:\9_PBT\01_<species>\reference_databases\<UNIQUE-DATE-STAMP-VERSION-STAMP-FOLDER>`. Use naming convention described above but with date-stamp (e.g., `bso_msat_coastwide_v.1.0.0_2021-02-08`)       

In this folder, create the following folders (exact names required):         
 * `Baseline_summary`
 * `Dendrogram`
 * `100_sims`
 * `LOO` (_in development_)
 * `FST` (_in development_)

### 01.(B) Add baseline inputs
At the root of your new directory, include a [rubias](https://github.com/eriqande/rubias#input-data) baseline and a [genepop](https://genepop.curtin.edu.au/help_input.html#Input) baseline. If you are working with microsatellite data, also include a BCF and BSE file matching these.      
Note: these should all be identical in character (e.g., # indiv., # collections, etc.) and produced at the same time.      

**SNP baselines** should also include the repunits and stock code files used to produce the baseline. This allows tracking of collection name change or repunit delineation change.       

**microsat baselines** should include a names.dat file here (stock code/groupings), but also recommended to build a `repunits_full.txt`-style file in order to apply Display Order, etc. in summarizing the data.      

Additional files can be stored at the root of this folder, and any that appear here will ultimately be added to md5 listed files in the summary. Ultimately, this is meant to store files that led to subsequent summary formats, so it is recommended that these files represent the minimum requirements to re-create the analyses. If multiple files exist, please make it clear why in the notes.txt file (see below) (eg. PBT-base vs. GSI-base if certain collections are used for PBT but dropped from GSI).

**NOTE:** It is strongly recommended to rename these baseline files using the same naming convention for the folder eg:         
```
b<sp>_<mtype>_<scope>_v.X.X.X
e.g., beu_SNP_coastwide_v.1.0.1 (SNPs) or bso_msat_skeena_v.1.0.0 (microsats)
```

It should include the file-type suffix to identify file-type (eg. `.gen` for genepop), or specifically name the file-type for more generic file-type suffixes (eg. `_rubias.txt` for rubias formatted text file). This allows a client to identify the baseline used by the version number and date in the file name. 

### 01.(C) Version-change summaries
Add to the `Baseline_summary` folder two files with the following suffixes or file names:      
- `*changes.txt`, a two-column tab-delimited file describing version changes between baselines.      
- `*notes.txt`, a two-column tab-delimited file describing details on specific collections (i.e., known issues)   
     
**important note #1:** carry forward changes.txt and notes.txt from previous baseline increments to bring forward previous notes. Append new notes to bottom of file.   

**important note #2:** changes and notes files must either be a) completely absent from the folder (in which case a message about "no changes/notes file present" will be noted in the benchmark), or present with information within it (eg. headers, at least one line of information even if "No changes noted"). A present but empty file will cause an error in knitting.   

**important note #3:** if the scope of the baseline is sub-Coastwide, but it is being derived directly from a specific coastwide baseline (eg. Using only two CUs, but by reducing the V3.0.0 baseline and not recreating from scratch), have the changes file note: "For this subset baseline, changes don't describe baseline versioning, but rather changes relative to the originating baseline [`<original baseline name>`], then subsequently describe any changes that make this sub-coastwide scope baseline unique (eg. split CU-A into CU-A1 and CU-A2).     

Example changes.txt file:        
```
Baseline Version	Changes from prior baselines
bch_Coastwide_v.1.1.0	1) changed name of CHILLIWACK_RIVER to CHILLIWACK_RIVER_summer
	2) changed name of CHEHALIS_RIVER to CHEHALIS_RIVER_summer
	3) created new repunit LFR-suppl and associated it with CK-9006 Fraser supplementation exclusion bin. 
	4) assigned both CHEHALIS_RIVER_summer and CHILLIWACK_RIVER_summer to new "suppl" repunit. 
	5) Chehalis moved back into GSI baseline (was previously PBT only)
	Changes were made to better describe these transplanted, but genetically distinct collections. Grouping together under the new repunit is based primarily off of dendrogram results in version 1.0 of the baseline.
bch_Coastwide_v.1.2.0	Nicola 2016 were added into the baseline. While they had been genotyped, they had not been added to the baseline previously due to a database error. 
```

Example notes.txt file: 
```
collection	notes
CAPILANO_RIVER	Is used for PBT only, as direct transplanting from Chilliwack River-fall interferes with GSI.
GOLD_RIVER	Is used for PBT only, as high-frequency of Robertson strays interfere with GSI. Moved from NoKy (official CU) to SWVI for same reason, as genetically it is very similar to Robertson due to straying
MOYEHA_RIVER	Moved from SWVI (official CU) to NoKy as genetically it is very similar to Conuma due to straying
```

### 01.D) Rubias-based summaries
Ensure the following were run and collect outputs and copy to benchmark folders as follows:       
**Baseline summary** to `Baseline_summary` folder. This file is generated with `summarise_rubias_baseline()` (see [summarise_rubias_baseline](https://github.com/bensutherland/simple_pop_stats#16-summarize-a-rubias-base-for-collections-years-and-total-n)).       
note: `by_year = TRUE`, and `out_prefix = "rubias_base_summary"` are recommended. 
note: suffix default `*.baseline_summary.txt` is required.     

**100% simulations** to `100_sims` folder. These files are generated with `full_sim()` (see [full_sim](https://github.com/bensutherland/simple_pop_stats#14-run-simulated-individual-assignment-test)).       
note: the string `*100_stats_2.*.txt$` is required, and must be unique in the folder. It is therefore recommended only to change the prefix of the file.       
Include:     
- `all_collection_results_<date>.txt.gz`
- `collection_100_stats_<date>.txt`
- `collection_100_stats_all_pops_<date>.txt`
- `collection_100_stats_all_reps_<date>.txt`
- `collection_100_stats_all_pops_matrix_<date>.txt`

**Simulations plot** to `100_sims` folder. This file is generated with `plot_summarize_100_sim` (see [plot_summarize_100](https://github.com/bensutherland/simple_pop_stats#15-plot-mean-assignment-per-repunit-from-100-sim). 
note: the string `*plot.pdf$` is required, and must be unique in the folder         

**Highest Tray** to `Baseline_summary` folder _(SNP only, optional)_. This file is generated with `highest_tray()` to produce a file that has the highest tray number currently in the rubias baseline, based on matching to the extraction sheet.         
note: requires the suffix `*highest_tray_number.txt$`      

### 01.E) Genepop-based summaries
**Number Markers** to `Baseline summary` folder. Generate with `characterize_genepop()` to produce `number_of_markers.csv`.      
     
**Dendrogram** to `Dendrogram` folder. Generate with `make_tree()`, modify as needed in FigTree, and then save a PDF.     
Copy both the PDF and the .tre file to the folder. Only the PDF will be copied into the benchmark.       
It is possible to have multiple dendrogram PDFs
#todo#: (not clear) -- created, it is recommended that they are stored in a nested folder - the Rmarkdown script does not look recursively.     

Consider when formatting the dendrogram for display:     
* i) R markdown will incorporate the image as it is formatted in the PDF, so ensure you are happy with the format in the PDF. 
* ii) If it is on a single page, it will incorporate it as a single page; if split across multiple pages, it will incorporate it as multiple pages.
* iii) Use [FigTree](http://tree.bio.ed.ac.uk/software/figtree/) to view and format the trees
* iv) To split your PDF, use the "print" option in Adobe Acrobat Reader DC, combined with "print to PDF" to create a multi-page PDF from the single page PDF exported from FigTree
* v) It is recommended that "Expansion" be used to spread the tree, and "Tip Labels" and "Node Labels" font sizes be increased to allow easier reading with minimum zooming.
* vi) Don't forget in FigTree to ensure bootstrapping is selected for the "Node Labels"

note: the file naming is flexible, but the Rmarkdown script recognizes the string `*.pdf$` so ensure that this string is not disrupted, and remains unique in the folder. 

#todo#
LOO - _in development_
Fst - _in development_

## 02. Create the benchmark PDF
### Open and edit the script
Open `01_scripts\baseline_benchmark.Rmd`, and edit the following:       

* folder: needs to be set with the folder name (**not full path**) of the target baseline. The script assumes that this folder is in the standard location: ` W:\9_PBT\01_<species>\reference_databases\`.                    
* species: needs to be set with the species name in double quotes. Currently one of: "chinook", "coho", "pink", "eulachon", "sockeye", or "chum".            

* datatype: needs to be set with the data type, either "SNP" or "microsat".        
* min_loci: needs to be set with the minimum number of loci allowed to keep an individual in the baseline. Numeric value (no quotes). eg. `150`     

### Run the script
Press `Knit` at the top of the script in Rstudio! Should automatically output the necessary document. Currently writes to the same folder as the script (01_scripts - default behaviour), so drag it over to the root folder for the baseline

**Common Issues**:       
- outputs a "Float too large" warning message regularly. Likely to do with trying to shove too much info into too small a page. Data loss has not been noted by developers, but please contact if you identify this occurring.    
- render crashes or states that LaTeX rendering is not enabled. If this occurs, confirm tinytex is loaded, and also that all necessary input files are included in all of the folders.      
