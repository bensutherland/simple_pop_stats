# Appendix A - Benchmarking document


## Methods to produce the baseline summary document using Rmarkdown.

**NOTE:** Work in progess, what is included in the document, and formatting of the document will likely change.

(initialized 2020-08-18)

The goal of this appendix is to walkthrough the minimum requirements for generating a summary document that describes a particular genetic baseline build. It is expected that the base of such an assessment will be kept in a common location, and not modified once this document is produced - should changes need to be made, a new time-stamped folder will be generated, and the summary document produced once again.



## A) Generate necessary data and file structure


### Step 1: Baseline formats

It is assumed at this point that the data exists in [rubias](https://github.com/eriqande/rubias#input-data) format . If not, please ensure that a Rubias-formatted baseline exists.

It is also advised that the data exists in [genepop](https://genepop.curtin.edu.au/help_input.html#Input) format . This can be accomplished a number of ways, but if access is available to the `MGL_GSI_SNP` github repo, a script exists to make the conversion (Currently in `01_scripts/PBT_Menu_functions/z-development/rubias_to_genepop.r`) - if access is required please contact the `simple_pop_stats` author or contributors.

These files should exist at the root of a date-stamped and version stamped folder. In MGL, these exist on:

` W:\9_PBT\01_<species>\reference_databases\<UNIQUE-DATE-STAMP-VERSION-STAMP-FOLDER>`

An example folder name is `2020_V1.0_2020-08-05_baseline` for chinook. 

Additional files can be stored at the root of this folder, and any that appear here will ultimately be added to md5 listed files in the summary. Ultimately, this is meant to store files that led to subsequent summary formats, so it is recommended that these files represent the minimum requirements to re-create the analyses. If multiple files do exist, please make it clear why (eg. PBT-base vs. GSI-base if certain collections are used for PBT but dropped from GSI).



### Step 2: Nested folder structure

In the newly created folder, please generate the following folders to store summary files:
 * `Baseline_summary`
 * `Dendrogram`
 * `100_sims`
 * `LOO` (TBD)
 * `FST` (TBD)
 
 
 
### Step 3: Version-change summaries

Within the newly created `Baseline_summary` folder, it is recommended to include 2 files with the following suffixes or file names:

- `*changes.txt`
- `*notes.txt`

In the "changes" file, this is a two-column format to describe Version changes between baselines. It is recommended that this is a file that is appended to, such that all changes are tracked back to the initial document. An example from Chinook would be:

```
Baseline Version	Changes from prior baselines
2020_V1.1	1) changed name of CHILLIWACK_RIVER to CHILLIWACK_RIVER_summer
	2) changed name of CHEHALIS_RIVER to CHEHALIS_RIVER_summer
	3) created new repunit LFR-suppl and associated it with CK-9006 Fraser supplementation exclusion bin. 
	4) assigned both CHEHALIS_RIVER_summer and CHILLIWACK_RIVER_summer to new "suppl" repunit. 
	5) Chehalis moved back into GSI baseline (was previously PBT only)
	Changes were made to better describe these transplanted, but genetically distinct collections. Grouping together under the new repunit is based primarily off of dendrogram results in version 1.0 of the baseline.
2020_V1.2	Nicola 2016 were added into the baseline. While they had been genotyped, they had not been added to the baseline previously due to a database error. 
2020_V1.3	1) Cle Elum hatchery repunit changed from NCOR to MCR-Sp
	2) Spring Creek Hatchery changed from MCR-Sp to LCR
	3) Naches River is dropped from baseline, as position in the dendrogram implies it may not be correctly identified (groups with Puget Sound, should be Middle Columbia Spring)
```

In the "notes" file, this is a two-column format of "collection" and "notes" that details specific notes on particular collections. This allows for tracking "known issues" - things that we are aware of or changes purposely made that might otherwise be questioned. An example from Chinook would be:

| collection              | notes                                                                                                                                                                                                 | 
|-------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------| 
| CAPILANO_RIVER          | Is used for PBT only, as direct transplanting from Chilliwack River-fall interferes with GSI.                                                                                                         | 
| GOLD_RIVER              | Is used for PBT only, as high-frequency of Robertson strays interfere with GSI. Moved from NoKy (official CU) to SWVI for same reason, as genetically it is very similar to Robertson due to straying | 
| CHEHALIS_RIVER_summer   | Has been placed into CK-9006 (LFR-suppl) to reflect the historical transplanting from the upper Fraser                                                                                                | 
| CHILLIWACK_RIVER_summer | Has been placed into CK-9006 (LFR-suppl) to reflect the historical transplanting from the upper Fraser                                                                                                | 
| CHEAKAMUS_RIVER_FALL    | Moved from SMn-GStr (official CU) to QP-fall as genetically it is very similar to Qualicum due to historical transplanting                                                                            | 
| MEGIN_RIVER             | Moved from SWVI (official CU) to NoKy as genetically it is very similar to Conuma due to straying                                                                                                     | 
| MOYEHA_RIVER            | Moved from SWVI (official CU) to NoKy as genetically it is very similar to Conuma due to straying                                                                                                     | 



### Step 4: Rubias-based summaries


A) Run `summarise_rubias_baseline()` to produce a summary of the rubias baseline. See [summarise_rubias_baseline](https://github.com/bensutherland/simple_pop_stats#16-summarize-a-rubias-base-for-collections-years-and-total-n) for more info. It is recommended here that `by_year = TRUE`, in order to report collection sizes by `N` separated by year; it is also recommended to use an informative prefix (default: `out_prefix = "rubias_base_summary"`). The resulting file will be sent to `03_results` - please copy and paste to the newly created `Baseline_summary` folder in step 2.

The file naming is flexible, but the Rmarkdown  script recognizes the string `*baseline_summary.txt` [default suffix] so ensure that this string is not disrupted, and remains unique in the folder. It is therefore recommended only to change the prefix of the file.


B) Run `full_sim()` to produce summary tables from the 100% simulations in rubias. See [full_sim](https://github.com/bensutherland/simple_pop_stats#14-run-simulated-individual-assignment-test) for more info. All resulting files (output to `03_results`) are recommended to be stored in the `100_sims` folder created in Step 2:
- `all_collection_results_<date>.txt.gz`
- `collection_100_stats_<date>.txt`
- `collection_100_stats_all_pops_<date>.txt`
- `collection_100_stats_all_reps_<date>.txt`
- `collection_100_stats_all_pops_matrix_<date>.txt`

The file naming is flexible, but the Rmarkdown  script recognizes the string `*100_stats_2.*.txt$` so ensure that this string is not disrupted, and remains unique in the folder. It is therefore recommended only to change the prefix of the file. 
  

C) Run `plot_summarize_100_sim` to produce an image summary of the 100% simulation results. See [plot_summarize_100](https://github.com/bensutherland/simple_pop_stats#15-plot-mean-assignment-per-repunit-from-100-sim) for more info. The default file (`03_results\summarize_100_sim_plot.pdf`) should be copied to the 

The file naming is flexible, but the Rmarkdown script recognizes the string `*plot.pdf$` so ensure that this string is not disrupted, and remains unique in the folder. It is therefore recommended only to change the prefix of the file. 

D) Run `highest_tray()` to produce a file that has the highest tray number currently in the rubias baseline, based on matching to the extraction sheet. This file, `<two.letter.code>_highest_tray_number.txt` should be placed in the Baseline_summary folder, and requires the suffix `*highest_tray_number.txt$`.  

### Step 5: Genepop-based analyses


A) Running simple_pop_stats steps 1-3 will generate a file called `03_results/number_of_markers.csv`. Please copy and paste to the newly created `Baseline_summary` folder in step 2 in order to display the number of markers in the baseline.


B) Running simple_pop_stats steps 1-7 will allow for the dendrogram to be produced. There are multiple ways to produce and format the results, and multiple formatting options for displaying the output. Ultimately, a single `*.pdf` file is needed in the `Dendrogram` folder to be included in the summary document. Should multiple images be created, it is recommended that they are stored in a nested folder - the Rmarkdown script does not look recursively. 

Things to consider when formatting the output:
* i) R markdown will incorporate the image as it is formatted in the PDF. 

* ii) If it is on a single page, it will incorporate it as a single page; if split across multiples, it will incorporate it as multiples.

* iii) Please be sure you are happy with the way it appears in the initial PDF. 

* iv) MGL has been using [FigTree](http://tree.bio.ed.ac.uk/software/figtree/t) to view and format the trees

* v) The "print" option combined with "print to PDF" in Adobe Acrobat Reader DC can be used to create a multi-page PDF from the single page PDF exported from FigTree

* vi) It is recommended that "Expansion" be used to spread the tree, and "Tip Labels" and "Node Labels" font sizes be increased to allow easier reading with minimum zooming.

* vii) Don't forget in FigTree to ensure bootstrapping is selected for the "Node Labels"

The file naming is flexible, but the Rmarkdown script recognizes the string `*.pdf$` so ensure that this string is not disrupted, and remains unique in the folder. 


C) LOO - TBD


D) Fst - TBD



## B) Run the Rmarkdown script


### Open and edit the script

The script is kept at `01_scripts\baseline_benchmark.Rmd` currently, within the Simple Pop Stats package. 

As written, it currently requires user intervention on two lines:

* [folder](https://github.com/bensutherland/simple_pop_stats/blob/b6a45553761486b1eff7669f2d61938817bc77ed/01_scripts/baseline_benchmark.Rmd#L21) needs to be set with the folder name (not full path) of the baseline to be considered. Should be in the standard ` W:\9_PBT\01_<species>\reference_databases\` place.
* [species](https://github.com/bensutherland/simple_pop_stats/blob/b6a45553761486b1eff7669f2d61938817bc77ed/01_scripts/baseline_benchmark.Rmd#L22) needs to be set with the species name. Currently one of: `chinook`; `coho`; `pink`; `eulachon`; `sockeye`; `chum`



### Run the script

Press `Knit` at the top of the script in Rstudio! Should automatically output the necessary document. Currently writes to the same folder as the script (01_scripts - default behaviour), so drag it over to the root folder for the baseline

 - TBD to re-direct to the working folder, likely via a `rmarkdown::render()` wrapper script

**Note**: outputing a "Float too large" error relatively regularly as a warning message. Likely to do with trying to shove too much info into too small a page. However, I have not noticed data loss. Please let me know if you do.


