#Making a map

#install.packages("tmap")
#install.packages("sf")
#install.packages("terra")
#install.packages("dplyr")
#install.packages("spData")
#install.packages("spDataLarge")
#install.packages("ggplot2")
#install.packages("leaflet")
#install.packages("leaflet.esri")

library(sf)
library(terra)
library(dplyr)
library(spData)
#library(spDataLarge)
library(tmap)
library(leaflet)
library(leaflet.esri)
library(ggplot2)
library(tidyverse)



#interactive_map(stock.codes_fn = "W:/9_PBT/01_chum/reference_databases/bcm_SNP_coastwide_v.5.0.1_2022-12-09_IYS/cmStockCodesCU.txt", 
#               repunits_fn="W:/9_PBT/01_chum/reference_databases/bcm_SNP_coastwide_v.5.0.1_2022-12-09_IYS/repunits_full_AK-IA.txt", 
#              plot_by = "region", filter_by_baseline=TRUE,
#             my_baseline.path = "W:/9_PBT/01_chum/reference_databases/bcm_SNP_coastwide_v.5.0.1_2022-12-09_IYS/bcm_SNP_coastwide_v.5.0.0_2022-08-22_n35_535amp.txt")




interactive_map <- function(stock.codes_fn = sc.base, repunits_fn = NULL, plot_by = "repunit", filter_by_baseline = TRUE, my_baseline.path = NULL){

  
# Open interactive viewer
tmap_mode("view")

# Read in 
stock.codes <- read.delim(file=stock.codes_fn)

# Use repunits file if provided
if(!(is.null(repunits_fn))){
  repunits <- read.delim(file=repunits_fn)
  joined <- base::merge(repunits,stock.codes,by="repunit",all.y=TRUE)
} else {
  joined <- stock.codes
}
# Merge Stock Codes and Repunits file  



# Select a baseline to filter by only collections in base
if(filter_by_baseline ==TRUE){
  if(is.null(my_baseline.path)){

    print("Select your genepop from file...")
    if(.Platform$OS.type == "unix") {
  
        my_baseline.path <- tk_choose.files(caption = "Select a rubias baseline file" )
  
    } else if(.Platform$OS.type == "windows") {
  
        my_baseline.path <- choose.files( default=file.path("W:/9_PBT/")
                                          , caption = "Select a rubias baseline file")
  
    }
  }
  # Read baseline
  baseline <- read_tsv(file=my_baseline.path)

    
  # Filter by baseline collections
  joined <- filter(joined,collection %in% baseline$collection)

}

# Move collection to first column
joined <- joined %>% select(collection,everything())

# Drop anything without GPS coordinates
joined <- joined %>% drop_na(YLAT)

# Generate map points
map_points <- joined %>%
  st_as_sf(coords = c("XLONG","YLAT"), crs=4326) %>% 
  st_cast("POINT")

# Shift to a Pacific focused map
map_points <- st_shift_longitude(map_points)

# Add points to map
map <- tm_shape(map_points) + 
  tm_dots(group=plot_by,
          col=plot_by,
  palette = "Dark2",
  popup.vars=TRUE,
  size=0.1) +
  tmap_options(max.categories = 100) +
  #tm_markers(text="collection") +
  tm_basemap(server = "Esri.WorldTopoMap")

# Visualize map
map

}
