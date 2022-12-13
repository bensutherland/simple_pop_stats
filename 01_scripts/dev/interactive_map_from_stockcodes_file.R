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


tmap_mode("view")


stock.codes <- read.delim(file="H:/Stock_Codes/Chum/cmStockCodesCU.txt")

repunits <- read.delim(file="W:/9_PBT/01_chum/reference_databases/bcm_SNP_coastwide_v.5.0.1_2022-12-09_IYS/repunits_full_AK-IA.txt")
joined <- merge(repunits,stock.codes,by="repunit",all.y=TRUE)

baseline <- read_tsv(file="W:/9_PBT/01_chum/reference_databases/bcm_SNP_coastwide_v.5.0.1_2022-12-09_IYS/bcm_SNP_coastwide_v.5.0.0_2022-08-22_n35_535amp.txt")


joined <- filter(joined,collection %in% baseline$collection)
joined <- joined %>% select(collection,everything())

joined <- joined %>% drop_na(YLAT)


map_points <- joined %>%
  st_as_sf(coords = c("XLONG","YLAT"), crs=4326) %>% 
  st_cast("POINT")

map_points <- st_shift_longitude(map_points)

map <- tm_shape(map_points) + 
  tm_dots(group="region",
          col="region",
  palette = "Dark2",
  popup.vars=TRUE) +
  #tm_markers(text="collection") +
  tm_basemap(server = "Esri.WorldTopoMap")
map


