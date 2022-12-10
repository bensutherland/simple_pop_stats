#Making a map

install.packages("tmap")
install.packages("sf")
install.packages("terra")
install.packages("dplyr")
install.packages("spData")
install.packages("spDataLarge")
install.packages("ggplot2")
install.packages("leaflet")
install.packages("leaflet.esri")

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
stock.codes <- stock.codes %>% drop_na(YLAT)

map_points <- stock.codes %>%
  st_as_sf(coords = c("XLONG","YLAT"), crs=4326) %>% 
  st_cast("POINT")

map_points <- st_shift_longitude(map_points)

map <- tm_shape(map_points) + 
  tm_dots(group="ProvState",
          col="ProvState",
  palette = "Dark2",
  popup.vars=TRUE) +
  tm_basemap(server = "Esri.WorldTopoMap")
map


