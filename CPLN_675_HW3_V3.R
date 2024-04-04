#title: "CPLN 675: Homework 3"
#author: "Mimi Tran & Javier Fernandez"
#date: "April 3, 2024"
  #output:
  #html_document:
  #toc: true
#toc_float: true
#code_folding: hide
#code_download: true

  
# install
options(repos = "https://cloud.r-project.org/")
install.packages('caret')
install.packages('pscl')
install.packages('plotROC')
install.packages('pROC')
install.packages('sf')
install.packages('tidyverse')
install.packages('knitr')
install.packages('kableExtra')
install.packages('tigris')
install.packages('viridis')
install.packages('gstat')
install.packages('sp')
install.packages('raster')
install.packages('spatialreg')


# libraries
library(caret)
library(pscl)
library(plotROC)
library(pROC)
library(sf)
library(tidyverse)
library(knitr)
library(kableExtra)
library(tigris)
library(viridis)
library(gstat)
library(sp)
library(raster)
library(spdep)
library(sf)


## 1. SET UP 

mapTheme <- theme(plot.title =element_text(size=12),
                  plot.subtitle = element_text(size=8),
                  plot.caption = element_text(size = 6),
                  axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_line(colour = 'transparent'),
                  panel.grid.minor=element_blank(),
                  legend.direction = "vertical", 
                  legend.position = "right",
                  plot.margin = margin(1, 1, 1, 1, 'cm'),
                  legend.key.height = unit(1, "cm"), legend.key.width = unit(0.2, "cm"))

plotTheme <- theme(
  plot.title =element_text(size=12),
  plot.subtitle = element_text(size=8),
  plot.caption = element_text(size = 6),
  axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
  axis.text.y = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  # Set the entire chart region to blank
  panel.background=element_blank(),
  plot.background=element_blank(),
  #panel.border=element_rect(colour="#F0F0F0"),
  # Format the grid
  panel.grid.major=element_line(colour="#D0D0D0",size=.75),
  axis.ticks=element_blank())

# 1a. DATA WRANGLING
# Load Calgary shapefile
cal_fish <- st_read('~/HW3/calgary/fishnet', promote_to_multi = FALSE)

# Import Calgary data features
library(readr)
CA_Soil <- read_csv("HW3/calgary/features/CA_Soil.csv")
CA_Parks <- read_csv("HW3/calgary/features/CA_Parks.csv")
CA_Impervious <- read_csv("HW3/calgary/features/CA_Impervious.csv")
CA_Elevation <- read_csv("HW3/calgary/features/CA_Elevation.csv")
CA_Waterbodies <- read_csv("HW3/calgary/features/CA_Waterbodies.csv")

# I am renaming OBJECTID columns to specify variables so we know what "counts" or "sums" coincide with which variables
ca_soil <- rename(CA_Soil, soil_OBJID = OBJECTID)
ca_parks <- rename(CA_Parks, parks_OBJID = OBJECTID)
ca_impervious <- rename(CA_Impervious, impervious_OBJID = OBJECTID)
ca_elevation <- rename(CA_Elevation, elevation_OBJID = OBJECTID)
ca_waterbodies <- rename(CA_Waterbodies, water_OBJID = OBJECTID)

# joining CSVs to Calgary fishnet by OBJECTID_1
joined_fishcal <- cal_fish %>%
  left_join(ca_soil, by = "OBJECTID_1") %>%
  left_join(ca_parks, by = "OBJECTID_1") %>%
  left_join(ca_impervious, by = "OBJECTID_1") %>%
  left_join(ca_elevation, by = "OBJECTID_1") %>%
  left_join(ca_waterbodies, by = "OBJECTID_1") 

# plotting joined_fishcal, visualizing
ggplot() +
  geom_sf(data = joined_fishcal, aes(fill = SUM.x)) +  # plot low permeability soil # logic: cells near low permeability soils will be more likely to flood
  geom_sf(data = joined_fishcal, aes(fill = SUM.y)) +  # plot parks # logic: cells near parks will be less likely to flood
  geom_sf(data = joined_fishcal, aes(fill = SUM.x.x)) + # plot impervious # logic: cells near impervious surfaces will be more likely to flood 
  geom_sf(data = joined_fishcal, aes(fill = MIN.y)) + # plot elevation # logic: cells near the lowest points will be more likely to flood
  geom_sf(data = joined_fishcal, aes(fill = COUNT)) + # plot water bodies # logic: cells near water bodies will be more likely to flood
  scale_fill_gradient(low = "grey", high = "blue") +  # color
  theme_minimal()


## 2. CONDUCTING SPATIAL LAG  
# creating a spatial weights matrix using queen contiguity (queen =  defines neighbors as spatial units sharing a common edge)
w <- poly2nb(joined_fishcal, queen = TRUE)
w <- nb2listw(w)

# compute spatial lag with each variable
joined_fishcal$lag_soil <- lag.listw(w, joined_fishcal$SUM.x) # low permeability soil
joined_fishcal$lag_parks <- lag.listw(w, joined_fishcal$SUM.y) # parks
joined_fishcal$lag_imp <- lag.listw(w, joined_fishcal$SUM.x.x) # impervious surfaces
joined_fishcal$lag_elev <- lag.listw(w, joined_fishcal$MIN.y) # elevation
joined_fishcal$lag_water <- lag.listw(w, joined_fishcal$COUNT) # water

# 2a. PLOTTING SPATIAL LAG
# visualizing lagged variables
# Soil with Low Permeability = "Impervious Soil"
ca_soil_map <- ggplot() + 
  geom_sf(data = joined_fishcal, aes(fill = lag_soil)) +
  scale_fill_gradient(name = "Impervious Soil", low = "grey", high = "blue") +
  labs(fill = "Impervious Soil") +
  theme_minimal() # areas in blue are more likely to flood
ca_soil_map

# Presence of Parks
ca_park_map <- ggplot() + 
  geom_sf(data = joined_fishcal, aes(fill = lag_parks)) +
  scale_fill_gradient(name = "Parks", low = "blue", high = "grey") +
  labs(fill = "Parks") +
  theme_minimal() # areas in blue are more likely to flood # logic: where there is a "low" count of parks, cells are more likely to flood
ca_park_map

# Presence of Impervious Surface
ca_imp_map <- ggplot() + 
  geom_sf(data = joined_fishcal, aes(fill = lag_imp)) +
  scale_fill_gradient(name = "Impervious Surfaces", low = "grey", high = "blue") +
  labs(fill = "Impervious Surfaces") +
  theme_minimal() # areas in blue are more likely to flood 
ca_imp_map

# Low Elevation
ca_elev_map <- ggplot() + 
  geom_sf(data = joined_fishcal, aes(fill = lag_elev)) +
  scale_fill_gradient(name = "Low Elevation", low = "blue", high = "grey") + 
  labs(fill = "Low Elevation") +
  theme_minimal() # areas in blue are more likely to flood # logic: areas that are low in elevation are more likely to flood
ca_elev_map # notice you can make out the river! 

# Water Bodies
ca_water_map <- ggplot() + 
  geom_sf(data = joined_fishcal, aes(fill = lag_water)) +
  scale_fill_gradient(name = "Water", low = "grey", high = "blue") + 
  labs(fill = "Water") +
  theme_minimal() # areas in blue are more likely to flood 
ca_water_map 

# 3. PLOTS (3.2 of Conservation Predictive Modeling?) (at this point we should be moving out of Interpolating Air Pollution)
cal_variables <- joined_fishcal %>%
  mutate(
    centroid = st_centroid (geometry),
    X = st_coordinates(centroid)[, 1], # Extract X coordinate of the centroid
    Y = st_coordinates(centroid)[, 2]  # Extract Y coordinate of the centroid
  ) %>% 
  dplyr::select(SUM.x,SUM.y,SUM.x.x,MIN.y,
                COUNT, X, Y) # SUM.x = soil, SUM.y = parks, SUM.x.x = impervious surface, MIN.y = elevation, COUNT = water




