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
install.packages('tidyr')
install.packages('dplyr')
install.packages(ggplot2)



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
library(tidyr)
library(dplyr)
library(ggplot2)


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
cal_fish <- st_read("C:/Users/ferna/OneDrive/Documents/ArcGIS/Projects/CPLN 6750/HW3/Data_Calgary/Fishnet_Clipped/fishnet_Clip2.shp", promote_to_multi = FALSE)

# Import Calgary data features
library(readr)
CA_Soil <- st_read("C:/Users/ferna/OneDrive/Documents/ArcGIS/Projects/CPLN 6750/HW3/Data_Calgary/Final_Tables/CA_Soil.xlsx")
CA_Parks <- st_read("C:/Users/ferna/OneDrive/Documents/ArcGIS/Projects/CPLN 6750/HW3/Data_Calgary/Final_Tables/CA_Parks.xlsx")
CA_Impervious <- st_read("C:/Users/ferna/OneDrive/Documents/ArcGIS/Projects/CPLN 6750/HW3/Data_Calgary/Final_Tables/CA_Impervious.xlsx")
CA_Elevation <- st_read("C:/Users/ferna/OneDrive/Documents/ArcGIS/Projects/CPLN 6750/HW3/Data_Calgary/Final_Tables/CA_Elevation.xlsx")
CA_Waterbodies <- st_read("C:/Users/ferna/OneDrive/Documents/ArcGIS/Projects/CPLN 6750/HW3/Data_Calgary/Final_Tables/CA_Waterbodies.xlsx")

# I am renaming OBJECTID columns to specify variables so we know what "counts" or "sums" coincide with which variables
cal_fish <- cal_fish %>%
  rename(cell = OBJECTID_1)

CA_Soil <- CA_Soil %>%
  rename(
    cell = OBJECTID_1,
    soil_sum = SUM,
    ) %>%
  dplyr::select(-OBJECTID, -COUNT, -AREA )


CA_Parks <- CA_Parks %>%
  rename(
    cell = OBJECTID_1,
    park_sum = SUM,
    park_mean = MEAN,
    park_count = COUNT  
    ) %>%
  # Remove specific columns
  dplyr::select(-OBJECTID, -AREA, -MIN, -MAX, -RANGE, -STD, -VARIETY, -MAJORITY, -MAJORITY_COUNT, -MAJORITY_PERCENT, -MINORITY,-MINORITY_COUNT, -MINORITY_PERCENT, -MEDIAN, -PCT90 )
  

CA_Impervious <- CA_Impervious %>%
  rename(
    cell = OBJECTID_1,
    impervious_mean = MEAN,
    Impervious_majority = Impervious
    ) %>%
  dplyr::select(-OBJECTID, -COUNT, -AREA, -MAX, -MIN, -RANGE, -SUM )


CA_Waterbodies <- CA_Waterbodies %>%
  rename(
    cell = OBJECTID_1,
    Waterbodies_mean = MEAN,
  ) %>%
  dplyr::select(-OBJECTID, -AREA, -COUNT, - SUM, -MIN, -MAX, -RANGE, -STD, -VARIETY, -MAJORITY, -MAJORITY_COUNT, -MAJORITY_PERCENT, -MINORITY,-MINORITY_COUNT, -MINORITY_PERCENT, -MEDIAN, -PCT90 )

CA_Elevation <- CA_Elevation %>%
  rename(
    cell = OBJECTID_1,
    elevation_max = MAX,
    elevation_min = MIN,
    elevation_mean = MEAN,
    ) %>%
  dplyr::select(-OBJECTID, -COUNT, -AREA, -RANGE, -STD, -SUM )

# joining CSVs to Calgary fishnet by "cell" number
joined_fishcal <- cal_fish %>%
  left_join(CA_Soil, by = "cell") %>%
  left_join(CA_Parks, by = "cell") %>%
  left_join(CA_Impervious, by = "cell") %>%
  left_join(CA_Elevation, by = "cell") %>%
  left_join(CA_Waterbodies, by = "cell") 

# plotting joined_fishcal, visualizing
ggplot() +
  geom_sf(data = joined_fishcal, aes(fill = Low_permeability)) +  # plot low permeability soil # logic: cells near low permeability soils will be more likely to flood
  scale_fill_gradient(low = "grey", high = "blue") +  # color
  theme_minimal()

ggplot() +
  geom_sf(data = joined_fishcal, aes(fill = Parks_Majority)) +  # plot parks # logic: cells near parks will be less likely to flood
  scale_fill_gradient(low = "grey", high = "blue") +  # color
  theme_minimal()

ggplot() +
  geom_sf(data = joined_fishcal, aes(fill = Impervious_majority)) + # plot impervious # logic: cells near impervious surfaces will be more likely to flood 
  scale_fill_gradient(low = "grey", high = "blue") +  # color
  theme_minimal()

ggplot() +
  geom_sf(data = joined_fishcal, aes(fill = elevation_min)) + # plot elevation # logic: cells near the lowest points will be more likely to flood
  scale_fill_gradient(low = "grey", high = "blue") +  # color
  theme_minimal()

ggplot() +
  geom_sf(data = joined_fishcal, aes(fill = Water_majority)) + # plot water bodies # logic: cells near water bodies will be more likely to flood
  scale_fill_gradient(low = "grey", high = "blue") +  # color
  theme_minimal()


## 2. CONDUCTING SPATIAL LAG  
# creating a spatial weights matrix using queen contiguity (queen =  defines neighbors as spatial units sharing a common edge)
w <- poly2nb(joined_fishcal, queen = TRUE)
w <- nb2listw(w)

# compute spatial lag with each variable
joined_fishcal$lag_soil <- lag.listw(w, joined_fishcal$Low_permeability) # low permeability soil
joined_fishcal$lag_parks <- lag.listw(w, joined_fishcal$Parks_Majority) # parks
joined_fishcal$lag_imp <- lag.listw(w, joined_fishcal$Impervious_majority) # impervious surfaces
joined_fishcal$lag_elev <- lag.listw(w, joined_fishcal$elevation_min) # elevation
joined_fishcal$lag_water <- lag.listw(w, joined_fishcal$Water_majority) # water

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
  scale_fill_gradient(name = "Parks", low = "grey", high = "green") +
  labs(fill = "Parks") +
  theme_minimal() # areas in green are more least likely to flood # logic: where there is a "low" count of parks, cells are more likely to flood
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
library("dplyr")
# importing inundation raster
# Specify the URL of the TIFF file
tiff_url <- "https://github.com/fdzjavier/CPLN675/raw/main/Data/rasters/Inundation_re.tif"

# Read the TIFF file into R as a raster object
inundation <- raster(tiff_url)
inundation
plot(inundation)

# upload inundation zonal statistics
inu_zonal <- st_read("C:/Users/ferna/OneDrive/Documents/ArcGIS/Projects/CPLN 6750/HW3/Data_Calgary/Final_Tables/ZonalSt_Inundation_v2.xlsx") %>%
  rename(
    cell = OBJECTID_1
  ) %>%
  dplyr::select(-OBJECTID, -AREA, -COUNT, - SUM)


# attach to fishnet
cal_inu_fish <- joined_fishcal %>%
  left_join(inu_zonal, by = "cell") 
view(cal_inu_fish)

ggplot() + 
  geom_sf(data = cal_inu_fish, aes(fill=(Flooded)), color = 'blue')

inundationPlotVariables <- 
  cal_inu_fish %>%
  as.data.frame() %>%
  dplyr::select(Flooded,Low_permeability, Parks_Majority, Impervious_majority, elevation_min, Water_majority) %>%
  gather(variable, value, -Flooded)

# Note the following variables: 
# SUM.x = low permeability soil
# SUM.y = parks 
# SUM.x.x = impervious surfaces
# MIN.y = elevation
# COUNT.x.x.x = water
# SUM.y.y.y = inundation

# Plotting Variables

test_summary <- inundationPlotVariables %>%
  group_by(Flooded, variable) %>%
  summarize(mean = mean(value))

ggplot(test_summary, aes(x = as.factor(Flooded), y = mean, fill = as.factor(Flooded))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(values = c("darkblue", "darkgreen"),
                    labels = c("Not Flooded", "Flooded"),
                    name = "") +
  labs(x = "Flooded", y = "Value")

ca_inundation <- cal_inu_fish %>%
  dplyr::select(cell, Flooded, Low_permeability, Parks_Majority, Impervious_majority, elevation_min, Water_majority)

# Model Building 
set.seed(3456)
trainIndex <- createDataPartition(ca_inundation$Flooded, p = .70,
                                  list = FALSE,
                                  times = 1)

floodTrain <- ca_inundation[ trainIndex,]
floodTest  <- ca_inundation[-trainIndex,]

floodModel <- glm(Flooded ~ ., 
                     family="binomial"(link="logit"), data = floodTrain %>%
                       as.data.frame() %>%
                       dplyr::select(-geometry, -cell))
summary(floodModel)
  

# Model validation for the testing dataset
classProbs <- predict(floodModel, floodTest, type="response")

hist(classProbs)
  

testProbs <- data.frame(obs = as.numeric(floodTest$Flooded),
                        pred = classProbs)

#Probability curve for the testing dataset
ggplot(testProbs, aes(x = pred, fill=as.factor(obs))) + 
  geom_density() +
  facet_grid(obs ~ .) + 
  xlab("Probability") +
  ylab("Frequency")+
  geom_vline(xintercept = .25) +
  scale_fill_manual(values = c("dark blue", "dark green"),
                    labels = c("Not Flooded","Flooded"),
                    name = "")+
  plotTheme

# Confusion matrix
testProbs$predClass  = ifelse(testProbs$pred > .25 ,1,0)

caret::confusionMatrix(reference = as.factor(testProbs$obs), 
                       data = as.factor(testProbs$predClass), 
                       positive = "1")

# ROC Curves for the testing dataset
ggplot(testProbs, aes(d = obs, m = pred)) + 
  geom_roc(n.cuts = 50, labels = FALSE) + 
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey')

auc(testProbs$obs, testProbs$pred)

#Cross Validation
ctrl <- trainControl(method = "cv", 
                     number = 100, 
                     p = 0.7, 
                     savePredictions = TRUE)

cvFit <- train(as.factor(Flooded) ~ .,  data = ca_inundation %>% 
                 as.data.frame() %>%
                 dplyr::select(-geometry, -cell), 
               method="glm", family="binomial",
               trControl = ctrl)

cvFit

ggplot(as.data.frame(cvFit$resample), aes(Accuracy)) + 
  geom_histogram() +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Accuracy",
       y="Count")+
  plotTheme

#Map Predictions
allPredictions <- 
  predict(cvFit, ca_inundation, type="prob")[,2]

ca_inundation <- 
  cbind(ca_inundation,allPredictions) %>%
  mutate(allPredictions = round(allPredictions * 100))

ggplot() + 
  geom_sf(data=ca_inundation, aes(fill=factor(ntile(allPredictions,5))), 
          colour=NA) +
  scale_fill_manual(values = c("#edf8fb","#b3cde3","#8c96c6","#8856a7","#810f7c"),
                    labels=as.character(quantile(ca_inundation$allPredictions,
                                                 c(0.1,.2,.4,.6,.8),
                                                 na.rm=T)),
                    name="Predicted\nProbabilities(%)\n(Quintile\nBreaks)") +
  mapTheme +
  labs(title="")

ggplot() + 
  geom_sf(data=ca_inundation, aes(fill=factor(ntile(allPredictions,5))), colour=NA) +
  scale_fill_manual(values = c("#edf8fb","#b3cde3","#8c96c6","#8856a7","#810f7c"),
                    labels=as.character(quantile(ca_inundation$allPredictions,
                                                 c(0.1,.2,.4,.6,.8),
                                                 na.rm=T)),
                    name="Predicted\nProbabilities(%)\n(Quintile\nBreaks)") +
  geom_sf(data=ca_inundation  %>% 
            filter(Flooded == 1), 
          fill="blue",colour=NA) +
  
  mapTheme +
  labs(title="Observed and Predicted Flooded Areas",
       subtitle="Observed flooded land in blue")

# Changes threshold to 25 
ca_inundation %>%
  mutate(confResult=case_when(allPredictions < 25 & Flooded==0 ~ "True_Negative",
                              allPredictions >= 25 & Flooded==1 ~ "True_Positive",
                              allPredictions < 25 & Flooded==1 ~ "False_Negative",
                              allPredictions >= 25 & Flooded==0 ~ "False_Positive")) %>%
  ggplot()+
  geom_sf(aes(fill = confResult), color = "transparent")+
  scale_fill_manual(values = c("Red","Orange","Light Blue","Light Green"),
                    name="Outcomes")+
  labs(title="Confusion Metrics") +
  mapTheme

#Map Predictions for DC
#upload DC variables
dc_inundation <- read_csv("C:/Users/mktran/Desktop/dcdata_new/cleaned_DCTable.csv")

dc_allPredictions <- 
  predict(cvFit, dc_inundation, type="prob")[,2]

dc_inundation <- 
  cbind(dc_inundation,dc_allPredictions) %>%
  mutate(dc_allPredictions = round(dc_allPredictions * 100))

ggplot() + 
  geom_sf(data=dc_inundation, aes(fill=factor(ntile(dc_allPredictions,5))), 
          colour=NA) +
  scale_fill_manual(values = c("#edf8fb","#b3cde3","#8c96c6","#8856a7","#810f7c"),
                    labels=as.character(quantile(dc_inundation$dc_allPredictions,
                                                 c(0.1,.2,.4,.6,.8),
                                                 na.rm=T)),
                    name="Predicted\nProbabilities(%)\n(Quintile\nBreaks)") +
  mapTheme +
  labs(title="")

ggplot() + 
  geom_sf(data=dc_inundation, aes(fill=factor(ntile(dc_allPredictions,5))), colour=NA) +
  scale_fill_manual(values = c("#edf8fb","#b3cde3","#8c96c6","#8856a7","#810f7c"),
                    labels=as.character(quantile(dc_inundation$dc_allPredictions,
                                                 c(0.1,.2,.4,.6,.8),
                                                 na.rm=T)),
                    name="Predicted\nProbabilities(%)\n(Quintile\nBreaks)") +
  geom_sf(data=dc_inundation  %>% # NOT ACTUALLY SURE ABOUT ADDING THIS, is this plotting the observed calgary floods? 
            filter(Flooded == 1), 
          fill="blue",colour=NA) +
  
  mapTheme +
  labs(title="Predicted Flooded Areas")

# Changes threshold to 25 
dc_inundation %>%
  mutate(confResult=case_when(allPredictions < 25 & Flooded==0 ~ "True_Negative",
                              allPredictions >= 25 & Flooded==1 ~ "True_Positive",
                              allPredictions < 25 & Flooded==1 ~ "False_Negative",
                              allPredictions >= 25 & Flooded==0 ~ "False_Positive")) %>%
  ggplot()+
  geom_sf(aes(fill = confResult), color = "transparent")+
  scale_fill_manual(values = c("Red","Orange","Light Blue","Light Green"),
                    name="Outcomes")+
  labs(title="Confusion Metrics") +
  mapTheme