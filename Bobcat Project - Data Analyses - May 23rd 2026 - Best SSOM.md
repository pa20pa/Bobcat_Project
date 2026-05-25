# New bobcat data analyses 
# Date: May 23rd, 2026

# Housekeeping

# Setting your working directory 
getwd() # This line gives current working directory 
setwd("C:/Users/Owner/OneDrive/From Mom's PC/Bobcat Project/Updated Bobcat Project")

# Loading R packags you will need
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(patchwork)
library(tibble)
library(unmarked)
library(lubridate)
library(AICcmodavg)

# Installing other packages you will need 
install.packages("gmmap")
install.packages("ggspatial")
install.packages("leaflet")
install.packages("leaflet.extras")
install.packages("tmap")

# Opening bobcat data
bobcat.data <- read.csv("improvedbobcatdatastandardized.csv", stringsAsFactors = T) %>%
  filter(CameraTrap != "")

# Formula for choosing the first non-NA value for CameraTrap
safe_first <- function(x) {
  x <- x[!is.na(x)]
  if(length(x) == 0) return(NA) 
  x[1]
}

# Cleaning CameraTrap IDs
bobcat.data$CameraTrap <- bobcat.data$CameraTrap %>%
  trimws() %>%                 # remove leading/trailing spaces
  toupper() %>%                # standardize case (optional but recommended)
  gsub("[^A-Z0-9_]", "", .)    # remove weird characters (keep letters, numbers, underscore)

# Filter out duplicates and keep only a couple variables of interest
# Changing variables of interest from ones in Alex Crow's document since a few things changed after your mutlicollinearity test
# Also getting rid of line 56 in Alex Crow's document now that you don't have traps with completely missing data
camera_covariates <- bobcat.data %>%
  group_by(CameraTrap) %>%
  summarise(
    Latitude = safe_first(Latitude),
    Longitude = safe_first(Longitude),
    DistancetoUrbanArea = safe_first(DistancetoUrbanArea),
    Slope = safe_first(Slope),
    Aspect = safe_first(Aspect),
    DistancetoWater = safe_first(DistancetoWater),
    DistancetoRoad = safe_first(DistancetoRoad),
    Elevation = safe_first(Elevation),
    WoodyMaterial = safe_first(WoodyMaterial),
    StandAge = safe_first(StandAge),
    StandDensityIndex = safe_first(StandDensityIndex),
    TopographicRoughnessIndex = safe_first(TopographicRoughnessIndex),
    CanopyHeight = safe_first(CanopyHeight),
    AreaBroadleafDeciduousForest = safe_first(AreaBroadleafDeciduousForest),
    AreaMixedForest = safe_first(AreaMixedForest),
    AreaShrubland = safe_first(AreaShrubland),
    AreaGrassland = safe_first(AreaGrassland),
    AreaWetland = safe_first(AreaWetland),
    AreaCropland = safe_first(AreaCropland),
    AreaBarrenLand = safe_first(AreaBarrenLand),
    AreaUrban = safe_first(AreaUrban),
    AreaWater = safe_first(AreaWater),
    Area1to25PercentCanopyCover = safe_first(Area1to25PercentCanopyCover),
    Area50to75PercentCanopyCover = safe_first(Area50to75PercentCanopyCover),
    .groups = "drop"
  )
  
# Creating camera summary
# Getting rid of line 78 code in Alex Crow's document since you don't have camera-traps with entirely missing data now
camera_summary <- bobcat.data %>%
  group_by(CameraTrap) %>%
  summarise(
    active_days = n(),
    total_captures = sum(NumberofBobcatCaptures, na.rm = TRUE),
    capture_rate = total_captures / active_days,
    ever_detected = as.integer(total_captures > 0),
    prop_detection_days = mean(BobcatCapture == "Yes", na.rm = TRUE),
    .groups = "drop"
  )

# Make camera data
camera_data <- left_join(camera_summary, camera_covariates, by = "CameraTrap")

# Creating presence map 
ggplot(camera_data, aes(x = Longitude, y = Latitude, color = factor(ever_detected))) +
  geom_point(size = 3) +
  labs(color = "Bobcat detected") +
  theme_classic()

# Creating intensity map
ggplot(camera_data, aes(x = Longitude, y = Latitude)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.6) +
  geom_point(aes(color = factor(ever_detected)), size = 2) +  coord_cartesian(
    xlim = quantile(camera_data$Longitude, c(0.05, 0.95)),
    ylim = quantile(camera_data$Latitude, c(0.05, 0.95))
  ) +
  scale_fill_viridis_c() +
  theme_classic(base_size = 15)

# Creating capture rate figure
ggplot(camera_data, aes(x = capture_rate)) +
  geom_histogram(bins = 30) +
  theme_classic(base_size = 15) +
  xlab("Capture rate (bobcats/active day)")

# Binned number of captures
camera_data <- camera_data %>%
  mutate(
    capture_class = case_when(
      total_captures == 0 ~ "0",
      total_captures == 1 ~ "1",
      total_captures <= 3 ~ "2-3",
      total_captures <= 6 ~ "4-6",
      TRUE ~ "7+"
    )
  )
  
ggplot(camera_data, aes(x = capture_class)) +
  geom_bar() +
  xlab("Number of Captures") +
  theme_classic(base_size = 15)

# Creating figures based on variable values at sites 

# Topographic features

# Slope
S1<- camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = Slope,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Slope (deg)")+
  ylim( c(0, 0.1))

S1

# Elevation
S2<-camera_data %>%
filter(capture_rate!=0) %>% # 152 and 153 tell code to remove 0s - can apply to other figures 
ggplot(aes(x = Elevation,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Elevation (m)") +
  ylim( c(0, 0.1))

S2

# Topographic Roughness
S3<- camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = TopographicRoughnessIndex,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Topographic Roughness Index") +
  ylim( c(0, 0.1))

S3

# Aspect 
S4<- camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = Aspect,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Aspect") +
  ylim( c(0, 0.1))

S4

# Distance from Selected Features

# Distance to Urban Area
D1<-camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = DistancetoUrbanArea,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15) +
  ylab("Capture rate (BC/d)") +
  xlab ("Distance to Urban Area (m)")+
  ylim( c(0, 0.1))

D1

# Distance to Water
D2<-camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = DistancetoWater,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Distance to Water (m)")+
  ylim( c(0, 0.1))

D2

# Distance to Road
D3<-camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = DistancetoRoad, y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Distance to Road (m)")+
  ylim( c(0, 0.1))

D3

# Forest-Related Variables

# Woody Material
F1<- camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = WoodyMaterial,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Woody Material (Ton / Acre)")+
  ylim( c(0, 0.1))

F1

# Stand Density 
F2<-camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = StandDensityIndex,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Stand Density Index")
F2
# Not sure what happened with this graph :(

# Stand Age 
F3<-camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = StandAge,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Stand Age")+
  ylim( c(0, 0.1))

F3

# Canopy Height
F4<- camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = CanopyHeight,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Canopy Height")+
  ylim( c(0, 0.1))

F4

# Area1to25PercentCanopyCover
F5<-camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = Area1to25PercentCanopyCover,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Amount of 1 to 25 Percent Canopy Cover (m^2)")+
  ylim( c(0, 0.1))

F5

# Area50to75PercentCanopyCover
F6<-camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = Area50to75PercentCanopyCover,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Amount of 50 to 75 Percent Canopy Cover (m^2)")+
  ylim( c(0, 0.1))

F6

# Land Cover 

# Area Broadleaf Deciduous Forest
F7<-camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = AreaBroadleafDeciduousForest,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Area Broadleaf Deciduous (m^2)")+
  ylim( c(0, 0.1))

F7

# Area Mixed Forest
F8<-camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = AreaMixedForest,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Area Mixed Forest (m^2)")+
  ylim( c(0, 0.1))

F8

# Area Shrubland
F9<-camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = AreaShrubland,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Area Shrubland (m^2)")+
  ylim( c(0, 0.1))

F9

# Area Grassland
F10<-camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = AreaGrassland,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+a
  xlab ("Area Grassland (m^2)")+
  ylim( c(0, 0.1))

F10

# Area Wetland
F11<-camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = AreaWetland,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Area Wetland (m^2)")+
  ylim( c(0, 0.1))

F11

# Area Cropland
F12<-camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = AreaCropland,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Area Cropland (m^2)")+
  ylim( c(0, 0.1))

F12

# Area Barren Land
F13<-camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = AreaBarrenLand,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Area Barren Land (m^2)")+
  ylim( c(0, 0.1))

F13

# Area Urban
F14<-camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = AreaUrban,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Area Urban (m^2)")+
  ylim( c(0, 0.1))

F14

# Area Water
F15<-camera_data %>%
filter(capture_rate!=0) %>%
ggplot(aes(x = AreaWater,
                        y = capture_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)+
  ylab("Capture rate (BC/d)") +
  xlab ("Area Water (m^2)")+
  ylim( c(0, 0.1))

F15

# Surveying Effort

# Effort
ggplot(camera_data, aes(x = active_days, y = total_captures)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  theme_classic(base_size = 15)
# Positive relationship 
# Makes sense that greater surveying effort = more bobcat captures 

# Wait to make maps that start at line 429 in Alex Crow's code 

# Single-Species Occupancy Modelling 
start.date <- as.Date("2018-11-22") # Sets start date

# Open data
Model.bobcat.data <- read.csv("improvedbobcatdatastandardized.csv", stringsAsFactors = T)
  
# Clean CameraTrap IDs
Model.bobcat.data$CameraTrap <- Model.bobcat.data$CameraTrap %>%
  trimws() %>%                 # remove leading/trailing spaces
  toupper() %>%                # standardize case (optional but recommended)
  gsub("[^A-Z0-9_]", "", .)    # remove weird characters (keep letters, numbers, underscore)

# Make SiteYear - TrapID_X (1-5)
Model.bobcat.data$SiteYear <- paste(Model.bobcat.data$CameraTrap, Model.bobcat.data$StudyYear, sep = "_")

# Convert detection data into binary (0-1)
bobcat.data <- Model.bobcat.data %>%
  mutate(
    Detect = ifelse(NumberofBobcatCaptures > 0, 1, 0)
    , Date = (start.date + (DeploymentDay - 1) + (StudyYear - 1) * 365)
    , Winter = ifelse(month(Date) >= 11, year(Date)-2015, year(Date) - 2016)
    ) %>%
  group_by(CameraTrap, Winter) %>%
  arrange(DeploymentDay) %>%
  mutate(
    , Week = ceiling(row_number() / 7)
  ) %>%
  ungroup()
# Removed line 753 in Alex Crow's file since there are no camera-traps that are missing all data now

# Remove any duplicates 
bobcat.data <- bobcat.data %>%
  group_by(CameraTrap, Winter, DeploymentDay,Week) %>%
  summarise(
    Detect = max(Detect, na.rm = TRUE),
    DistancetoUrbanArea = safe_first(DistancetoUrbanArea),
    Slope = safe_first(Slope),
    Aspect = safe_first(Aspect),
    DistancetoWater = safe_first(DistancetoWater),
    DistancetoRoad = safe_first(DistancetoRoad),
    Elevation = safe_first(Elevation),
    WoodyMaterial = safe_first(WoodyMaterial), ##### <- Site Cov
    StandAge = safe_first(StandAge),
    StandDensityIndex = safe_first(StandDensityIndex),
    TopographicRoughnessIndex = safe_first(TopographicRoughnessIndex),
    CanopyHeight = safe_first(CanopyHeight),
    AreaBroadleafDeciduousForest = safe_first(AreaBroadleafDeciduousForest),
    AreaMixedForest = safe_first(AreaMixedForest),
    AreaShrubland = safe_first(AreaShrubland),
    AreaGrassland = safe_first(AreaGrassland),
    AreaWetland = safe_first(AreaWetland),
    AreaCropland = safe_first(AreaCropland),
    AreaBarrenLand = safe_first(AreaBarrenLand),
    AreaUrban = safe_first(AreaUrban),
    AreaWater = safe_first(AreaWater),
    Area1to25PercentCanopyCover = safe_first(Area1to25PercentCanopyCover),
    Area50to75PercentCanopyCover = safe_first(Area50to75PercentCanopyCover),
    .groups = "drop"
  )

# Model site data 
site.data <- bobcat.data %>%
  group_by(CameraTrap) %>%
  summarise(
    DistancetoUrbanArea = safe_first(DistancetoUrbanArea),
    Slope = safe_first(Slope),
    Aspect = safe_first(Aspect),
    DistancetoWater = safe_first(DistancetoWater),
    DistancetoRoad = safe_first(DistancetoRoad),
    Elevation = safe_first(Elevation),
    WoodyMaterial = safe_first(WoodyMaterial), ##### <- Site Cov
    StandAge = safe_first(StandAge),
    StandDensityIndex = safe_first(StandDensityIndex),
    TopographicRoughnessIndex = safe_first(TopographicRoughnessIndex),
    CanopyHeight = safe_first(CanopyHeight),
    AreaBroadleafDeciduousForest = safe_first(AreaBroadleafDeciduousForest),
    AreaMixedForest = safe_first(AreaMixedForest),
    AreaShrubland = safe_first(AreaShrubland),
    AreaGrassland = safe_first(AreaGrassland),
    AreaWetland = safe_first(AreaWetland),
    AreaCropland = safe_first(AreaCropland),
    AreaBarrenLand = safe_first(AreaBarrenLand),
    AreaUrban = safe_first(AreaUrban),
    AreaWater = safe_first(AreaWater),
    Area1to25PercentCanopyCover = safe_first(Area1to25PercentCanopyCover),
    Area50to75PercentCanopyCover = safe_first(Area50to75PercentCanopyCover),
    .groups = "drop"
  )

site.data <- left_join(camera_summary, site.data, by = "CameraTrap")

# Make the row names CameraTrap
site.data <- as.data.frame(site.data)
rownames(site.data) <- site.data$CameraTrap
site.data$CameraTrap<-as.factor(site.data$CameraTrap)

bobcat.data.binned <- bobcat.data %>%
  arrange(CameraTrap, Winter, Week, DeploymentDay) %>%
  group_by(CameraTrap) %>%
  mutate(Occasion = ceiling(row_number() / 7)) %>% 
  ungroup()
  
# Binning bobcat data
y<-bobcat.data.binned %>%
  select(CameraTrap, Occasion, Detect) %>%
  pivot_wider(
    names_from = Occasion,
    values_from = Detect,
    names_prefix = "V",
    values_fill = NA   #NA = not sampled
    , values_fn = max
  ) %>%
  arrange(CameraTrap)%>%
  column_to_rownames("CameraTrap") %>%
  as.matrix()

# Modelling year
Year<-bobcat.data.binned %>%
  select(CameraTrap, Occasion, Winter) %>%
  pivot_wider(
    names_from = Occasion,
    values_from = Winter,
    names_prefix = "V",
    values_fill = NA   #NA = not sampled
    , values_fn = max
  ) %>%
  arrange(CameraTrap)%>%
  column_to_rownames("CameraTrap") %>%
  as.matrix()

# Modelling week
Week<-bobcat.data.binned %>%
  select(CameraTrap, Occasion, Week) %>%
  pivot_wider(
    names_from = Occasion,
    values_from = Week,
    names_prefix = "V",
    values_fill = NA   #NA = not sampled
    , values_fn = max
  ) %>%
  arrange(CameraTrap)%>%
  column_to_rownames("CameraTrap") %>%
  as.matrix()

# Modelling effort
Effort<-bobcat.data.binned %>%
  group_by(CameraTrap, Occasion) %>%
  summarise(Effort = n(), .groups = "drop") %>%
  select(CameraTrap, Occasion, Effort) %>%
  pivot_wider(
    names_from = Occasion,
    values_from = Effort,
    names_prefix = "V",
    values_fill = 0   #NA = not sampled
    , values_fn = max
  ) %>%
  arrange(CameraTrap)%>%
  column_to_rownames("CameraTrap") %>%
  as.matrix()

# Making a simple model
simple <- unmarkedFrameOccu( # y is a matrix with observed detection history 
                                          # (0's and 1's, one row per site, one column per survey)
                                      y = y
)

summary(simple)

# Building a basic single-season occupancy model with intercepts only (one estimate for detection, one for occupancy)
occu.m1 <- occu(formula = ~1 # detection formula first
                          ~1, # occupancy formula second, 
                  data = simple)
summary(occu.m1) 

# Building a more standard model
bobcat.model <- unmarkedFrameOccu( # y is a matrix with observed detection history 
                                          # (0's and 1's, one row per site, one column per survey)
                                      y = y,
                                      # obsCovs = observation covariates in a list, 
                                      # each variable has site rows x survey columns
                                      obsCovs = list(Week = Week,
                                                     Year = Year,
                                                     Effort = Effort),
                                      # siteCovs = dataframe with site rows x column variables
                                      siteCovs = camera_data) 
summary(bobcat.model)

# Creating a full model with all variables 
Model.Full <- occu(formula = ~Week + Year + Effort # detection formula first
                          ~ 
                  Elevation
                + Slope # Low P
                + Aspect
                + DistancetoUrbanArea
                + DistancetoWater
                + DistancetoRoad
                + WoodyMaterial
                + StandAge
                + StandDensityIndex
                + TopographicRoughnessIndex # Low P
                + CanopyHeight
                + AreaBroadleafDeciduousForest
                + AreaMixedForest
                + AreaShrubland
                + AreaGrassland
                + AreaWetland
                + AreaCropland
                + AreaBarrenLand
                + AreaUrban
                + AreaWater
                + Area1to25PercentCanopyCover
                + Area50to75PercentCanopyCover, # occupancy formula second,
                , data = bobcat.model)
summary(Model.Full)

# AIC value for full model is 943.4432 
# P value low for area cropland
# Not sure how great this model is since there were a lot of NA values?

Model.Topography <- occu(formula = ~Week + Year + Effort # detection formula first
                    ~ 
                  Elevation
                + Slope
                + Aspect
                + TopographicRoughnessIndex,
                , data = bobcat.model)
summary(Model.Topography)

# AIC value for topography variables model is 1017.511 
# P value lowest < 0.05 for Slope and Topographic Roughness Index
# Topographic Roughness Index had lowest P-value by far

Model.Forest.Variables <- occu(formula = ~Week + Year + Effort # detection formula first
                         ~ 
                         WoodyMaterial
                       + StandDensityIndex
                       + StandAge
                       + CanopyHeight,
                       , data = bobcat.model)
summary(Model.Forest.Variables)

# AIC vaule for forest variables model is 984.3686
# AIC value lower for this model than topography model
# But P value > 0.05 for all features in this model
# Lowest P value is Stand Age 

Model.Land.Cover <- occu(formula = ~Week + Year + Effort # detection formula first
                         ~ 
                         AreaBroadleafDeciduousForest
                         + AreaMixedForest
                         + AreaShrubland
                         + AreaGrassland
                         + AreaWetland
                         + AreaCropland
                         + AreaBarrenLand
                         + AreaUrban
                         + AreaWater,
                         , data = bobcat.model)
summary(Model.Land.Cover)

# AIC value for land cover variables model is 1027.04
# P value is > 0.05 for all features in this model
# Lowest P value is Area Water
# Second lowest P value is Area Broadleaf Deciduous Forest
                         
Model.Distances <- occu(formula = ~Week + Year + Effort # detection formula first
                         ~ 
                         DistancetoWater
                         + DistancetoUrbanArea
                         + DistancetoRoad,
                         , data = bobcat.model)
summary(Model.Distances)

# AIC value for distance variables model is 1029.311
# P value is > 0.05 for all features in this model
# Lowest P value is Distance to Urban Area
# Second lowest P value is Distance to Road 
                         
Model.Canopy.Cover <- occu(formula = ~Week + Year + Effort # detection formula first
                         ~ 
                         Area1to25PercentCanopyCover
                         + Area50to75PercentCanopyCover,
                         , data = bobcat.model)

summary(Model.Canopy.Cover)

# AIC value for canopy cover variables is 1026.528 
# P value is > 0.05 for both features in this model
# Lowest P value is Area 1 to 25 Percent Canopy Cover  

# Results of the Top 15 Single-Species Occupancy Models you did with four variables

Mixed.Model.One <- occu(formula = ~Week + Year + Effort
                         ~ 
                         TopographicRoughnessIndex
                         + WoodyMaterial
                         + Slope
                         + AreaWater,
                         , data = bobcat.model)

summary(Mixed.Model.One)

# Above model is the best single species occupancy model of all potential combinations of four variables
# Interesting that amount of water results in lower AIC than area of forest types 
# Would this be a good candidate model?
# Topographic Roughness and Slope are statistically significant (p-value < 0.05)

# Model with only topographic roughness 

Model.Topographic.Roughness <- occu(formula = ~Week + Year + Effort
                               ~ 
                               TopographicRoughnessIndex,
                               , data = bobcat.model)
summary(Model.Topographic.Roughness)

# Model with only woody material 

Model.Woody.Material <- occu(formula = ~Week + Year + Effort
                               ~ 
                               WoodyMaterial,
                               , data = bobcat.model)
summary(Model.Woody.Material)

# Model with only slope

Model.Slope <- occu(formula = ~Week + Year + Effort
                               ~ 
                               Slope,
                               , data = bobcat.model)
summary(Model.Slope)

# Model with only canopy height

Model.Canopy.Height <- occu(formula = ~Week + Year + Effort
                               ~ 
                               CanopyHeight,
                               , data = bobcat.model)
summary(Model.Canopy.Height)

# Model with only distance to urban area 

Model.Distance.Urban <- occu(formula = ~Week + Year + Effort
                               ~ 
                               DistancetoUrbanArea,
                               , data = bobcat.model)
summary(Model.Distance.Urban)

# Model with only distance to water

Model.Distance.Water <- occu(formula = ~Week + Year + Effort
                               ~ 
                               DistancetoWater,
                               , data = bobcat.model)
summary(Model.Distance.Water)

# Model with only wetland

Model.Wetland <- occu(formula = ~Week + Year + Effort
                               ~ 
                               AreaWetland,
                               , data = bobcat.model)
summary(Model.Wetland)

# Model with only broadleaf deciduous forest

Model.Broadleaf.Deciduous <- occu(formula = ~Week + Year + Effort
                               ~ 
                               AreaBroadleafDeciduousForest,
                               , data = bobcat.model)
summary(Model.Broadleaf.Deciduous)


# Model with only stand density

Model.Stand.Density <- occu(formula = ~Week + Year + Effort
                               ~ 
                               StandDensityIndex,
                               , data = bobcat.model)
summary(Model.Stand.Density)

# Model with only distance to road

Model.Distance.Road <- occu(formula = ~Week + Year + Effort
                               ~ 
                               DistancetoRoad,
                               , data = bobcat.model)
summary(Model.Distance.Road)

# Model with only elevation

Model.Elevation <- occu(formula = ~Week + Year + Effort
                               ~ 
                               Elevation,
                               , data = bobcat.model)
summary(Model.Elevation)

# Model with only aspect

Model.Aspect <- occu(formula = ~Week + Year + Effort
                               ~ 
                               Aspect,
                               , data = bobcat.model)
summary(Model.Aspect)

# Model with only 50 to 75 percent canopy cover

Model.50to75Canopy <- occu(formula = ~Week + Year + Effort
                               ~ 
                               Area50to75PercentCanopyCover,
                               , data = bobcat.model)
summary(Model.50to75Canopy)

# Model with only topographic roughness and slope

Model.TwoVariables.One <- occu(formula = ~Week + Year + Effort
                               ~ 
                               TopographicRoughnessIndex
                               + Slope,
                               , data = bobcat.model)
summary(Model.TwoVariables.One)

# Model with only slope and woody material

Model.TwoVariables.Two <- occu(formula = ~Week + Year + Effort
                               ~ 
                               WoodyMaterial
                               + Slope,
                               , data = bobcat.model)
summary(Model.TwoVariables.Two)

# Model with only topographic roughness and woody material

Model.TwoVariables.Three <- occu(formula = ~Week + Year + Effort
                               ~ 
                               WoodyMaterial
                               + TopographicRoughnessIndex,
                               , data = bobcat.model)
summary(Model.TwoVariables.Three)


Mixed.Model.Two <- occu(formula = ~Week + Year + Effort
                         ~ 
                         TopographicRoughnessIndex
                         + WoodyMaterial
                         + Slope
                         + CanopyHeight,
                         , data = bobcat.model)

summary(Mixed.Model.Two)

# Seems like a vaid model due to the two forest-related variables
# Topographic Roughness and Slope are statistically significant (p-value < 0.05)

Mixed.Model.Three <- occu(formula = ~Week + Year + Effort
                         ~ 
                         TopographicRoughnessIndex
                         + WoodyMaterial
                         + Slope
                         + AreaMixedForest,
                         , data = bobcat.model)

summary(Mixed.Model.Three)

# Interesting finding - I think this would be a candidate finding since forests and woody material would be related
# Topographic Roughness and Slope are statistically significant (p-value < 0.05)

Mixed.Model.Four <- occu(formula = ~Week + Year + Effort
                         ~ 
                         TopographicRoughnessIndex
                         + WoodyMaterial
                         + Slope
                         + AreaBarrenLand,
                         , data = bobcat.model)

summary(Mixed.Model.Four)

# Not sure if this would count as candidate model
# Topographic Roughness and Slope are statistically significant (p-value < 0.05)

Mixed.Model.Five <- occu(formula = ~Week + Year + Effort
                         ~ 
                         TopographicRoughnessIndex
                         + WoodyMaterial
                         + Slope
                         + AreaCropland,
                         , data = bobcat.model)

summary(Mixed.Model.Five)

# Also not sure if this would count as candidate model
# Topographic Roughness, Slope, and Woody Material are statistically significant (p-value < 0.05)

Mixed.Model.Six <- occu(formula = ~Week + Year + Effort
                         ~ 
                         TopographicRoughnessIndex
                         + WoodyMaterial
                         + Slope
                         + DistancetoUrbanArea,
                         , data = bobcat.model)

summary(Mixed.Model.Six)

# Not sure about results of model above 
# Want to see trend in figure with zeroes taken out 
# Suspect that camera-trap placement played a role - if relationship is negative and
# camera-traps were placed closer to urban areas, not sure if this model is good 
# Topographic Roughness and Slope are statistically significant (p-value < 0.05)

Mixed.Model.Seven <- occu(formula = ~Week + Year + Effort
                         ~ 
                         TopographicRoughnessIndex
                         + WoodyMaterial
                         + Slope
                         + DistancetoWater,
                         , data = bobcat.model)

summary(Mixed.Model.Seven)

# I would count this as a candidate model
# Interesting that it looks like amount of water at site instead of distance to nearest water source from site
# could be more important if the first model is a valid candidate model 
# Double check with Scott to see if he placed camera-traps at sites closer to water sources 
# Topographic Roughness and Slope are statistically significant (p-value < 0.05)

Mixed.Model.Eight <- occu(formula = ~Week + Year + Effort
                         ~ 
                         TopographicRoughnessIndex
                         + WoodyMaterial
                         + Slope
                         + AreaBroadleafDeciduousForest,
                         , data = bobcat.model)

summary(Mixed.Model.Eight)

# This should count as a candidate model
# Makes sense that there would be a relationship between broadleaf deciduous forest and woody material
# Topographic Roughness and Slope are statistically significant (p-value < 0.05)

Mixed.Model.Nine <- occu(formula = ~Week + Year + Effort
                         ~ 
                         TopographicRoughnessIndex
                         + WoodyMaterial
                         + Slope
                         + Area1to25PercentCanopyCover,
                         , data = bobcat.model)

summary(Mixed.Model.Nine)

# This should be considered a candidate model as well
# Topographic Roughness and Slope are statistically significant (p-value < 0.05)

Mixed.Model.Ten <- occu(formula = ~Week + Year + Effort
                         ~ 
                         TopographicRoughnessIndex
                         + WoodyMaterial
                         + Slope
                         + StandDensityIndex,
                         , data = bobcat.model)

summary(Mixed.Model.Ten)

# Should be considered a candidate model
# Topographic Roughness and Slope are statistically significant (p-value < 0.05)

Mixed.Model.Eleven <- occu(formula = ~Week + Year + Effort
                         ~ 
                         TopographicRoughnessIndex
                         + WoodyMaterial
                         + Slope
                         + DistancetoRoad,
                         , data = bobcat.model)

summary(Mixed.Model.Eleven)

# Also not sure about this result 
# Want to see figure when zeroes are excluded 
# If relationship is negative with zeroes excluded, could be result of camera-trap placement
# This would make findings questionable 
# Topographic Roughness and Slope are statistically significant (p-value < 0.05)

Mixed.Model.Twelve <- occu(formula = ~Week + Year + Effort
                         ~ 
                         TopographicRoughnessIndex
                         + WoodyMaterial
                         + Slope
                         + Elevation,
                         , data = bobcat.model)

summary(Mixed.Model.Twelve)

# Should count as candidate model
# Topographic Roughness and Slope are statistically significant (p-value < 0.05)

Mixed.Model.Thirteen <- occu(formula = ~Week + Year + Effort
                         ~ 
                         TopographicRoughnessIndex
                         + WoodyMaterial
                         + Slope
                         + Aspect,
                         , data = bobcat.model)

summary(Mixed.Model.Thirteen)

# Should count as candidate model
# Topographic Roughness and Slope are statistically significant (p-value < 0.05)

Mixed.Model.Fourteen <- occu(formula = ~Week + Year + Effort
                         ~ 
                         TopographicRoughnessIndex
                         + WoodyMaterial
                         + Slope
                         + Area50to75PercentCanopyCover,
                         , data = bobcat.model)

summary(Mixed.Model.Fourteen)

# Should count as candidate model
# Topographic Roughness and Slope are statistically significant (p-value < 0.05)

Mixed.Model.Fifteen <- occu(formula = ~Week + Year + Effort
                         ~ 
                         TopographicRoughnessIndex
                         + WoodyMaterial
                         + Slope
                         + AreaWetland,
                         , data = bobcat.model)

summary(Mixed.Model.Fifteen)

# Not sure if this should be counted as candidate model but it's interesting
# Top 15 models with four variables show that topographic roughness, slope, and woody material
# seem to be the most important factors for this study - need to confirm with Scott
# Topographic Roughness and Slope are statistically significant in all models
# Woody Material is only statistically significant in one model
# Amount of land cover classes also seems to be important - Area of Water, Mixed Forest, Barren Land and
# Cropland were in the top 5 models 

# Top 3 models are remodeled below 
# These models were considered best after some models originally in best 15 were removed since some of 
# the variables did not have data collected on a continuum (i.e. Area of Water, Area Cropland, etc.)

Top.Model <- occu(formula = ~Week + Year + Effort
             ~ 
             TopographicRoughnessIndex
             + WoodyMaterial
             + Slope
             + CanopyHeight,
             , data = bobcat.model)
summary(Top.Model)

# Slope and Topographic Roughness are significant
# Woody Material is not significant but relatively close (p = 0.091)
# Canopy Height is not significant 
# Topographic Roughness is most important since it has lowest p-value
# Slope and Woody Material are second and third most important, respectively 

Best.Model.Two <- occu(formula = ~Week + Year + Effort
             ~ 
             TopographicRoughnessIndex
             + WoodyMaterial
             + Slope
             + DistancetoUrbanArea,
             , data = bobcat.model)
summary(Best.Model.Two)

# Slope and Topographic Roughness are significant
# Woody Material and Distance to Urban Area are not

Best.Model.Three <- occu(formula = ~Week + Year + Effort
             ~ 
             TopographicRoughnessIndex
             + WoodyMaterial
             + Slope
             + DistancetoWater,
             , data = bobcat.model)
summary(Best.Model.Three)

# Slope and Topographic Roughness are significant 
# Woody Material and Distance to Water are not 


# Getting predicted occupancies and detections for each site based on top 3 models
# Also calculating standard error, 95% confidence intervals, and average occupancies and average occupancy standard errors 

# Getting predicted occupancy for all sites for top model (mean + 95% CI)
occ.preds.one <- predict(Top.Model, type = "state", level = 0.95)
head(occ.preds.one) 
# Results show predicted probability of all sites, SE, lower, and upper based on top model

# Getting average predicted occupancy for top model
mean.occupancy.one <- mean(occ.preds.one$Predicted)
print(mean.occupancy.one)

# Getting standard error for mean occupancy probability for top model
mean.occupancy.se.one <- mean(occ.preds.one$SE)
print(mean.occupancy.se.one)

# Getting predicted occupancy for all sites for second best model (mean + 95% CI)
occ.preds.two <- predict(Best.Model.Two, type = "state", level = 0.95)
head(occ.preds.two) 

# Getting average predicted occupancy for second best model 
mean.occupancy.two <- mean(occ.preds.two$Predicted)
print(mean.occupancy.two)

# Getting standard error for mean occupancy probability for second best model
mean.occupancy.se.two <- mean(occ.preds.two$SE)
print(mean.occupancy.se.two)

# Getting predicted occupancy for all sites for third best model (mean + 95% CI)
occ.preds.three <- predict (Best.Model.Three, type = "state", level = 0.95)
head (occ.preds.three) 

# Getting mean occupancy probability for third best model
mean.occupancy.three <- mean(occ.preds.three$Predicted)
print(mean.occupancy.three)

# Getting standard error for mean occupancy probability for third best model
mean.occupancy.se.three <- mean(occ.preds.three$SE)
print(mean.occupancy.se.three)

# Getting predicted detections for all sites for top model
det.preds.one <- predict(Top.Model, type = "det", level = 0.95)
head (det.preds.one)

# Getting predicted detections for all sites for second best model
det.preds.two <- predict(Best.Model.Two, type = "det", level = 0.95)
head (det.preds.two)

# Getting predicted detections for all sites for third best model
det.preds.three <- predict(Best.Model.Three, type = "det", level = 0.95)
head (det.preds.three)


# Attempting to make occupancy probability figures based on Alex Crow's code
# Starts on Alex's most recent R Markdown file on line 1018

# Making topographic roughness occupancy probability predictions with top model

predict.topographic.roughness <- cbind(predict(Top.Model,
                                 newdata = data.frame(TopographicRoughnessIndex
                                 = seq(min(camera_data$TopographicRoughnessIndex, na.rm = TRUE),
                                 max(camera_data$TopographicRoughnessIndex, na.rm = TRUE), 
                                 by = 0.01),
                                 Slope = mean(camera_data$Slope),
                                 CanopyHeight = mean(camera_data$CanopyHeight),
                                 WoodyMaterial = mean(camera_data$WoodyMaterial)),
                                 type = "state"),
                                 data.frame(TopographicRoughnessIndex = seq(min
                                 (camera_data$TopographicRoughnessIndex, na.rm = TRUE),
                                 max(camera_data$TopographicRoughnessIndex, na.rm = TRUE), 
                                 by = 0.01),
                                 Slope = mean(camera_data$Slope),
                                 CanopyHeight = mean(camera_data$CanopyHeight),
                                 WoodyMaterial = mean(camera_data$WoodyMaterial)))

# Making slope occupancy probability predictions with top model

predict.slope <- cbind(predict(Top.Model,
                                 newdata = data.frame(Slope
                                 = seq(min(camera_data$Slope, na.rm = TRUE),
                                 max(camera_data$Slope, na.rm = TRUE), 
                                 by = 0.01),
                                 TopographicRoughnessIndex = mean(camera_data$TopographicRoughnessIndex),
                                 CanopyHeight = mean(camera_data$CanopyHeight),
                                 WoodyMaterial = mean(camera_data$WoodyMaterial)),
                                 type = "state"),
                                 data.frame(Slope = seq(min
                                 (camera_data$Slope, na.rm = TRUE),
                                 max(camera_data$Slope, na.rm = TRUE), 
                                 by = 0.01),
                                 TopographicRoughnessIndex = mean(camera_data$TopographicRoughnessIndex),
                                 CanopyHeight = mean(camera_data$CanopyHeight),
                                 WoodyMaterial = mean(camera_data$WoodyMaterial)))

# Making woody material occupancy probability predictions with top model

predict.woody.material <- cbind(predict(Top.Model,
                                 newdata = data.frame(WoodyMaterial
                                 = seq(min(camera_data$WoodyMaterial, na.rm = TRUE),
                                 max(camera_data$WoodyMaterial, na.rm = TRUE), 
                                 by = 0.01),
                                 TopographicRoughnessIndex = mean(camera_data$TopographicRoughnessIndex),
                                 CanopyHeight = mean(camera_data$CanopyHeight),
                                 Slope = mean(camera_data$Slope)),
                                 type = "state"),
                                 data.frame(WoodyMaterial = seq(min
                                 (camera_data$WoodyMaterial, na.rm = TRUE),
                                 max(camera_data$WoodyMaterial, na.rm = TRUE), 
                                 by = 0.01),
                                 TopographicRoughnessIndex = mean(camera_data$TopographicRoughnessIndex),
                                 CanopyHeight = mean(camera_data$CanopyHeight),
                                 Slope = mean(camera_data$Slope)))

# Making canopy height occupancy probability predictions with top model

predict.canopy.height <- cbind(predict(Top.Model,
                                 newdata = data.frame(CanopyHeight
                                 = seq(min(camera_data$CanopyHeight, na.rm = TRUE),
                                 max(camera_data$CanopyHeight, na.rm = TRUE), 
                                 by = 0.01),
                                 TopographicRoughnessIndex = mean(camera_data$TopographicRoughnessIndex),
                                 WoodyMaterial = mean(camera_data$WoodyMaterial),
                                 Slope = mean(camera_data$Slope)),
                                 type = "state"),
                                 data.frame(CanopyHeight = seq(min
                                 (camera_data$CanopyHeight, na.rm = TRUE),
                                 max(camera_data$CanopyHeight, na.rm = TRUE), 
                                 by = 0.01),
                                 TopographicRoughnessIndex = mean(camera_data$TopographicRoughnessIndex),
                                 WoodyMaterial = mean(camera_data$WoodyMaterial),
                                 Slope = mean(camera_data$Slope)))


# Plotting bobcat occupancy probability relationship with topographic roughness 

ggplot(data = predict.topographic.roughness, aes(x = TopographicRoughnessIndex, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  stat_smooth(method = "loess", col = "black", se = FALSE) +
  labs(x = "Topographic roughness index (scaled)", y = "Predicted bobcat occupancy probability") +
  theme_classic()
# Code is working but not generating figure :(

# Plotting bobcat occupancy probability relationship with slope

ggplot(data = predict.slope, aes(x = Slope, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  stat_smooth(method = "loess", col = "black", se = FALSE) +
  labs(x = "Slope (scaled)", y = "Predicted bobcat occupancy probability") +
  theme_classic()
# Code is working but not generating figure :(

# Plotting bobcat occupancy probability relationship with woody material

ggplot(data = predict.woody.material, aes(x = WoodyMaterial, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  stat_smooth(method = "loess", col = "black", se = FALSE) +
  labs(x = "Woody material (scaled)", y = "Predicted bobcat occupancy probability") +
  theme_classic()
# AHA 
# More dead, woody material = greater probability of bobcat occurrence :)
# Need to recreate with raw data for manuscript

# Plotting bobcat occupancy probability relationship with canopy height 

ggplot(data = predict.canopy.height, aes(x = CanopyHeight, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  stat_smooth(method = "loess", col = "black", se = FALSE) +
  labs(x = "Canopy height (scaled)", y = "Predicted bobcat occupancy probability") +
  theme_classic()
# Code is working but not generating figure :(


# Averaging models based on code provided by Paterson occupancy tutorials

# Creating a list of the 3 best models

occu.model.list <- list(m1 = Top.Model, m2 = Best.Model.Two, m3 = Best.Model.Three)

# Installing package 

install.packages("AICcmodavg")

# Loading the package AICcmodavg

library(AICcmodavg)

# Creating model to predict values based on average of top 3 models 
occu.modavg.psi.predict <- modavgPred(occu.model.list,
                           parm.type = "psi",
                           newdata = bobcat.model@siteCovs)[c("mod.avg.pred",
                                                             "lower.CL",
                                                             "upper.CL")]
                                                             
# Putting predictions, CI, and all site covariates into one data frame
occu.modavg.psi.predict.df <- data.frame(Predicted = occu.modavg.psi.predict$mod.avg.pred,
                              lower = occu.modavg.psi.predict$lower.CL,
                              upper = occu.modavg.psi.predict$upper.CL,
                              camera_data)

# Looking at values for average of all 3 top models 
head(occu.modavg.psi.predict.df)
# Looks like it was done correctly

# Getting summary for average of all 3 top models 
summary(occu.modavg.psi.predict.df)

# Getting average impact of Woody Material on occupancy based on average of top 3 models
avg.psi.woody.material <- modavg(parm = "WoodyMaterial", cand.set = occu.model.list, parm.type = "psi")
print(avg.psi.woody.material)

# Getting average impact of Slope on occupancy based on average of top 3 models
avg.psi.slope <- modavg(parm = "Slope", cand.set = occu.model.list, parm.type = "psi")
print(avg.psi.slope)

# Getting average impact of Topographic Roughness on occupancy based on average of top 3 models
avg.psi.topographic.roughness <- modavg(parm = "TopographicRoughnessIndex", cand.set = occu.model.list, parm.type = "psi")
print(avg.psi.topographic.roughness)

# Getting average impact of Canopy Height on occupancy based on average of top 3 models 
avg.psi.canopy.height <- modavg(parm = "CanopyHeight", cand.set = occu.model.list, parm.type = "psi")
print(avg.psi.canopy.height)

# Getting average impact of Week on occupancy based on average of top 3 models
avg.psi.week <- modavg(parm = "Week", cand.set = occu.model.list, parm.type = "det")
print(avg.psi.week)

# Getting average impact of Year on occupancy based on average of top 3 models
avg.psi.year <- modavg(parm = "Year", cand.set = occu.model.list, parm.type = "det")
print(avg.psi.year)

# Getting average impact of Effort on occupancy based on average of top 3 models
avg.psi.effort <- modavg(parm = "Effort", cand.set = occu.model.list, parm.type = "det")
print(avg.psi.effort)
# Do not understand why the code will not work with the detection parameters :(


# Creating models exclusively based on detection
# These models and their AIC, delta AIC, and AIC weights are in Buckman et al. (2023)

# Model for Week only
Model.Week <- occu(formula = ~Week + Year, data = bobcat.model)
summary(Model.Week)



# Calculating AIC values for the models above based on excel file that 
# contains single-species occupancy modeling results
AIC.values <- c(968.7174, 969.2954, 969.3818, 969.473,
                969.8224, 970.3818, 970.6141, 970.633,
                970.7803, 970.8175)
                
# Using AIC values to calculate delta AIC values
delta.AIC.values <- AIC.values - min(AIC.values)

# Viewing results from delta AIC value calculations
print(delta.AIC.values)

# Calculating AIC weights 
# First install and load R package qpcR
install.packages("qpcR")
library(qpcR)

# Calculate the AIC weights 
AIC.weights <- akaike.weights(AIC.values)
print(AIC.weights$weights)
            
# Conducting Mackenzie-Bailey Goodness of Fit on the best model
Mackenzie.Bailey.Top.Bobcat.Model <- mb.gof.test(Top.Model,
                                  # Demonstrate with small number of sims (10), 
                                  # but then change to large number (e.g. 1000)
                                   nsim = 50)

# View Results
Mackenzie.Bailey.Top.Bobcat.Model
# Not working because det.hist and preds.psi in your dataframe have different numbers of rows :(



