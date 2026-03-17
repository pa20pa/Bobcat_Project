# Redo of Bobcat Coding
# Date: March 17th, 2026
# STICK WITH THIS FILE 
# PERFORMED SUCCESSFUL COLLINEARITY TESTS!!!
# NOW YOU KNOW WHAT VARIABLES TO INCLUDE IN SUBSEQUENT TESTING!!! :D 

# Housekeeping
rm(list = ls())

# Setting your working directory 
getwd() # This line gives current working directory 
setwd("C:/Users/Owner/OneDrive/From Mom's PC/Bobcat Project/Updated Bobcat Project") # This line changes the working directory 

# Opening and reading your dataset
d<-read.csv("BobcatProject3.0Final.csv", stringsAsFactors = T) # This line tells R to read the bobcat file
str(d)
View(d)

library(skimr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(car)
library(DHARMa)
library(performance)
library(ggcorrplot)

# Opening and reading your dataset
d<-read.csv("BobcatProject3.0Final.csv", stringsAsFactors = T) # This line tells R to read the bobcat file
str(d)
View(d)
d$StudyYear<-as.factor(d$StudyYear)

# Doing data quality checks
levels(d$Month)
levels(d$CameraTrap)
levels(d$StudyYear)
levels(d$BobcatCapture)
levels(d$LatitudeandLongitudeIncludesSummerDeployment)

# Initial look at your data
skim(d)
str(d)

# Determining the number of study nights per year
d %>%
  dplyr::group_by(StudyYear) %>%
  dplyr::summarise(NumCameraTrapNights=dplyr::n())

# Standardizing data 

# Install and load dpylr package
install.packages("dplyr")
library(dplyr)

# Creating function to standardize variables
standardize <- function(x){
  return((x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))
}

# Applying function to your dataset 
std_bobcat_data <- as.data.frame(apply(d[14:211],2,standardize)) # Want to standardize data found in columns 14-211
# This appears to standardize data, but not sure if NA values were included
# It looks like NA values were included based on NA values in the standardized data table

# Website used to get code to standardize data
# https://www.youtube.com/watch?v=dPTkby7Kpp0

# Adding the first 13 columns back into dataframe 
combined_bobcat_data <- cbind(d$CameraTrap, 
                              d$Latitude, 
                              d$Longitude, 
                              d$Month, 
                              d$DayofMonth,
                              d$DeploymentDay, 
                              d$StudyYear,
                              d$BobcatCapture, 
                              d$NumberofBobcatCaptures,
                              d$LatitudeandLongitudeIncludesSummerDeployment,
                              d$SiteSurveyedMoreThanOnceinYear, 
                              d$NumberofCameraTrapNightsinYear,
                              d$SurveyEffort,
                              std_bobcat_data)
View(combined_bobcat_data)

# Renaming columns of dependent variables with d$ in title 
colnames(combined_bobcat_data)[which(names(combined_bobcat_data) == "d$NumberofBobcatCaptures")] <- "NumberofBobcatCaptures"
colnames(combined_bobcat_data)[which(names(combined_bobcat_data) == "d$CameraTrap")] <- "CameraTrap"
colnames(combined_bobcat_data)[which(names(combined_bobcat_data) == "d$Latitude")] <- "Latitude"
colnames(combined_bobcat_data)[which(names(combined_bobcat_data) == "d$Longitude")] <- "Longitude"
colnames(combined_bobcat_data)[which(names(combined_bobcat_data) == "d$Month")] <- "Month"
colnames(combined_bobcat_data)[which(names(combined_bobcat_data) == "d$DayofMonth")] <- "DayofMonth"
colnames(combined_bobcat_data)[which(names(combined_bobcat_data) == "d$DeploymentDay")] <- "DeploymentDay"
colnames(combined_bobcat_data)[which(names(combined_bobcat_data) == "d$StudyYear")] <- "StudyYear"
colnames(combined_bobcat_data)[which(names(combined_bobcat_data) == "d$BobcatCapture")] <- "BobcatCapture"
colnames(combined_bobcat_data)[which(names(combined_bobcat_data) == "d$LatitudeandLongitudeIncludesSummerDeployment")] <- "LatitudeandLongitudeIncludesSummerDeployment"
colnames(combined_bobcat_data)[which(names(combined_bobcat_data) == "d$SiteSurveyedMoreThanOnceinYear")] <- "SiteSurveyedMoreThanOnceinYear"
colnames(combined_bobcat_data)[which(names(combined_bobcat_data) == "d$NumberofCameraTrapNightsinYear")] <- "NumberofCameraTrapNightsinYear"
colnames(combined_bobcat_data)[which(names(combined_bobcat_data) == "d$LatencyofDetectioninYear")] <- "LatencyofDetectioninYear"
view(combined_bobcat_data)

# Exporting combined_bobcat_data as a csv 
write.csv(combined_bobcat_data, "standardizedbobcatdata2.0.csv")

# Getting location of working directory 
getwd()

# Exporting csv file
write.csv(combined_bobcat_data, "standardizedbobcatdata2.0.csv",
          row.names = FALSE)

# Testing for multicollinearity between variables using glmmTMB() and check_collinearity() functions 

# Installing essential packages 
install.packages("performance")
library("performance")
install.packages("rbibutils")
packageVersion("rbibutils")
install.packages("glmmTMB")
library("glmmTMB")

# Checking for collinearity between landcover variables 
collinearity.check.landcover <- glmmTMB(NumberofBobcatCaptures ~ AreaNeedleleafForestBobcats + AreaBroadleafDeciduousForestBobcats + AreaMixedForestBobcats +
                                AreaShrublandBobcats + AreaGrasslandBobcats + AreaWetlandBobcats +
                                AreaCroplandBobcats + AreaBarrenLandBobcats + AreaUrbanBobcats + 
                                AreaWaterBobcats + AreaNeedleleafForestNoBobcats + AreaBroadleafDeciduousForestNoBobcats +
                                AreaMixedForestNoBobcats + AreaShrublandNoBobcats + AreaGrasslandNoBobcats +
                                AreaWetlandNoBobcats + AreaCroplandNoBobcats + AreaBarrenLandNoBobcats +
                                AreaUrbanNoBobcats + AreaWaterNoBobcats, data = combined_bobcat_data)
check_collinearity(collinearity.check.landcover)
# Only keeping AreaBroadleafDeciduousForestBobcats for testing in overall model since it has lowest VIF
# Thesis also showed Broadleaf Deciduous Forest was most common land cover type in study area 
# Since thesis showed no difference in land cover between sites where bobcats did and did not occur, bobcats were likely using 
# most readily available land cover type - makes this land cover type most valid to include in overall model
# Still has VIF > 3 so no other landcover variables will be in overall model

# Checking for collinearity between percent canopy cover variables 
collinearity.check.canopycover <- glmmTMB(NumberofBobcatCaptures ~ Area0to1PercentCanopyCoverBobcats +
                                          Area1to25PercentCanopyCoverBobcats + Area25to50PercentCanopyCoverBobcats + 
                                          Area50to75PercentCanopyCoverBobcats + AreaGreater75PercentCanopyCoverBobcats,
                                          data = combined_bobcat_data)
check_collinearity(collinearity.check.canopycover)
# No variables exhibit low collinearity
# Keeping Area50to75PercentCanopyCoverBobcats for testing in overall model
# Most likely to explain bobcat occurrence based on finding that canopy cover of bobcats can peak at 66%
# Finding is in Serieys et al. (2025)
# Paper is called: Bobcats select young forests and avoid clear-cut and mature forests in a timber-logged landscape

# Checking for collinearity between stand composition variables WITH bobcats 
collinearity.check.standcomposition <- glmmTMB(NumberofBobcatCaptures ~ AreaRedPineBobcats + AreaTamarackBobcats + AreaPitchPineBobcats +
                                               AreaEasternRedcedarBobcats + AreaExoticSoftwoodsBobcats + AreaNorwaySpruceBobcats +
                                               AreaEasternWhitePineNorthernRedOakWhiteAshBobcats + AreaEasternRedCedarHardwoodBobcats +
                                               AreaChestnutOakBobcats + AreaWhiteOakRedOakHickoryBobcats + AreaOtherPineHardwoodBobcats +
                                               AreaWhiteOakBobcats + AreaNorthernRedOakBobcats + AreaBurOakBobcats +
                                               AreaYellowPoplarWhiteOakNorthernRedOakBobcats + AreaScarletOakBobcats + 
                                               AreaYellowPoplarBobcats + AreaBlackWalnutBobcats + AreaBlackLocustBobcats +
                                               AreaCherryWhiteAshYellowPoplarBobcats + AreaChestnutOakBlackOakScarletOakBobcats +
                                               AreaElmAshBlackLocustBobcats + AreaRedMapleOakBobcats + AreaAtlanticWhiteCedarBobcats +
                                               AreaMixedUplandHardwoodsBobcats + AreaBlackAshAmericanElmRedMapleBobcats +
                                               AreaRedMapleLowlandBobcats + AreaSycamorePecanAmericanElmBobcats + AreaRiverBirchSycamoreBobcats +
                                               AreaSugarMapleBeechYellowBirchBobcats + AreaHardMapleBasswoodBobcats +
                                               AreaRedMapleUplandBobcats + AreaPaperBirchBobcats + AreaGrayBirchBobcats +
                                               AreaNorthernWhiteCedarBobcats + AreaShortleafPineBobcats + AreaLoblollyPineBobcats +
                                               AreaEasternWhitePineBobcats + AreaEasternWhitePineEasternHemlockBobcats + AreaEasternHemlockBobcats + 
                                               AreaAspenBobcats + AreaRedPineNoBobcats + AreaPitchPineNoBobcats +
                                               AreaEasternRedcedarNoBobcats + AreaExoticSoftwoodsNoBobcats + AreaNorwaySpruceNoBobcats +
                                               AreaEasternWhitePineNorthernRedOakWhiteAshNoBobcats + AreaEasternRedCedarHardwoodNoBobcats +
                                               AreaChestnutOakNoBobcats + AreaWhiteOakRedOakHickoryNoBobcats + AreaOtherPineHardwoodNoBobcats +
                                               AreaWhiteOakNoBobcats + AreaNorthernRedOakNoBobcats +
                                               AreaYellowPoplarWhiteOakNorthernRedOakNoBobcats + AreaScarletOakNoBobcats + 
                                               AreaYellowPoplarNoBobcats + AreaBlackWalnutNoBobcats + AreaBlackLocustNoBobcats +
                                               AreaCherryWhiteAshYellowPoplarNoBobcats + AreaChestnutOakBlackOakScarletOakNoBobcats +
                                               AreaElmAshBlackLocustNoBobcats + AreaRedMapleOakNoBobcats + AreaAtlanticWhiteCedarNoBobcats +
                                               AreaMixedUplandHardwoodsNoBobcats + AreaBlackAshAmericanElmRedMapleNoBobcats +
                                               AreaRedMapleLowlandNoBobcats + AreaRiverBirchSycamoreNoBobcats +
                                               AreaSugarMapleBeechYellowBirchNoBobcats + AreaHardMapleBasswoodNoBobcats +
                                               AreaRedMapleUplandNoBobcats + AreaPaperBirchNoBobcats + AreaGrayBirchNoBobcats +
                                               AreaLoblollyPineNoBobcats + AreaEasternWhitePineNoBobcats + AreaEasternWhitePineEasternHemlockNoBobcats +
                                               AreaEasternHemlockNoBobcats + AreaCottonwoodNoBobcats + AreaAspenNoBobcats +
                                               AreaBlackCherryNoBobcats + AreaNorthernWhiteCedarNoBobcats, data = combined_bobcat_data)
check_collinearity(collinearity.check.standcomposition)
# AreaTamarackBobcats has lowest VIF - likely due to large number of zeroes and low values for this variable 
# Still has VIF > 3 
# Given that bobcats used the most common land cover type in the study area most and there was no difference in the most common
# land cover type where bobcats did and did not occur, I would expect something similar for stand composition
# AreaWhiteOakRedOakHickory was most common stand composition type where bobcats occurred and where they did not occur
# AreaWhiteOakRedOakHickoryBobcats will be included in overall model even though it does not have lowest VIF
# Removed the following for now since they caused issues: 
# AreaCottonwoodBobcats, AreaBlackCherryBobcats, AreaTamarackNoBobcats, AreaShortleafPineNoBobcats, AreaBurOakNoBobcats, and AreaSycamorePecanAmericanElmNoBobcats

# Checking for collinearity between canopy height variables WITH bobcats 
collinearity.check.canopyheight <- glmmTMB(NumberofBobcatCaptures ~ AreaCanopyHeight18Bobcats + AreaCanopyHeight19Bobcats + AreaCanopyHeight20Bobcats +
                                           AreaCanopyHeight21Bobcats + AreaCanopyHeight17Bobcats + AreaCanopyHeight22Bobcats + AreaCanopyHeight23Bobcats + 
                                           AreaCanopyHeight24Bobcats + AreaCanopyHeight25Bobcats + AreaCanopyHeight26Bobcats + AreaCanopyHeight27Bobcats + 
                                           AreaCanopyHeight28Bobcats + AreaCanopyHeight29Bobcats + AreaCanopyHeight30Bobcats + AreaCanopyHeight31Bobcats +
                                           AreaCanopyHeight32Bobcats + AreaCanopyHeight33Bobcats + AreaCanopyHeight16Bobcats + AreaCanopyHeight15Bobcats + 
                                           AreaCanopyHeight14Bobcats + AreaCanopyHeight13Bobcats + AreaCanopyHeight12Bobcats + AreaCanopyHeight11Bobcats +
                                           AreaCanopyHeight10Bobcats + AreaCanopyHeight9Bobcats + AreaCanopyHeight8Bobcats + AreaCanopyHeight7Bobcats +
                                           AreaCanopyHeight6Bobcats + AreaCanopyHeight5Bobcats + AreaCanopyHeight4Bobcats + AreaCanopyHeight3Bobcats, 
                                           data = combined_bobcat_data)
check_collinearity(collinearity.check.canopyheight)
# Keeping AreaCanopyHeight18Bobcats in final model since VIF is lowest
# Still has VIF > 3 so no other canopy height variables WITH bobcats will be in overall model
# Also keeping this height due to evidence from Serieys et al. (2025) that bobcats select young forests between 6-15 years old
# Quick search shows stands can grow to heights of approx 18 m at this age 
# Removed the following for now since they caused issues: 
# AreaCanopyHeight0Bobcats, AreaCanopyHeight1Bobcats, and AreaCanopyHeight2Bobcats

# Checking for collinearity between canopy height variables WITHOUT bobcats
collinearity.check.canopyheight.nobobcats <- glmmTMB(NumberofBobcatCaptures ~ AreaCanopyHeight18NoBobcats + AreaCanopyHeight19NoBobcats + AreaCanopyHeight20NoBobcats +
                                             AreaCanopyHeight21NoBobcats + AreaCanopyHeight17NoBobcats + AreaCanopyHeight22NoBobcats + AreaCanopyHeight23NoBobcats + 
                                             AreaCanopyHeight24NoBobcats + AreaCanopyHeight25NoBobcats + AreaCanopyHeight26NoBobcats + AreaCanopyHeight27NoBobcats + 
                                             AreaCanopyHeight28NoBobcats + AreaCanopyHeight29NoBobcats + AreaCanopyHeight30NoBobcats + AreaCanopyHeight31NoBobcats +
                                             AreaCanopyHeight32NoBobcats + AreaCanopyHeight33NoBobcats + AreaCanopyHeight16NoBobcats + AreaCanopyHeight15NoBobcats + 
                                             AreaCanopyHeight14NoBobcats + AreaCanopyHeight13NoBobcats + AreaCanopyHeight12NoBobcats + AreaCanopyHeight11NoBobcats +
                                             AreaCanopyHeight10NoBobcats + AreaCanopyHeight9NoBobcats + AreaCanopyHeight8NoBobcats + AreaCanopyHeight7NoBobcats +
                                             AreaCanopyHeight6NoBobcats + AreaCanopyHeight5NoBobcats + AreaCanopyHeight4NoBobcats, data = combined_bobcat_data)
check_collinearity(collinearity.check.canopyheight.nobobcats)
# Keeping AreaCanopyHeight18NoBobcats in final model since VIF is lowest
# Also due to what was found in test above 
# Still has VIF > 3 so no other canopy height variables WITHOUT bobcats will be in overall model
# Removed the following for now since they caused issues: 
# AreaCanopyHeight0NoBobcats, AreaCanopyHeight1NoBobcats, AreaCanopyHeight2NoBobcats, and AreaCanopyHeight3NoBobcats

# Checking for collinearity with "problem" variables 
# Trying with truncated poisson since these variables contain zero-inflated data
# Trying with canopy height variables first and do two variables at a time
collinearity.check.poisson.canopyheight.one <- glmmTMB(NumberofBobcatCaptures ~ AreaCanopyHeight0Bobcats + AreaCanopyHeight1Bobcats, 
                                               data = combined_bobcat_data, family = truncated_poisson)
check_collinearity(collinearity.check.poisson.canopyheight.one)
# Model did not work BUT there was perfect collinearity between the two variables
# Keeping AreaCanopyHeight0Bobcats and testing with other "problem" canopy height variables one at a time
collinearity.check.poisson.canopyheight.two <- glmmTMB(NumberofBobcatCaptures ~ AreaCanopyHeight0Bobcats + AreaCanopyHeight2Bobcats, 
                                                       data = combined_bobcat_data, family = truncated_poisson)
check_collinearity(collinearity.check.poisson.canopyheight.two)
collinearity.check.poisson.canopyheight.three <- glmmTMB(NumberofBobcatCaptures ~ AreaCanopyHeight0Bobcats + AreaCanopyHeight0NoBobcats, 
                                                         data = combined_bobcat_data, family = truncated_poisson)
check_collinearity(collinearity.check.poisson.canopyheight.three) 
collinearity.check.poisson.canopyheight.four <- glmmTMB(NumberofBobcatCaptures ~ AreaCanopyHeight0Bobcats + AreaCanopyHeight1NoBobcats, 
                                                         data = combined_bobcat_data, family = truncated_poisson)
check_collinearity(collinearity.check.poisson.canopyheight.four)
collinearity.check.poisson.canopyheight.five <- glmmTMB(NumberofBobcatCaptures ~ AreaCanopyHeight0Bobcats + AreaCanopyHeight2NoBobcats, 
                                                        data = combined_bobcat_data, family = truncated_poisson)
check_collinearity(collinearity.check.poisson.canopyheight.five)
collinearity.check.poisson.canopyheight.six <- glmmTMB(NumberofBobcatCaptures ~ AreaCanopyHeight0Bobcats + AreaCanopyHeight3NoBobcats, 
                                                        data = combined_bobcat_data, family = truncated_poisson)
check_collinearity(collinearity.check.poisson.canopyheight.six)
# Above models show PERFECT collinearity between all "problem" canopy height variables even though models had errors

# Checking for collinearity with "problem" stand composition variables
# Also doing this two variables at a time to determine if they are perfectly correlated 
collinearity.check.poisson.standcomposition.one <- glmmTMB(NumberofBobcatCaptures ~ AreaCottonwoodBobcats + AreaBlackCherryBobcats,
                                                          data = combined_bobcat_data, family = truncated_poisson)
check_collinearity(collinearity.check.poisson.standcomposition.one)
# Going to test remaining variables with AreaCottonwoodBobcats
collinearity.check.poisson.standcomposition.two <- glmmTMB(NumberofBobcatCaptures ~ AreaCottonwoodBobcats + AreaTamarackNoBobcats,
                                                           data = combined_bobcat_data, family = truncated_poisson)
check_collinearity(collinearity.check.poisson.standcomposition.two)
collinearity.check.poisson.standcomposition.three <- glmmTMB(NumberofBobcatCaptures ~ AreaCottonwoodBobcats + AreaShortleafPineNoBobcats,
                                                             data = combined_bobcat_data, family = truncated_poisson)
check_collinearity(collinearity.check.poisson.standcomposition.three)
collinearity.check.poisson.standcomposition.four <- glmmTMB(NumberofBobcatCaptures ~ AreaCottonwoodBobcats + AreaSycamorePecanAmericanElmNoBobcats,
                                                            data = combined_bobcat_data, family = truncated_poisson)
check_collinearity(collinearity.check.poisson.standcomposition.four)
collinearity.check.poisson.standcomposition.five <- glmmTMB(NumberofBobcatCaptures ~ AreaCottonwoodBobcats + AreaBurOakNoBobcats,
                                                            data = combined_bobcat_data, family = truncated_poisson)
check_collinearity(collinearity.check.poisson.standcomposition.five)
# Above models also show PERFECT collinearity between all "problem" stand composition variables

# Tested for collinearity between a "problem" stand composition and "problem" canopy height variable 
# Also exhibited PERFECT collinearity
# AreaCanopyHeight0NoBobcats is probably the best predictor variable out of the "problem" variables
# Evidence that many felids avoid using areas with little vegetation 
# Lovallo and Anderson (1996) also found bobcats use unforested areas less during winter 
# Paper is called: Bobcat (Lynx rufus) Home Range Size and Habitat Use in Northwest Wisconsin
# Including AreaCanopyHeight0NoBobcats in overall model

# Checking for collinearity with FINAL variables 
collinearity.check.allvariables <- glmmTMB(NumberofBobcatCaptures ~ DistancetoUrbanArea +
                                    Slope + Aspect + DistancetoWater + DistancetoRoad + 
                                    Elevation + WoodyMaterial + StandAge + StandDensityIndex +
                                    TopographicRoughnessIndex + AreaBroadleafDeciduousForestBobcats +
                                    Area50to75PercentCanopyCoverBobcats + AreaWhiteOakRedOakHickoryBobcats + 
                                    AreaCanopyHeight18Bobcats + AreaCanopyHeight18NoBobcats +
                                    AreaCanopyHeight0NoBobcats, data = combined_bobcat_data, family = truncated_poisson)
check_collinearity(collinearity.check.allvariables)
# Did not work :(
# Trying after removing AreaCanopyHeight0NoBobcats
# Should be okay since this was indirectly tested for via AreaBarrenLandNoBobcats and that was good to remove earlier
collinearity.check.allnonproblemvariables <- glmmTMB(NumberofBobcatCaptures ~ DistancetoUrbanArea +
                                                     Slope + Aspect + DistancetoWater + DistancetoRoad + 
                                                     Elevation + WoodyMaterial + StandAge + StandDensityIndex +
                                                     TopographicRoughnessIndex + AreaBroadleafDeciduousForestBobcats +
                                                     Area50to75PercentCanopyCoverBobcats + AreaWhiteOakRedOakHickoryBobcats + 
                                                     AreaCanopyHeight18Bobcats + AreaCanopyHeight18NoBobcats, 
                                                     data = combined_bobcat_data) # Worked fine after dropping "problem" variable
check_collinearity(collinearity.check.allnonproblemvariables)
# THIS SHOWS VIF VALUES FOR VARIABLES ABOVE 
# Test automatically dropped the following variable:
# AreaCanopyHeight18NoBobcats
# Almost all have low correlation but two low-correlation variables have VIF > 3
# Variables are:
# Area50to75PercentCanopyCoverBobcats and AreaCanopyHeight18Bobcats
# Removing latter from following tests since former is likely more important due to shelter, hunting opportunities,
# and protection from predators that canopy cover provides
# Two variables also show high correlation with VIF >>> 3:
# AreaBroadleafDeciduousForestBobcats and AreaWhiteOakRedOakHickoryBobcats
# Latter is probably less important so removing that one from final test 
collinearity.check.final <- glmmTMB(NumberofBobcatCaptures ~ DistancetoUrbanArea +
                                    Slope + Aspect + DistancetoWater + DistancetoRoad + 
                                    Elevation + WoodyMaterial + StandAge + StandDensityIndex +
                                    TopographicRoughnessIndex + AreaBroadleafDeciduousForestBobcats +
                                    Area50to75PercentCanopyCoverBobcats, 
                                    data = combined_bobcat_data)
check_collinearity(collinearity.check.final)
# ALL VARIABLES IN THIS MODEL HAVE VIF < 3 
# ALL WILL BE INCLUDED AS PREDICTORS TO EXPLAIN BOBCAT OCCURRENCE WITH SUBSEQUENT MODELS 
# FOR VARIABLES WHERE BOBCATS VS. NO BOBCATS DATA WERE COLLECTED - YOU WILL NEED TO COMPARE 
# DATA WHERE BOBCATS OCCURRED TO DATA WHERE BOBCATS DID NOT OCCUR 

# Next model for bobcat occupancy and detection
# First you need to install and load the packages you need
list.of.packages <- c(
                    "unmarked",       # occupancy modeling packge
                    "tidyr",          # A package for data manipulation
                    "dplyr",          # A package for data manipulation
                    "vegan",          # tools for descriptive parameters in ecology
                    "AICcmodavg",     # package to implement data averaging
                    "readr",          # package to read tables
                    "ggplot2",        # visusalisations
                    "gridExtra",      # multiple grid based
                    "kableExtra",     # HTML tables
                    "knitr") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Next is to load your dataset based on the standardized data
bobcat.data <- read.csv("standardizedbobcatdata2.0.csv", stringsAsFactors = T)
View(bobcat.data)

# Next is to create unmarked dataframe
# Did this after standardizing survey period for each year so sites were surveyed for 131 days each year
# Year 2 = first day of survey is now Nov 22nd 2019 (different than other years since it was a leap year)
# Other years = first day of survey is now Nov 21st 
library("unmarked")

# Code Graveyard :(
# y <- bobcat.data[9] # Creating part of dataframe with CameraTrap, DeploymentDay, StudyYear, and NumberofBobcatCaptures
# head(y)
# site.data <- bobcat.data[c(15, 34:36, 40, 47:53)] 
# head(site.data)
# obs.data <- bobcat.data[c(1,6:7)]
# head(obs.data)
# NOTE - YOU WILL NEED TO FIGURE OUT HOW TO INCORPORATE DETECTION PROBABILITY INTO MODEL

# Another Code Graveyard :( 
# Creating unmarked dataframe with function unmarkedMultFrame()
# Using this function since you have data from more than one season 
# bobcat.umf <- unmarkedMultFrame(y = y, siteCovs = site.data, numPrimary = 1) # Tried putting numPrimary = 5 but the code had an error
# summary(bobcat.umf)
# Not sure if this is the correct code exactly BUT it runs
# Thinking numPrimary should = 5 since the data are from 5 years but the code would not run :(

# UPDATE AS OF MARCH 17TH!!!!!!!!!
# YOU GOT THE UNMAKREDMULTFRAME TO WORK WHEN NUMPRIMARY = 5!!!
# THIS IS IN THE SEPARATE R FILE YOU CREATED WITH ALEX CROW ON MARCH 17TH
# CONTINUE TO CREATE YOUR DYNAMIC OCCUPANCY MODEL BASED ON THAT - RUN THAT FILE WHEN YOU GET TO THIS POINT IN YOUR CODE

View(bobcat.umf)
View(site.data)
View(y) 
View(obs.data)
# I originally had only the dependent variable (aka NumberofBobcatCaptures) in y 
# y needed to also include variables that are related to survey effort and detection probability 
# Ended up incorporating the following into y as well: CameraTrap, DeploymentDay, and StudyYear

# Now try creating your model for occupancy process
# Need to model with dynamic occupancy modelling since your data covers more than one year
dynamic.occ.bobcat.one <- colext(~1, # Occupancy constant
                                 ~1, # Colonization constant
                                 ~1, # Extinction constant
                                 ~1, # Detection constant
                                 data = bobcat.umf)

summary()

# You are following along with the code on the following webstie:
# https://rpubs.com/auzal/DynamicOccupancyModel_wolf

