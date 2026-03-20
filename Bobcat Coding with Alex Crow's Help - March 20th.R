
# Bobcat Coding - Setting up the dynamic occupancy model

# Libraries
library(dplyr)
library(tidyr)
library(tibble)
library(unmarked)

# Working directory
setwd("C:/Users/Owner/OneDrive/From Mom's PC/Bobcat Project/Updated Bobcat Project")

# Open data
bobcat.data <- read.csv("standardizedbobcatdata2.0.csv", stringsAsFactors = T)

# Clean CameraTrap IDs
bobcat.data$CameraTrap <- bobcat.data$CameraTrap %>%
  trimws() %>%                 # remove leading/trailing spaces
  toupper() %>%                # standardize case (optional but recommended)
  gsub("[^A-Z0-9_]", "", .)    # remove weird characters (keep letters, numbers, underscore)

# Formula for choosing the first non-NA value for CameraTrap
safe_first <- function(x) {
  x <- x[!is.na(x)]
  if(length(x) == 0) return(NA)
  x[1]
}

# Make SiteYear - TrapID_X (1-5)
bobcat.data$SiteYear <- paste(bobcat.data$CameraTrap, bobcat.data$StudyYear, sep = "_")

# Convert detection data into binary (0-1)
bobcat.data <- bobcat.data %>%
  mutate(
    Detect = ifelse(
      is.na(NumberofBobcatCaptures), NA,
      ifelse(NumberofBobcatCaptures > 0, 1, 0)
    )
  )

# Remove duplicates
bobcat.data <- bobcat.data %>%
  group_by(CameraTrap, StudyYear, DeploymentDay) %>%
  summarise(
    Detect = max(Detect, na.rm = TRUE),
    
    DistancetoUrbanArea = first(na.omit(DistancetoUrbanArea)),
    Slope = first(na.omit(Slope)),
    Aspect = first(na.omit(Aspect)),
    DistancetoWater = first(na.omit(DistancetoWater)),
    DistancetoRoad = first(na.omit(DistancetoRoad)),
    Elevation = first(na.omit(Elevation)),
    WoodyMaterial = first(na.omit(WoodyMaterial)),
    StandAge = first(na.omit(StandAge)),
    StandDensityIndex = first(na.omit(StandDensityIndex)),
    TopographicRoughnessIndex = first(na.omit(TopographicRoughnessIndex)),
    AreaBroadleafDeciduousForestBobcats = first(na.omit(AreaBroadleafDeciduousForestBobcats)),
    Area50to75PercentCanopyCoverBobcats = first(na.omit(Area50to75PercentCanopyCoverBobcats)),
    
    .groups = "drop"
  )

# Make site.data BEFORE completing cases, since it introduces NAs
site.data <- bobcat.data %>%
  group_by(CameraTrap) %>%
  summarise(
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
    AreaBroadleafDeciduousForestBobcats = safe_first(AreaBroadleafDeciduousForestBobcats),
    Area50to75PercentCanopyCoverBobcats = safe_first(Area50to75PercentCanopyCoverBobcats),
    .groups = "drop"
  )

# There are years with missing data (hence the safe_first), and camera traps 
# with no data whatsoever for the variables identified above. E.g. ARPO3

# Make the row names CameraTrap
site.data <- as.data.frame(site.data)
rownames(site.data) <- site.data$CameraTrap
site.data$CameraTrap<-as.factor(site.data$CameraTrap)

# Set total sampling effort per season
all_days <- 1:131

# Complete missing rows (no sampling)
bobcat.data <- bobcat.data %>%
  group_by(CameraTrap, StudyYear) %>%
  complete(DeploymentDay = all_days) %>%
  ungroup()

# Make Data for DOM - Rows = Trap, Columns = Year_DayofDeployment
y <- bobcat.data %>%
  arrange(CameraTrap, StudyYear, DeploymentDay) %>%
  pivot_wider(
    id_cols = CameraTrap,
    names_from = c(StudyYear, DeploymentDay),
    values_from = Detect
  )

# Make the row names CameraTrap
y <- as.data.frame(y)
rownames(y) <- y$CameraTrap

# Remove the CameraTrap helper column
y$CameraTrap <- NULL

# Align site.data to y
site.data <- site.data[rownames(y), ]

# Remove the helper column again
site.data$SiteYear <- NULL

# Tests!
print(all(rownames(y) == rownames(site.data)))  # MUST be TRUE - Will deal with later :D
print(ncol(y) %% 5 == 0)                        # MUST be TRUE
# It WORRRRKSSSSSS
# YAYYYYYY

# Creating the unmarked data frame
bobcat.umf <- unmarkedMultFrame(
  y = as.matrix(y),
  siteCovs = site.data,
  numPrimary = 5
)

summary(bobcat.umf)
# IT STILL WORKS!!!

plot(bobcat.umf)
# Noice!

# Models!
dynamic.occ.bobcat.one <- colext(~1, # Occupancy constant
                                 ~1, # Colonization constant
                                 ~1, # Extinction constant
                                 ~1, # Detection constant
                                 data = bobcat.umf)

# Next extract parameter estimates 
occupancy_est <- backTransform(dynamic.occ.bobcat.one, type = "psi") # Should estimate occupancy probability 
colonization_est <- backTransform(dynamic.occ.bobcat.one, type = "col") # Should estimate colonization probability 
extinction_est <- backTransform(dynamic.occ.bobcat.one, type = "ext") # Should estimate extinction probability 
detection_est <- backTransform(dynamic.occ.bobcat.one, type = "det") # Should estimate detection probability 

# Next print the estimates 
cat("Occupancy probability (psi):", occupancy_est@estimate, "\n")
cat("Colonization probability (gamma):", colonization_est@estimate, "\n")
cat("Extinction probability (epsilon):", extinction_est@estimate, "\n")
cat("Detection probability (p):", detection_est@estimate, "\n")

# Model with detection dependent on effort 
bobcat.m1 <- colext(~1, # Occupancy constant
                    ~1 # Colonization constant 
                    ~1 # Extinction constant
                    ~effort, # Probability of detection is affected by effort
                    data = bobcat.umf) # We need to incorporate effort in somehow :(
bobcat.m2 <- colext(~1, ~1, ~1, ~feature_type, data = bobcat.umf) # Feature type also needs to be incorporated somehow :(
bobcat.m3 <- colext(~1, ~1, ~1, ~effort+feature_type, data = bobcat.umf)

