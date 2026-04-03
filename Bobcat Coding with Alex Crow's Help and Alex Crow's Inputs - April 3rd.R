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
  filter(!CameraTrap %in% c("ARPO3", "SKNO2", "SUTS3", "TRES4")) %>%
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

# All combinations
all_traps <- unique(bobcat.data$CameraTrap)
all_years <- unique(bobcat.data$StudyYear)
all_days  <- 1:131

# Complete Cases
bobcat.data <- bobcat.data %>%
  complete(
    CameraTrap = all_traps,
    StudyYear = all_years,
    DeploymentDay = all_days
  )

# Effort
bobcat.data <- bobcat.data %>%
  mutate(
    effort = ifelse(is.na(Detect), 0, 1)
  )

effort_mat <- bobcat.data %>%
  arrange(CameraTrap, StudyYear, DeploymentDay) %>%
  pivot_wider(
    id_cols = CameraTrap,
    names_from = c(StudyYear, DeploymentDay),
    values_from = effort
  )

# Make the row names CameraTrap
effort_mat <- as.data.frame(effort_mat)
rownames(effort_mat) <- effort_mat$CameraTrap

# Remove the CameraTrap helper column
effort_mat$CameraTrap <- NULL

# Make Data for DOM - Rows = Trap, Columns = Year_DayofDeployment
y <- bobcat.data %>%
  arrange(CameraTrap, StudyYear, DeploymentDay) %>%
  pivot_wider(
    id_cols = CameraTrap,
    names_from = c(StudyYear, DeploymentDay),
    values_from = Detect,
    names_sep = "_"
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

dim(y)
sum(is.na(y))
# It WORRRRKSSSSSS

# Creating the unmarked data frame
bobcat.umf <- unmarkedMultFrame(
  y = as.matrix(y),
  siteCovs = site.data,
  obsCovs = list(effort = effort_mat),
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

# This is the end


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
                    ~1, # Colonization constant 
                    ~1, # Extinction constant
                    ~effort, # Probability of detection is affected by effort
                    data = bobcat.umf) 
bobcat.m2 <- colext(~1, ~1, ~1, ~Elevation, data = bobcat.umf) # Feature type also needs to be incorporated somehow :(
bobcat.m3 <- colext(~1, ~1, ~1, ~effort+Elevation, data = bobcat.umf)


dlist<-fitList(Null = dynamic.occ.bobcat.one,m1=bobcat.m1, m2=bobcat.m2, m3=bobcat.m3)
selmod<-modSel(dlist,nullmod="Null")
selmod

# Model that contains all covariates of interest
# This is global model
bobcat.m.global <- colext(~1, ~1, ~1, ~DistancetoUrbanArea+Slope+Aspect+DistancetoWater+DistancetoRoad+Elevation
                          +WoodyMaterial+StandAge+StandDensityIndex+TopographicRoughnessIndex+AreaBroadleafDeciduousForestBobcats
                          +Area50to75PercentCanopyCoverBobcats+effort, data = bobcat.umf)

# Get packages MuMIn and AICcmodavg
library(MuMIn)
library(AICcmodavg)

# Get all submodels from global model 
# Not working :(
all.bobcat.submodels <- dredge(bobcat.m.global)

# Create model selection table 
bobcat.model.table <- aictab(all.bobcat.submodels)

# Print table 
print(bobcat.model.table)
