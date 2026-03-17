library(dplyr)
library(tidyr)
library(tibble)

bobcat.data$SiteYear <- paste(bobcat.data$CameraTrap, bobcat.data$StudyYear, sep = "_")

bobcat.data$Detect<-ifelse(bobcat.data$NumberofBobcatCaptures >0,1,0)

View(bobcat.data)

y <- bobcat.data %>%
  select(SiteYear, DeploymentDay, Detect) %>%
  pivot_wider(
    names_from = DeploymentDay,
    values_from = Detect
  )

{bobcat.data} |>
  dplyr::summarise(n = dplyr::n(), .by = c(SiteYear, DeploymentDay)) |>
  dplyr::filter(n > 1L)

combined_bobcat_data$SiteYear <- paste(
  combined_bobcat_data$CameraTrap,
  combined_bobcat_data$StudyYear,
  sep = "_"
)
site.data <- combined_bobcat_data |>
  dplyr::distinct(SiteYear, .keep_all = TRUE) |>
  dplyr::select(
    SiteYear,
    DistancetoUrbanArea,
    Slope,
    Aspect,
    DistancetoWater,
    DistancetoRoad,
    Elevation,
    WoodyMaterial,
    StandAge,
    StandDensityIndex,
    TopographicRoughnessIndex,
    AreaBroadleafDeciduousForestBobcats,
    Area50to75PercentCanopyCoverBobcats
  )
rownames(site.data) <- site.data$SiteYear
site.data <- site.data[rownames(y), ]
site.data$SiteYear <- NULL


# Attempt 2 :D

library(dplyr)
library(tidyr)
library(unmarked)

bobcat.data <- combined_bobcat_data %>%
  mutate(
    Detect = ifelse(
      is.na(NumberofBobcatCaptures), NA,
      ifelse(NumberofBobcatCaptures > 0, 1, 0)
    )
  )

bobcat.data <- bobcat.data %>%
  group_by(CameraTrap, StudyYear, DeploymentDay) %>%
  summarise(
    Detect = max(Detect, na.rm = TRUE),
    across(
      c(DistancetoUrbanArea, Slope, Aspect, DistancetoWater,
        DistancetoRoad, Elevation, WoodyMaterial, StandAge,
        StandDensityIndex, TopographicRoughnessIndex,
        AreaBroadleafDeciduousForestBobcats,
        Area50to75PercentCanopyCoverBobcats),
      ~ first(.x)
    ),
    .groups = "drop"
  )

all_days <- 1:131

bobcat.data <- bobcat.data %>%
  group_by(CameraTrap, StudyYear) %>%
  complete(DeploymentDay = all_days) %>%
  ungroup()

bobcat.data <- bobcat.data %>%
  mutate(SiteYear = paste(CameraTrap, StudyYear, sep = "_"))

y <- bobcat.data %>%
  arrange(CameraTrap, StudyYear, DeploymentDay) %>%
  pivot_wider(
    id_cols = SiteYear,
    names_from = c(StudyYear, DeploymentDay),
    values_from = Detect
  )

y <- as.data.frame(y)
rownames(y) <- y$SiteYear
y$SiteYear <- NULL

site.data <- bobcat.data %>%
  distinct(SiteYear, .keep_all = TRUE) %>%
  select(
    SiteYear,
    DistancetoUrbanArea,
    Slope,
    Aspect,
    DistancetoWater,
    DistancetoRoad,
    Elevation,
    WoodyMaterial,
    StandAge,
    StandDensityIndex,
    TopographicRoughnessIndex,
    AreaBroadleafDeciduousForestBobcats,
    Area50to75PercentCanopyCoverBobcats
  )

site.data$SiteYear <- trimws(site.data$SiteYear)
rownames(y) <- trimws(rownames(y))
site.data <- site.data[rownames(y), ]

rownames(site.data) <- site.data$SiteYear
site.data <- site.data[rownames(y), ]
site.data$SiteYear <- NULL

all(rownames(y) == rownames(site.data))

# Tests!
print(nrow(y))
print(nrow(site.data)) # Should match

print(all(rownames(y) == rownames(site.data)))  # MUST be TRUE - Will deal with later :D
print(ncol(y) %% 5 == 0)                        # MUST be TRUE

# Checking for problems with line 127
setdiff(rownames(y), rownames(site.data))
head(rownames(y))
head(rownames(site.data))

View(site.data)

# Check each site-year has 131 days
check_days <- bobcat.data %>%
  count(CameraTrap, StudyYear)

print(table(check_days$n))  # MUST show only 131

bobcat.umf <- unmarkedMultFrame(
  y = as.matrix(y),
  siteCovs = site.data,
  numPrimary = 5
)

summary(bobcat.umf) # Summarizes data above

plot(bobcat.umf) # Supposed to plot detection vs. non-detection for bobcats 

# Next attempt to create dynamic occupancy model 

# First fit the model 
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

# Fitting models for detection probability 

# Model with detection dependent on effort 
bobcat.m1 <- colext(~1, # Occupancy constant
                    ~1 # Colonization constant 
                    ~1 # Extinction constant
                    ~effort, # Probability of detection is affected by effort
                    data = bobcat.umf) # We need to incorporate effort in somehow :(
bobcat.m2 <- colext(~1, ~1, ~1, ~feature_type, data = bobcat.umf) # Feature type also needs to be incorporated somehow :(
bobcat.m3 <- colext(~1, ~1, ~1, ~effort+feature_type, data = bobcat.umf)






