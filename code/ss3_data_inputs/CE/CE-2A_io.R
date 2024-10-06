rm(list = ls())

# Spatial configuration:
spat_config = '2A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))

# DEFINITION OF REGIONS 2010
# Note that the boundaries of Area R3 and R4 were changed for the assessment in 2010, 
# with part of Area R3 moved to Area R4
# Region 1(1)   10N, 40E-75E - Arabian Sea
# Region 1(2)   10S-10N, 40E-75E - western equatorial
# Region 1(2)   15S-10S, 60E-75E - western equatorial
# Region 1(2)   40S-10S, 35E-60E - Mzbqe Channel, excluding SE corner which is included in R4 (40S-30S, 40E-60E).
# Region 2(3)   40S-15S, 60E-120E and 40S-30S, 40E-60E; - southern Indian Ocean
# Region 2(3)   15S-20N, 75E-130E - eastern Indian Ocean, Bay of Bengal, Timor Sea 
#
# DEFINITION OF FISHERIES
# Fishery 1   Gillnet (GI 1a)                              [region 1] 1
# Fishery 2   Handline (HD 1a)                             [region 1] 1
# Fishery 3   Longline (LL 1a)                             [region 1] 1 
# Fishery 4   Other (OT 1a)                                [region 1] 1
# Fishery 5   Baitboat (BB 1b)                             [region 2] 1
# Fishery 6   Purse-seine - free schools (FS 1b)           [region 2] 1
# Fishery 7   Longline (LL 1b)                             [region 2] 1
# Fishery 8   Purse-seine - log schools (LS 1b)           [region 2] 1
# Fishery 9   Troll (TR 1b)                               [region 2] 1 
# Fishery 10  Longline (LL 2)                             [region 3] 2
# Fishery 11  Gillnet (GI 2)                              [region 3] 2
# Fishery 12  Other (OT 2)                                [region 3] 2
# Fishery 13  Troll (TR 2)                                [region 3] 2
# Fishery 14  Purse-seine - free schools (FS 2)           [region 3] 2
# Fishery 15  Purse-seine - log schools (LS 2)            [region 3] 2
# Fishery 16  Longline - fresh tuna (LF 2)                [region 3] 2

fish_info = get_fisheries(spat_config)
ModelFisheries = fish_info$fleet_name

# -------------------------------------------------------------------------
# Read raw CE data after preprocessing:
data = read.csv(file.path(shrpoint_path, 'data/processed', 'catch_grid.csv'))

# Get area information:
data$Area = get_2Aarea_from_lonlat(data$long, data$lat)
table(data$Area)
# Create area and fishery columns:
data = create_2Aarea_cols(data)
table(data$ModelArea)
data = data %>% 
  dplyr::mutate(ModelFishery = paste(FisheryCode, AssessmentAreaName)) %>% 
  dplyr::mutate(ModelFleet = as.numeric(factor(ModelFishery,levels=ModelFisheries)))
# Table modelfleet:
table(data$ModelFleet)
# Make sure no NAs:
which(is.na(data$ModelFishery))
which(is.na(data$ModelFleet))


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Prepare catch for SS3


# -------------------------------------------------------------------------
work = data	%>% 
  group_by(Year,Quarter,ModelFishery) %>% 
  summarise(Catch = sum(Catch)) %>% as.data.frame() %>%	
  #spread(ModelFishery,Catch,fill=0) %>% 
  mutate(qtr = yearqtr2qtr(Year,Quarter,1950,13)) %>%
  mutate(ModelFleet = as.numeric(factor(ModelFishery,levels=ModelFisheries)))
table(work$ModelFleet)

# Save SS catch input
write.csv(work, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'catch.csv'), row.names = FALSE)

