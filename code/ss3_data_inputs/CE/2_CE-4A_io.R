rm(list = ls())

# Spatial configuration:
spat_config = '4A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source('code/auxiliary_functions.R')

# DEFINITION OF REGIONS 2010
# Note that the boundaries of Area R3 and R4 were changed for the assessment in 2010, 
# with part of Area R3 moved to Area R4
# Region 1(1)   10N, 40E-75E - Arabian Sea
# Region 1(2)   10S-10N, 40E-75E - western equatorial
# Region 1(2)   15S-10S, 60E-75E - western equatorial
# Region 2(3)   40S-10S, 35E-60E - Mzbqe Channel, excluding SE corner which is included in R4 (40S-30S, 40E-60E).
# Region 3(4)   40S-15S, 60E-120E and 40S-30S, 40E-60E; - southern Indian Ocean
# Region 4(5)   15S-20N, 75E-130E - western Indian Ocean, Bay of Bengal, Timor Sea 
#
# DEFINITION OF FISHERIES 2012 MFCL
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
# Fishery 11  Longline (LL 3)                             [region 4] 3
# Fishery 12  Gillnet (GI 4)                              [region 5] 4
# Fishery 13  Longline (LL 4)                             [region 5] 4
# Fishery 14  Other (OT 4)                                [region 5] 4
# Fishery 15  Troll (TR 4)                                [region 5] 4
# Fishery 16  Purse-seine - free schools (FS 2)           [region 3] 2
# Fishery 17  Purse-seine - log schools (LS 2)            [region 3] 2
# Fishery 18  Troll (TR 2)                                [region 3] 2
# Fishery 19  Purse-seine - free schools (FS 4)           [region 5] 4
# Fishery 20  Purse-seine - log schools (LS 4)            [region 5] 4
# Fishery 21  Longline - fresh tuna (FL 4)                [region 5] 4

fish_info = get_fisheries(spat_config)
ModelFisheries = fish_info$fleet_name

# -------------------------------------------------------------------------
# Read traditional CE data after preprocessing:
data = read.csv(file.path(shrpoint_path, 'data/processed', 'catch_grid.csv'))

# Get area information:
data$Area = get_4Aarea_from_lonlat(data$long, data$lat)
table(data$Area)
# Create area and fishery columns:
data = create_4Aarea_cols(data)
table(data$ModelArea)
# Create model fleet column:
data = data %>% 
  dplyr::mutate(ModelFishery = paste(FisheryCode, AssessmentAreaName, sep = '_')) %>% 
  dplyr::mutate(ModelFleet = as.numeric(factor(ModelFishery,levels=ModelFisheries)))
# Table modelfleet:
table(data$ModelFleet)
# Make sure no NAs:
which(is.na(data$ModelFishery))
which(is.na(data$ModelFleet))

# -------------------------------------------------------------------------
# Aggregate data:
# Remove Month, SchoolType, Grid:

agg_data = data %>% group_by(Year, Quarter, Fleet, Gear, ModelFleet, ModelFishery) %>%
  summarise(NCmtFish = sum(NCmtFish)) 
# Save for data exploration:
write.csv(agg_data, file = file.path(shrpoint_path, 'data/processed', 'agg-catch.csv'), row.names = FALSE)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# catch for SS3

work = data	%>% 
  group_by(Year,Quarter,ModelFishery) %>% 
  summarise(Catch = sum(Catch)) %>% as.data.frame() %>%	
  #spread(ModelFishery,Catch,fill=0) %>% 
  mutate(qtr = yearqtr2qtr(Year,Quarter,1950,13)) %>%
  mutate(ModelFleet = as.numeric(factor(ModelFishery,levels=ModelFisheries)))
table(work$ModelFleet)
which(is.na(work$ModelFleet))

# Format for ss3:
catch_df = work[,c('qtr', 'Quarter', 'ModelFleet', 'Catch')]
colnames(catch_df) = c('year', 'seas', 'fleet', 'catch')
catch_df = catch_df %>% mutate(catch_se = 0.01, seas = 1)
catch_df = bind_rows(tibble::tibble(year = -999, seas = 1, fleet = sort(unique(catch_df$fleet)), catch = 0, catch_se = 0.01),
                     catch_df)

# Save SS catch input
write.csv(work, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'catch.csv'), row.names = FALSE)
