rm(list = ls())

# Spatial configuration:
spat_config = '4A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))
source(here('code', 'parameters_for_plots.R'))

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

ModelFisheries <- c('GI 1a','HD 1a','LL 1a','OT 1a','BB 1b','FS 1b','LL 1b','LS 1b','TR 1b',
                    'LL 2','LL 3','GI 4','LL 4','OT 4','TR 4','FS 2','LS 2','TR 2','FS 4','LS 4','LF 4')

# -------------------------------------------------------------------------
# Read traditional LF data after preprocessing:
data = read.csv(file.path(shrpoint_path, 'data/processed', 'agesize_grid.csv'))

# Get area information:
data$Area = get_4Aarea_from_lonlat(data$Lon, data$Lat)
table(data$Area)
# Assign Area==0 to == 3
data = data %>% mutate(Area = if_else(Area == 0, 3, Area))
table(data$Area)
# Assign NAs to model area == 2 (check with team)
sum(is.na(data$Area))
data = data %>% mutate(Area = if_else(is.na(Area), 2, Area))
table(data$Area)

# Create area and fishery columns:
data = create_4Aarea_cols(data)
table(data$ModelArea)
# Create model fleet column:
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
# CAAL for SS3

work = data	%>% 
  group_by(Year,Quarter,ModelFleet,LowBin,Age_int) %>% 
  summarise(n_fish = n()) %>% 
  mutate(Yr = yearqtr2qtr(Year,Quarter,1950,13), .before = 'Year') %>% 
  spread(Age_int, n_fish, fill = 0) %>% ungroup() %>%
  select(-c('Year', 'Quarter')) %>% mutate(HighBin = LowBin, .after = 'LowBin') %>%
  mutate(Nsamp = rowSums(across(`0`:`28`)), .before = `0`)

# Save SS catch input
write.csv(work, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'caal.csv'), row.names = FALSE)
