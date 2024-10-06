rm(list = ls())

# Spatial configuration:
spat_config = '1A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))

#Fishery definiton
fish_info = read.csv(file.path('code/ss3_data_inputs', paste0('FisheryDefinitions_', spat_config, '.csv')), sep = ';')
ModelFisheries = fish_info$fleet_name

# -------------------------------------------------------------------------
# Read traditional LF data after preprocessing:
data = read.csv(file.path(shrpoint_path, 'data/processed', 'agesize_grid.csv'))

# Get area information:
data$Area = get_1Aarea_from_lonlat(data$Lon, data$Lat)
table(data$Area)
# Assign Area==0 to == 2
data = data %>% mutate(Area = if_else(Area == 0, 2, Area))
table(data$Area)
# Assign NAs to model area == 2 (check with team)
sum(is.na(data$Area))
data = data %>% mutate(Area = if_else(is.na(Area), 2, Area))
table(data$Area)

# Create area and fishery columns:
data = create_1Aarea_cols(data)
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
