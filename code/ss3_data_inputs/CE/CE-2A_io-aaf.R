rm(list = ls())

# Spatial configuration:
spat_config = '2A_io'
spat_subconfig = 'aaf'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))

# DEFINITION OF REGIONS 2010
# Same fishery definition as 4A model:

fish_info = get_fisheries('4A_io') 
ModelFisheries = fish_info$fleet_name

# -------------------------------------------------------------------------
# Read raw CE data after preprocessing:
data = read.csv(file.path(shrpoint_path, 'data/processed', 'catch_grid.csv'))

# Get area information:
data$Area = get_4Aarea_from_lonlat(data$long, data$lat)
table(data$Area)
# Create area and fishery columns:
data = create_4Aarea_cols(data)
table(data$ModelArea)
data = data %>% 
  dplyr::mutate(ModelFishery = paste(FisheryCode, AssessmentAreaName, sep = '_')) %>% 
  dplyr::mutate(ModelFleet = as.numeric(factor(ModelFishery,levels=ModelFisheries)))
# Table modelfleet:
table(data$ModelFleet)
# Make sure no NAs:
which(is.na(data$ModelFishery))
which(is.na(data$ModelFleet))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Prepare catch for SS3

work = data	%>% 
  group_by(Year,Quarter,ModelFishery) %>% 
  summarise(Catch = sum(Catch)) %>% as.data.frame() %>%	
  mutate(qtr = yearqtr2qtr(Year,Quarter,1950,13)) %>%
  mutate(ModelFleet = as.numeric(factor(ModelFishery,levels=ModelFisheries)))
# Format for ss3:
catch_df = work[,c('qtr', 'Quarter', 'ModelFleet', 'Catch')]
colnames(catch_df) = c('year', 'seas', 'fleet', 'catch')
catch_df = catch_df %>% mutate(catch_se = 0.01, seas = 1)
catch_df = bind_rows(tibble::tibble(year = -999, seas = 1, fleet = sort(unique(catch_df$fleet)), catch = 0, catch_se = 0.01),
                     catch_df)

# Save SS catch input
write.csv(work, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'catch.csv'), row.names = FALSE)
