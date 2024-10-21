rm(list = ls())

# Spatial configuration:
spat_config = '1A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source('code/auxiliary_functions.R')

# Read raw CE data after preprocessing:
data = read.csv(file.path(shrpoint_path, 'data/processed', 'catch_grid.csv'))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Aggregated subconfig:
spat_subconfig = 'agg'

# Get fisheries:
fish_info = get_fisheries(spat_config)
ModelFisheries = fish_info$fleet_name

# Get area information:
data$Area = get_1Aarea_from_lonlat(data$long, data$lat)
table(data$Area)
# Create area and fishery columns:
data = create_1Aarea_cols(data)
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
write.csv(catch_df, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'catch.csv'), row.names = FALSE)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Areas-as-fleet subconfig:
spat_subconfig = 'aaf'

# No need to produce the ss3 inputs again, just use the 4A inputs
# Remember to produce the 4A inputs beforehand
file.copy(from = file.path(shrpoint_path, 'data/ss3_inputs/4A_io', 'catch.csv'),
          to = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'catch.csv'))
