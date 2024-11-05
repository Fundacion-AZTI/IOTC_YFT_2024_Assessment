rm(list = ls())

# Spatial configuration:
spat_config = '4A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))

#Fishery definiton
fish_info = get_fisheries(spat_config)
ModelFisheries = fish_info$fleet_name

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
  dplyr::mutate(ModelFishery = paste(FisheryCode, AssessmentAreaName, sep = '_')) %>% 
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
  mutate(Nsamp = rowSums(across(`0`:`40`)), .before = `0`)

# Format for ss3:
caal_df = work %>% dplyr::rename(FltSvy = ModelFleet, Lbin_lo = LowBin, Lbin_hi = HighBin)
caal_df = caal_df %>% mutate(Seas = 1, .after = Yr)
caal_df = caal_df %>% mutate(Gender = 0, Part = 0, Ageerr = 1, .after = FltSvy)

# Save SS catch input
write.csv(caal_df, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'caal.csv'), row.names = FALSE)
