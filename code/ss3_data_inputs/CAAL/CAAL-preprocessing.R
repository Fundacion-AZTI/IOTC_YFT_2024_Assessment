rm(list = ls())

# Spatial configuration:
spat_config = '4A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))

# -------------------------------------------------------------------------
# Read age-size data:
age_data = read.csv(file.path(shrpoint_path, 'data/raw', 'age-size.csv'), sep = ';')
age_data = age_data %>% dplyr::select(Month, Year, Fishing.Gear, Lat, Lon, Sex, FL, Age, Age_type)
age_data$Sex[age_data$Sex == ''] = 'U'

# Explore variables: 
dim(age_data)
summary(age_data)
sum(age_data$Fishing.Gear == "") # some missing fleet information

# Remove rows without age and fleet information
age_data = age_data %>% dplyr::filter(!is.na(Age), !(Fishing.Gear == ""))
dim(age_data)

# Explora spatial extent:
range(age_data$Lon, na.rm = T)
range(age_data$Lat, na.rm = T)

# Create quarter column:
age_data = age_data %>% mutate(Quarter = floor((Month-1)/3) + 1)
table(age_data$Quarter)

# Rename some fishing gears:
table(age_data$Fishing.Gear)
age_data = age_data %>% mutate(FisheryCode = if_else(Fishing.Gear == 'PS', 'LS', Fishing.Gear)) # rename all PS by LS
age_data = age_data %>% mutate(FisheryCode = if_else(FisheryCode == 'LS' & FL > 80, 'FS', FisheryCode))
table(age_data$FisheryCode)

# Create length bin column:
range(age_data$FL)
age_data = age_data %>% mutate(LowBin = cut(FL, breaks = seq(from = 10, to = 198, by = 4), right = FALSE,
                                            labels = seq(from = 10, to = 194, by = 4)))
table(age_data$LowBin)

# Create Age column:
max_age = 10 # plus group in model
age_data = age_data %>% mutate(Age_1 = if_else(Age >= max_age, max_age, Age)) # specify 7 to fish older than 7
age_data = age_data %>% mutate(Age_int = cut(Age_1, breaks = seq(from = 0, to = max_age + 0.25, by = 0.25), right = FALSE,
                                             labels = seq(from = 0, to = max_age*4, by = 1)))
table(age_data$Age_int)
sum(is.na(age_data$Age_int))

# Final preprocessed data:
age_data = age_data %>% select(Year, Quarter, Lon, Lat, FisheryCode, LowBin, Age_int)

# Save this object for analyses:
write.csv(age_data, file = file.path(shrpoint_path, 'data/processed', 'agesize_grid.csv'), row.names = FALSE)
