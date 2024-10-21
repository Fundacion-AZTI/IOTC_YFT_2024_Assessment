rm(list = ls())

# Spatial configuration:
spat_config = '1A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source('code/auxiliary_functions.R')

# Initial length bins (correct) in IOTC dataset:
L_labels  =  c(Paste("L0",seq(10,98,2)), Paste("L",seq(100,198,2))) 

# Length bins in the SS model:
L_labels_SS  =  c(Paste("L0",seq(10,98,4)), Paste("L",seq(102,198,4)))

# Read original LF data after preprocessing:
data = read.csv(file.path(shrpoint_path, 'data/processed', 'size_grid-original.csv'))

# Read cwp55 LF data after preprocessing:
# You will need to run the LF-CE-cwp55-merge.R script before running the following lines
load(file.path(shrpoint_path, 'data/processed', 'mergedStd_5.RData'))
data_std = mergedStd
# Change columns names to make it work:
colnames(data_std) = str_to_title(colnames(data_std))
colnames(data_std)[c(6)] = c('FisheryCode')

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Aggregated subconfig:
spat_subconfig = 'agg'

#Fishery definiton
fish_info = get_fisheries(spat_config)
ModelFisheries = fish_info$fleet_name


# Original dataset --------------------------------------------------------
# Get area information:
data$Area = get_1Aarea_from_lonlat(data$Long, data$Lat)
table(data$Area)
# Create area columns:
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

# Aggregate data (both, simple and std):
# Remove Month, SchoolType, Grid:
agg_data = data %>% group_by(Year, Quarter, Fleet, Gear, ModelFleet, ModelFishery) %>%
  summarise_at('Quality', list(mean)) %>%
  inner_join(data %>% group_by(Year, Quarter, Fleet, Gear, ModelFleet, ModelFishery) %>%
               summarise_at(c('Nfish_samp', L_labels), list(sum)))

# Filter data based on some criteria (original dataset):
work = filter_LF_1A(agg_data) 
# 1. Do the aggregation for length bins (traditional way):
work1 = work %>%
  dplyr::group_by(ModelFleet,Year,Quarter) %>% 
  dplyr::summarise_at(L_labels,list(Sum)) %>%
  as.data.frame() %>%
  tidyr::gather(length,total,L010:L198) %>%
  dplyr::mutate(length=as.numeric(substr(length,2,4))) %>%
  dplyr::mutate(length=length-(length-10) %% 4) %>%
  dplyr::mutate(length=ifelse(length<100,Paste("L0",length),Paste("L",length))) %>%
  dplyr::group_by(ModelFleet,Year,Quarter,length) %>% 
  dplyr::summarise_at("total",list(Sum)) %>% 
  tidyr::spread(length,total,fill=0) %>% 
  as.data.frame()
# 2. Do the aggregation for reporting quality (simple mean)
work2 = work %>%
  dplyr::group_by(ModelFleet,Year,Quarter) %>%
  summarise(RepQual = mean(Quality))
# Merge both:
work = left_join(work1, work2)
work = work %>% relocate(RepQual, .after = Quarter) 

work = work %>%
  dplyr::mutate(Yr = yearqtr2qtr(Year,Quarter,1950,13), Seas = 1,Gender=0,Part=0,Nsamp = RepQual) %>%
  dplyr::select(Yr,Seas,ModelFleet,Gender,Part,Nsamp,L010:L198) %>%
  dplyr::arrange(ModelFleet,Yr)		
work[,L_labels_SS] = round(work[,L_labels_SS],1)

#Format for ss3:
size_df = work %>% dplyr::rename(FltSvy = ModelFleet)

# Save SS catch input
write.csv(size_df, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'size-original.csv'), row.names = FALSE)


# cwp55 dataset -----------------------------------------------------------
# Get area information:
data_std$Area = get_1Aarea_from_lonlat(data_std$Lon, data_std$Lat)
table(data_std$Area)
data_std = create_1Aarea_cols(data_std)
table(data_std$ModelArea)
# Create ModelFleet column again:
data_std = data_std %>% 
  dplyr::mutate(ModelFishery = paste(FisheryCode, AssessmentAreaName, sep = '_')) %>% 
  dplyr::mutate(ModelFleet = as.numeric(factor(ModelFishery,levels=ModelFisheries)))
# Table modelfleet:
table(data_std$ModelFleet)
# Make sure no NAs:
which(is.na(data_std$ModelFishery))
which(is.na(data_std$ModelFleet))

# Aggregate data (both, simple and std):
# Remove Month, SchoolType, Grid:
agg_data_std = data_std %>% group_by(Year, Quarter, Fleet, Gear, ModelFleet, ModelFishery) %>%
  summarise_at('Quality', list(mean)) %>%
  inner_join(data_std %>% group_by(Year, Quarter, Fleet, Gear, ModelFleet, ModelFishery) %>%
               summarise_at(c('Nfish_samp', 'Ncnofish', L_labels), list(sum)))

# Filter data based on some criteria (cwp55 dataset):
work = filter_LF_1A(agg_data_std) 
# 1. Do the aggregation for length bins (weighted by catch in numbers):
work1 = work %>% 
  dplyr::mutate(across(all_of(L_labels))/Nfish_samp) %>% # then get proportions by row
  dplyr::mutate(across(all_of(L_labels), ~ .x*Ncnofish)) %>% # then weight by catch in numbers
  dplyr::group_by(ModelFleet,Year,Quarter) %>% 
  dplyr::summarise_at(L_labels,list(Sum)) %>%
  as.data.frame() %>%
  tidyr::gather(length,total,L010:L198) %>%
  dplyr::mutate(length=as.numeric(substr(length,2,4))) %>%
  dplyr::mutate(length=length-(length-10) %% 4) %>%
  dplyr::mutate(length=ifelse(length<100,Paste("L0",length),Paste("L",length))) %>%
  dplyr::group_by(ModelFleet,Year,Quarter,length) %>% 
  dplyr::summarise_at("total",list(Sum)) %>% 
  tidyr::spread(length,total,fill=0) %>% 
  as.data.frame()
# 2. Do the aggregation for reporting quality (weighted mean)
work2 = work %>%
  dplyr::group_by(ModelFleet,Year,Quarter) %>%
  summarise(RepQual = weighted.mean(Quality, Ncnofish))
# Merge both:
work = left_join(work1, work2)
work = work %>% relocate(RepQual, .after = Quarter) 

# Apply last filters to have the same data:
work = work %>%
  dplyr::mutate(Yr = yearqtr2qtr(Year,Quarter,1950,13), Seas = 1,Gender=0,Part=0,Nsamp = RepQual) %>%
  dplyr::select(Yr,Seas,ModelFleet,Gender,Part,Nsamp,L010:L198) %>%
  dplyr::arrange(ModelFleet,Yr)		
work[,L_labels_SS] = round(work[,L_labels_SS],1)

#Format for ss3:
size_df = work %>% dplyr::rename(FltSvy = ModelFleet)

# Save SS catch input
write.csv(size_df, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'size-cwp55.csv'), row.names = FALSE)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Areas-as-fleets subconfig:
spat_subconfig = 'aaf'

# No need to produce the ss3 inputs again, just use the 4A inputs
# Remember to produce the 4A inputs beforehand
file.copy(from = file.path(shrpoint_path, 'data/ss3_inputs/4A_io', 'size-original.csv'),
          to = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'size-original.csv'))
file.copy(from = file.path(shrpoint_path, 'data/ss3_inputs/4A_io', 'size-cwp55.csv'),
          to = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'size-cwp55.csv'))
