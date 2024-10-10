rm(list = ls())

# Spatial configuration:
spat_config = '2A_io'
spat_subconfig = 'agg'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))

#Fishery definiton
fish_info = get_fisheries(spat_config)
ModelFisheries = fish_info$fleet_name

# Initial length bins (correct) in IOTC dataset:
L_labels  =  c(Paste("L0",seq(10,98,2)), Paste("L",seq(100,198,2))) 

# Length bins in the SS model:
L_labels_SS  =  c(Paste("L0",seq(10,98,4)), Paste("L",seq(102,198,4)))


# -------------------------------------------------------------------------
# Read original LF data after preprocessing:
data = read.csv(file.path(shrpoint_path, 'data/processed', 'size_grid-original.csv'))

# Get area information:
data$Area = get_2Aarea_from_lonlat(data$Long, data$Lat)
table(data$Area)
# Create area columns:
data = create_2Aarea_cols(data)
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
# Read spatially standardized data:
# You will need to run the make_grid.R script before running the following lines
load(file.path(shrpoint_path, 'data/processed', 'mergedStd_5.RData'))
data_std = mergedStd
# Change columns names to make it work:
colnames(data_std) = str_to_title(colnames(data_std))
colnames(data_std)[c(6)] = c('FisheryCode')
# Update area information since grids info has changed:
data_std$Area = get_2Aarea_from_lonlat(data_std$Lon, data_std$Lat)
table(data_std$Area)
data_std = create_2Aarea_cols(data_std)
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

# -------------------------------------------------------------------------
# Aggregate data (both, simple and std):
# Remove Month, SchoolType, Grid:

agg_data = data %>% group_by(Year, Quarter, Fleet, Gear, ModelFleet, ModelFishery) %>%
  summarise_at('Quality', list(mean)) %>%
  inner_join(data %>% group_by(Year, Quarter, Fleet, Gear, ModelFleet, ModelFishery) %>%
               summarise_at(c('Nfish_samp', L_labels), list(sum)))

agg_data_std = data_std %>% group_by(Year, Quarter, Fleet, Gear, ModelFleet, ModelFishery) %>%
  summarise_at('Quality', list(mean)) %>%
  inner_join(data_std %>% group_by(Year, Quarter, Fleet, Gear, ModelFleet, ModelFishery) %>%
               summarise_at(c('Nfish_samp', 'Ncnofish', L_labels), list(sum)))

# -------------------------------------------------------------------------
# Get LF input, simple aggregation, Nsamp is RQ ---------

# Filter data based on some criteria:
work = filter_LF_2A(agg_data) 
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


# -------------------------------------------------------------------------
# Get LF input, weighting LF and Rep Quality ---------

# Filter data based on some criteria:
work = filter_LF_2A(agg_data_std) 
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
