# Spatial configuration:
spat_config = '4A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'inputs', 'auxiliary_functions.R'))

# DEFINITION OF REGIONS 2010
# Note that the boundaries of Area R3 and R4 were changed for the assessment in 2010, 
# with part of Area R3 moved to Area R4
# Region 1(1)   10N, 40E-75E - Arabian Sea
# Region 1(2)   10S-10N, 40E-75E - western equatorial
# Region 1(2)   15S-10S, 60E-75E - western equatorial
# Region 2(3)   40S-10S, 35E-60E - Mzbqe Channel, excluding SE corner which is included in R4 (40S-30S, 40E-60E).
# Region 3(4)   40S-15S, 60E-120E and 40S-30S, 40E-60E; - southern Indian Ocean
# Region 4(5)   15S-20N, 75E-130E - eastern Indian Ocean, Bay of Bengal, Timor Sea 
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

# Initial length bins (wrong) in IOTC dataset:
# This was an error in the 2021, which did not include half of the length bins. Confirmed by Dan
L_labels_wrong  =  c(Paste("L0",seq(10,98,4)), Paste("L",seq(102,198,4)))

# Initial length bins (correct) in IOTC dataset:
L_labels  =  c(Paste("L0",seq(10,98,2)), Paste("L",seq(100,198,2))) 

# Length bins in the SS model:
L_labels_SS  =  c(Paste("L0",seq(10,98,4)), Paste("L",seq(102,198,4)))

# -------------------------------------------------------------------------
# Get LF input with bug, without weighting and Nsamp 5 ------------------------------

# Read preprocessed data (see LF-preprocessing):
data = read.csv(file.path(shrpoint_path, 'data/processed', 'size_grid.csv'))

# Filter data based on some criteria:
work = filter_LF_4A(data) 
# Continue..
work = work %>%
  dplyr::group_by(ModelArea,ModelFleet,Year,Quarter) %>% 
  dplyr::summarise_at(L_labels_wrong,list(Sum)) %>%
  as.data.frame() %>%
  tidyr::gather(length,total,L010:L198) %>%
  dplyr::mutate(length=as.numeric(substr(length,2,4))) %>%
  dplyr::mutate(length=length-(length-10) %% 4) %>%
  dplyr::mutate(length=ifelse(length<100,Paste("L0",length),Paste("L",length))) %>%
  dplyr::group_by(ModelArea,ModelFleet,Year,Quarter,length) %>% 
  dplyr::summarise_at("total",list(Sum)) %>% 
  tidyr::spread(length,total,fill=0) %>% 
  as.data.frame()

work = work %>%
  dplyr::mutate(sno=rowSums(dplyr::select(work,L010:L198))) %>%
  dplyr::filter(sno >= 20) %>%	
  dplyr::mutate(Yr = yearqtr2qtr(Year,Quarter,1950,13), Seas = 1,Gender=0,Part=0,Nsamp = 5) %>%
  dplyr::select(Yr,Seas,ModelFleet,Gender,Part,Nsamp,L010:L198) %>%
  dplyr::arrange(ModelFleet,Yr)		
work[,L_labels_SS] = round(work[,L_labels_SS],1)

# Save SS catch input
write.csv(work, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size-bug.csv'), row.names = FALSE)


# -------------------------------------------------------------------------
# Get LF input without bug, without weighting and Nsamp 5 ------------------------------

# Read preprocessed data (see LF-preprocessing):
data = read.csv(file.path(shrpoint_path, 'data/processed', 'size_grid.csv'))

# Filter data based on some criteria:
work = filter_LF_4A(data) 
# Continue..
work = work %>%
  dplyr::group_by(ModelArea,ModelFleet,Year,Quarter) %>% 
  dplyr::summarise_at(L_labels,list(Sum)) %>%
  as.data.frame() %>%
  tidyr::gather(length,total,L010:L198) %>%
  dplyr::mutate(length=as.numeric(substr(length,2,4))) %>%
  dplyr::mutate(length=length-(length-10) %% 4) %>%
  dplyr::mutate(length=ifelse(length<100,Paste("L0",length),Paste("L",length))) %>%
  dplyr::group_by(ModelArea,ModelFleet,Year,Quarter,length) %>% 
  dplyr::summarise_at("total",list(Sum)) %>% 
  tidyr::spread(length,total,fill=0) %>% 
  as.data.frame()

work = work %>%
  dplyr::mutate(sno=rowSums(dplyr::select(work,L010:L198))) %>%
  dplyr::filter(sno >= 20) %>%	
  dplyr::mutate(Yr = yearqtr2qtr(Year,Quarter,1950,13), Seas = 1,Gender=0,Part=0,Nsamp = 5) %>%
  dplyr::select(Yr,Seas,ModelFleet,Gender,Part,Nsamp,L010:L198) %>%
  dplyr::arrange(ModelFleet,Yr)		
work[,L_labels_SS] = round(work[,L_labels_SS],1)

# Save SS catch input
write.csv(work, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size.csv'), row.names = FALSE)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Get LF input without bug, without weighting and Nsamp from catch-weighted Rep Quality ---------
# You will need to run the make_grid.R script before running the following lines

load(file.path(shrpoint_path, 'data/processed', 'mergedStd_5.RData'))
data = mergedStd
# Change columns names to make it work:
colnames(data) = str_to_title(colnames(data))
colnames(data)[c(6:9)] = c('FisheryCode', 'ModelArea', 'ModelFleet', 'ModelFishery')

# Filter data based on some criteria:
work = filter_LF_4A(data) 
# 1. Do the aggregation for length bins (traditional way):
work1 = work %>%
  dplyr::group_by(ModelArea,ModelFleet,Year,Quarter) %>% 
  dplyr::summarise_at(L_labels,list(Sum)) %>%
  as.data.frame() %>%
  tidyr::gather(length,total,L010:L198) %>%
  dplyr::mutate(length=as.numeric(substr(length,2,4))) %>%
  dplyr::mutate(length=length-(length-10) %% 4) %>%
  dplyr::mutate(length=ifelse(length<100,Paste("L0",length),Paste("L",length))) %>%
  dplyr::group_by(ModelArea,ModelFleet,Year,Quarter,length) %>% 
  dplyr::summarise_at("total",list(Sum)) %>% 
  tidyr::spread(length,total,fill=0) %>% 
  as.data.frame()
# 2. Do the aggregation for reporting quality (weighted mean)
work2 = work %>%
  dplyr::group_by(ModelArea,ModelFleet,Year,Quarter) %>%
  summarise(RepQual = weighted.mean(Reporting_quality, Ncnofish))
# Merge both:
work = left_join(work1, work2)
work = work %>% relocate(RepQual, .after = Quarter) 

# Apply last filters to have the same data:
work = work %>%
  # dplyr::mutate(sno=rowSums(dplyr::select(work,L010:L198))) %>%
  # dplyr::filter(sno >= 20) %>%	# No need to apply this filter since we are using RepQuality. Confirm with team
  dplyr::mutate(Yr = yearqtr2qtr(Year,Quarter,1950,13), Seas = 1,Gender=0,Part=0) %>%
  dplyr::select(Yr,Seas,ModelFleet,Gender,Part,RepQual,L010:L198) %>%
  dplyr::arrange(ModelFleet,Yr)		
work[,L_labels_SS] = round(work[,L_labels_SS],1)

# Save SS catch input
write.csv(work, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size_RQ-w.csv'), row.names = FALSE)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Get LF input without bug, weighting LF and Rep Quality ---------
# You will need to run the make_grid.R script before running the following lines

load(file.path(shrpoint_path, 'data/processed', 'mergedStd_5.RData'))
data = mergedStd
# Change columns names to make it work:
colnames(data) = str_to_title(colnames(data))
colnames(data)[c(6:9)] = c('FisheryCode', 'ModelArea', 'ModelFleet', 'ModelFishery')

# Filter data based on some criteria:
work = filter_LF_4A(data) 
# 1. Do the aggregation for length bins (traditional way):
work1 = work %>% ungroup() %>%
  dplyr::mutate(Samp_sum = rowSums(across(all_of(L_labels)))) %>% # First calculate row sum 
  dplyr::mutate(across(all_of(L_labels))/Samp_sum) %>% # then get proportions by row
  dplyr::mutate(across(all_of(L_labels), ~ .x*Ncnofish)) %>% # then weight by catch in numbers
  dplyr::group_by(ModelArea,ModelFleet,Year,Quarter) %>% 
  dplyr::summarise_at(L_labels,list(Sum)) %>%
  as.data.frame() %>%
  tidyr::gather(length,total,L010:L198) %>%
  dplyr::mutate(length=as.numeric(substr(length,2,4))) %>%
  dplyr::mutate(length=length-(length-10) %% 4) %>%
  dplyr::mutate(length=ifelse(length<100,Paste("L0",length),Paste("L",length))) %>%
  dplyr::group_by(ModelArea,ModelFleet,Year,Quarter,length) %>% 
  dplyr::summarise_at("total",list(Sum)) %>% 
  tidyr::spread(length,total,fill=0) %>% 
  as.data.frame()
# 2. Do the aggregation for reporting quality (weighted mean)
work2 = work %>%
  dplyr::group_by(ModelArea,ModelFleet,Year,Quarter) %>%
  summarise(RepQual = weighted.mean(Reporting_quality, Ncnofish))
# Merge both:
work = left_join(work1, work2)
work = work %>% relocate(RepQual, .after = Quarter) 

# Apply last filters to have the same data:
work = work %>%
  # dplyr::mutate(sno=rowSums(dplyr::select(work,L010:L198))) %>%
  # dplyr::filter(sno >= 20) %>%	# No need to apply this filter since we are using RepQuality. Confirm with team
  dplyr::mutate(Yr = yearqtr2qtr(Year,Quarter,1950,13), Seas = 1,Gender=0,Part=0) %>%
  dplyr::select(Yr,Seas,ModelFleet,Gender,Part,RepQual,L010:L198) %>%
  dplyr::arrange(ModelFleet,Yr)		
work[,L_labels_SS] = round(work[,L_labels_SS],1)

# Save SS catch input
write.csv(work, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size_LF-w_RQ-w.csv'), row.names = FALSE)
