# This script will do the preprocessing of the LF data regardless the number of areas in the SS model
rm(list = ls())

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))

###############
# read data from standard SF table
##############

# Follow the 4A configuration:
ModelFisheries <- c('GI 1a','HD 1a','LL 1a','OT 1a','BB 1b','FS 1b','LL 1b','LS 1b','TR 1b','LL 2','LL 3','GI 4','LL 4','OT 4','TR 4','FS 2','LS 2','TR 2','FS 4','LS 4','LF 4')

# Read size datasets:
Data_1 = read.csv(file.path(shrpoint_path, 'data/raw',  "YFT_STD_SF_WPTT26_REGULAR_TO_CWP55_GRIDS.csv"))
Data_2 = read.csv(file.path(shrpoint_path, 'data/raw',  "YFT_STD_SF_WPTT26_NON_REGULAR_TO_CWP55_GRIDS.csv"))

# Merge both:
Data = bind_rows(Data_1, Data_2)

# Some checks (all 5x5 grids):
table(substring(Data$FISHING_GROUND_CODE, 1, 1))

# Filtering:
Data = Data %>% 
  dplyr::select(YEAR,MONTH_START,FLEET_CODE,GEAR_CODE,FISHING_GROUND_CODE,SCHOOL_TYPE_CODE,CLASS_LOW,CLASS_HIGH,REPORTING_QUALITY,FISH_COUNT) %>% 
  mutate(Year = YEAR, Month=MONTH_START, Fleet=FLEET_CODE, Gear = GEAR_CODE, SchoolType =SCHOOL_TYPE_CODE, Grid=FISHING_GROUND_CODE,LowBin=CLASS_LOW,HighBin=CLASS_HIGH,Quality=REPORTING_QUALITY,Nfish=FISH_COUNT)  

dHelper = read.csv(file.path(shrpoint_path, 'data/raw', "IOTC-2024-WPTT26(AS) - YFT - Fishery mapping.csv"))
dHelper = dHelper %>% 
  mutate(Fleet=FLEET, Gear=GEAR_CODE, SchoolType=SCHOOL_TYPE_CODE,FisheryCode=FISHERY) %>% 
  dplyr::select(Fleet,Gear,SchoolType,FisheryCode)

Data = Data %>% 
  dplyr::filter(!(Gear == 'HOOK' | Gear == 'HATR' | Gear=='PSOB' | (Gear == 'PS' & SchoolType == 'UNCL'))) %>%  # I exclude HATR here but please check again if the HATR LF is good enough now
  dplyr::left_join(dHelper,by=c("Gear"="Gear","Fleet"="Fleet","SchoolType"="SchoolType")) %>% 
  dplyr::mutate(Quarter = floor((Month-1)/3) + 1) %>%
  mutate(CellType = substring(Grid,1,1)) 

# Check fishery mapping:
which(is.na(Data$FisheryCode))

Data = plyr::ddply(Data, "Grid", .fun = function(d) {
  lat = get.lat.from.id(d$Grid[1]);
  long = get.long.from.id(d$Grid[1]);
  d$Lat = lat
  d$Long = long
  return(d)}
)

# Continue..
Data = Data %>% dplyr::select(Year,Quarter,Month,Grid,Lat,Long,Fleet,Gear,SchoolType,FisheryCode,LowBin,Quality,Nfish)

# Aggregate:
Data = Data %>% 
  dplyr::group_by(Year,Quarter,Month,Grid,Lat,Long,Fleet,Gear,SchoolType,FisheryCode,LowBin) %>% 
  dplyr::summarise(Nfish = sum(Nfish),Quality=mean(Quality)) 

# Now aggregate largest and smallest length bin:
out_data = Data %>% 
  dplyr::mutate(LenBin=if_else(LowBin<198,LowBin,198)) %>% # aggregate largest bins
  dplyr::mutate(LenBin=if_else(LenBin>10,LenBin,10)) %>% # aggregate largest bins
  group_by(Year,Quarter,Month,Grid,Lat,Long,Fleet,Gear,SchoolType,FisheryCode,LenBin) %>% 
  dplyr::summarise(Nfish = sum(Nfish),Quality=mean(Quality))

# Make sure we have all length bins:
identical(sort(unique(out_data$LenBin)), seq(from = 10, to = 198, by = 2))

# Now change the format to have everything in the original format (see irregular code):
out_data = out_data %>% mutate(LenBin = if_else(LenBin<100, paste0('L0', LenBin), paste0('L', LenBin)))
out_data = out_data %>% tidyr::spread(LenBin,Nfish,fill=0)
out_data = out_data %>% mutate(Nfish_samp=rowSums(across(L010:L198)), .before = 'L010')

# Save this object for analyses:
write.csv(out_data, file = file.path(shrpoint_path, 'data/processed', 'size_grid-regular.csv'), row.names = FALSE)
