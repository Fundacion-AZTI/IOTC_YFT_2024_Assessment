# This script will do the preprocessing of the LF data regardless the number of areas in the SS model
rm(list = ls())

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source('code/auxiliary_functions.R')

# Define raw size filenames:
raw_filenames = c("IOTC-2024-WPTT26(AS) - YFT - SF frequencies (original).csv", 
                  "IOTC-2024-WPTT26(AS) - YFT - SF frequencies (cwp55).csv")

# Define output filenames (respect same order as before):
output_filenames = c('size_grid-original.csv', 'size_grid-cwp55.csv')

###############
# read data from standard SF table
##############

for(i in seq_along(raw_filenames)) {

  # Read data:
  Data = read_csv(file.path(shrpoint_path, 'data/raw',  raw_filenames[i]))
  head(Data)
  
  # Some checks (grid categories):
  table(substring(Data$FISHING_GROUND_CODE, 1, 1))
  # Data %>% group_by(CLASS_LOW) %>% summarise(nfish = sum(FISH_COUNT))
  
  # Filtering:
  Data = Data %>% 
    dplyr::select(YEAR,MONTH_START,FLEET_CODE,GEAR_CODE,FISHING_GROUND_CODE,SCHOOL_TYPE_CODE,CLASS_LOW,CLASS_HIGH,REPORTING_QUALITY,FISH_COUNT) %>% 
    mutate(Year = YEAR, Month=MONTH_START, Fleet=FLEET_CODE, Gear = GEAR_CODE, SchoolType =SCHOOL_TYPE_CODE, Grid=FISHING_GROUND_CODE,LowBin=CLASS_LOW,HighBin=CLASS_HIGH,Quality=REPORTING_QUALITY,Nfish=FISH_COUNT)  
  
  dHelper = read.csv(file.path(shrpoint_path, 'data/raw', "IOTC-2024-WPTT26(AS) - YFT - Fishery mapping_Rev1.csv"))
  dHelper = dHelper %>% 
    mutate(Fleet=FLEET, Gear=GEAR_CODE, SchoolType=SCHOOL_TYPE_CODE,FisheryCode=FISHERY) %>% 
    dplyr::select(Fleet,Gear,SchoolType,FisheryCode)
  
  Data = Data %>% 
    dplyr::filter(!(Gear == 'HOOK' | Gear == 'HATR' | Gear=='PSOB' | (Gear == 'PS' & SchoolType == 'UNCL'))) %>%  # I exclude HATR here but please check again if the HATR LF is good enough now
    dplyr::filter(!(Grid == 'NJA_SYC')) %>%
    dplyr::left_join(dHelper,by=c("Gear"="Gear","Fleet"="Fleet","SchoolType"="SchoolType")) %>% 
    mutate_cond(FisheryCode=='PS',FisheryCode=SchoolType) %>%
    dplyr::mutate(Quarter = floor((Month-1)/3) + 1)
  
  # Check:
  table(substring(Data$Grid, 1, 1))
  which(is.na(Data$FisheryCode))
  
  # Continue..
  Data = plyr::ddply(Data, "Grid", .fun = function(d) {
    lat = get.lat.from.id(d$Grid[1]);
    long = get.long.from.id(d$Grid[1]);
    d$Lat = lat
    d$Long = long
    return(d)}
  )		
  
  # Select..
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
  write.csv(out_data, file = file.path(shrpoint_path, 'data/processed', output_filenames[i]), row.names = FALSE)
  cat(output_filenames[i], " written", "\n")
  
}
