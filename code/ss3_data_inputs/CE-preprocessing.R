# This script will do the preprocessing of the CE data regardless the number of areas in the SS model
rm(list = ls())

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

Data = read.csv(file.path(shrpoint_path, 'data/raw', "IOTC-2024-WPTT26(AS) - YFT - Raised catches.csv"))

Data = Data %>% 
  transmute(YEAR,MONTH_START,FLEET_CODE,GEAR_CODE,FISHING_GROUND_CODE,CATCH_SCHOOL_TYPE_CODE,CATCH,CATCH_IN_NUMBERS) %>% 
  mutate(Year = YEAR, Month=MONTH_START, Fleet=FLEET_CODE, Gear = GEAR_CODE, SchoolType =CATCH_SCHOOL_TYPE_CODE, Grid=FISHING_GROUND_CODE, NCnoFish = CATCH_IN_NUMBERS,NCmtFish=CATCH) %>% 
  dplyr::select(Year,Month,Fleet, Gear,SchoolType,Grid,NCnoFish,NCmtFish)

dHelper = read.csv(file.path(shrpoint_path, 'data/raw', "IOTC-2024-WPTT26(AS) - YFT - Fishery mapping_Rev1.csv"))
dHelper = dHelper %>% 
  mutate(Fleet=FLEET, Gear=GEAR_CODE, SchoolType=SCHOOL_TYPE_CODE,FisheryCode=FISHERY) %>% 
  dplyr::select(Fleet,Gear,SchoolType,FisheryCode)

Data = Data %>% 
  dplyr::left_join(dHelper,by=c("Gear"="Gear","Fleet"="Fleet","SchoolType"="SchoolType")) %>% 
  mutate_cond(FisheryCode=='PS',FisheryCode=paste(SchoolType)) %>% 
  dplyr::mutate(Catch = NCmtFish,Grid=as.character(Grid),Quarter = floor((Month-1)/3) + 1)

# Check:
table(substring(Data$Grid, 1, 1))
which(is.na(Data$FisheryCode))

Data = plyr::ddply(Data,"Grid",.fun = function(d) {
  lat = get.lat.from.id(d$Grid[1]);
  long = get.long.from.id(d$Grid[1]);
  d$lat =lat
  d$long = long
  return(d)}
)

# Check fishery mapping:
which(is.na(Data$FisheryCode))

# Save this object for analyses:
write.csv(Data, file = file.path(shrpoint_path, 'data/processed', 'catch_grid.csv'), row.names = FALSE)
