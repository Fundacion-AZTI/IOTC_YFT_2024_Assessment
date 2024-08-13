# This script will do the preprocessing of the LF data regardless the number of areas in the SS model

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))

###############
# read data from standard SF table
##############

# Follow the 4A configuration:
ModelFisheries <- c('GI 1a','HD 1a','LL 1a','OT 1a','BB 1b','FS 1b','LL 1b','LS 1b','TR 1b','LL 2','LL 3','GI 4','LL 4','OT 4','TR 4','FS 2','LS 2','TR 2','FS 4','LS 4','LF 4')

Data = read.csv(file.path(shrpoint_path, 'data/raw',  "IOTC-2024-WPTT26(AS) - YFT - Size frequencies.csv"))

Data = Data %>% 
  dplyr::select(YEAR,MONTH_START,FLEET_CODE,GEAR_CODE,FISHING_GROUND_CODE,SCHOOL_TYPE_CODE,FIRST_CLASS_LOW,SIZE_INTERVAL, NO_FISH, REPORTING_QUALITY,C001:C150) %>% 
  mutate(Year = YEAR, Month=MONTH_START, Fleet=FLEET_CODE, Gear = GEAR_CODE, SchoolType =SCHOOL_TYPE_CODE, Grid=FISHING_GROUND_CODE, TnoFish = NO_FISH, Quality= REPORTING_QUALITY,FirstClassLow=FIRST_CLASS_LOW,SizeInterval=SIZE_INTERVAL)  

dHelper = read.csv(file.path(shrpoint_path, 'data/raw', "IOTC-2024-WPTT26(AS) - YFT - Fishery mapping.csv"))
dHelper = dHelper %>% 
  mutate(Fleet=FLEET, Gear=GEAR_CODE, SchoolType=SCHOOL_TYPE_CODE,FisheryCode=FISHERY) %>% 
  dplyr::select(Fleet,Gear,SchoolType,FisheryCode)

Data = Data %>% 
  dplyr::filter(!(Gear == 'HOOK' | Gear == 'HATR' | Gear=='PSOB' | (Gear == 'PS' & SchoolType == 'UNCL'))) %>%  # I exclude HATR here but please check again if the HATR LF is good enough now
  dplyr::filter(!substring(Grid,1,1) == "9") %>%
  dplyr::left_join(dHelper,by=c("Gear"="Gear","Fleet"="Fleet","SchoolType"="SchoolType")) %>% 
  mutate_cond(FisheryCode=='PS',FisheryCode=SchoolType) %>%
  dplyr::mutate(Quarter = floor((Month-1)/3) + 1) %>%
  mutate(CellType = substring(Grid,1,1)) %>%
  dplyr::filter(CellType %in% as.character(1:6)) # Remove unclassified cell types

Data = plyr::ddply(Data, "Grid", .fun = function(d) {
  lat = get.lat.from.id(d$Grid[1]);
  long = get.long.from.id(d$Grid[1]);
  d$Lat = lat
  d$Long = long
  return(d)}
)		
Data = Data %>% dplyr::select(Year,Quarter,Month,Grid,Lat,Long,Fleet,Gear,SchoolType,FisheryCode,TnoFish,FirstClassLow,SizeInterval,REPORTING_QUALITY,C001:C150)

# Get area information:
Data$Area = get_4Aarea_from_lonlat(Data$Long, Data$Lat)
table(Data$Area)

# -------------------------------------------------------------------------

# Create area columns:
Data = create_4Aarea_cols(Data)

# Create ModelFleet column:
Data = Data %>% 
  dplyr::mutate(ModelFishery = paste(FisheryCode, AssessmentAreaName)) %>% 
  dplyr::mutate(ModelFleet = as.numeric(factor(ModelFishery,levels=ModelFisheries)))

# Select important variables:
Data = Data %>% dplyr::select(Year,Quarter,Month,Grid,Lat,Long,Fleet,Gear,SchoolType,Area,AssessmentArea,AssessmentAreaName, ModelArea,FisheryCode,ModelFishery,ModelFleet,FirstClassLow,SizeInterval,REPORTING_QUALITY,C001:C150)

# Continue..
C_labels = c(Paste("C00",1:9),Paste("C0",10:99), Paste("C",100:150))
data = Data %>% 
  dplyr::group_by(Year,Quarter,Month,Grid,Lat,Long,Fleet,Gear,SchoolType,Area,AssessmentArea,AssessmentAreaName,ModelArea,FisheryCode,ModelFishery,ModelFleet,FirstClassLow,SizeInterval,REPORTING_QUALITY) %>% 
  dplyr::summarise_at(C_labels,list(Sum)) %>%
  as.data.frame() 

data = data %>%
  dplyr::mutate(sno=rowSums(dplyr::select(data,C001:C150))) %>%
  dplyr::mutate(C095=rowSums(dplyr::select(data,C095:C150))) %>%
  dplyr::select(Year,Quarter,Month,Grid,Lat,Long,Fleet,Gear,SchoolType,Area,AssessmentArea, AssessmentAreaName, ModelArea,FisheryCode, ModelFishery,ModelFleet,FirstClassLow,SizeInterval,REPORTING_QUALITY,sno,C001:C095) %>% 
  tidyr::gather(length,total,C001:C095) %>%
  dplyr::mutate(length = FirstClassLow+(as.numeric(substr(length,2,4))-1)*SizeInterval) %>% 
  dplyr::mutate(length=ifelse(length<100,Paste("L0",length),Paste("L",length))) %>% 
  tidyr::spread(length,total,fill=0)

# Save this object for analyses:
write.csv(data, file = file.path(shrpoint_path, 'data/processed', 'size_grid.csv'), row.names = FALSE)
