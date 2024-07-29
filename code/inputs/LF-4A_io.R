# Spatial configuration:
spat_config = '4A_io'

# Set working directiry using here():
proj_dir = here()
setwd(proj_dir)

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

###############
# read data from standard SF table
##############

ModelFisheries <- c('GI 1a','HD 1a','LL 1a','OT 1a','BB 1b','FS 1b','LL 1b','LS 1b','TR 1b','LL 2','LL 3','GI 4','LL 4','OT 4','TR 4','FS 2','LS 2','TR 2','FS 4','LS 4','LF 4')

Data = read.csv(file.path(shrpoint_path, 'data/raw',  "IOTC-2024-WPTT26(AS) - YFT - Size frequencies.csv"))

Data = Data %>% 
  dplyr::select(YEAR,MONTH_START,FLEET_CODE,GEAR_CODE,FISHING_GROUND_CODE,SCHOOL_TYPE_CODE,FIRST_CLASS_LOW,SIZE_INTERVAL, NO_FISH, REPORTING_QUALITY,C001:C150) %>% 
  mutate(Year = YEAR, Month=MONTH_START, Fleet=FLEET_CODE, Gear = GEAR_CODE, SchoolType =SCHOOL_TYPE_CODE, Grid=FISHING_GROUND_CODE, TnoFish = NO_FISH, Quality= REPORTING_QUALITY,FirstClassLow=FIRST_CLASS_LOW,SizeInterval=SIZE_INTERVAL)  


dHelper = read.csv(file.path(shrpoint_path, 'data/raw', "IOTC-2024-WPTT26(AS) - YFT - Fishery mapping.csv"))
dHelper = dHelper %>% 
  mutate(Fleet=FLEET, Gear=GEAR_CODE, SchoolType=SCHOOL_TYPE_CODE,FisheryCode=FISHERY) %>% 
  dplyr::select(Fleet,Gear,SchoolType,FisheryCode)

# Assign FLL to LF in mapping table (mistake, confirmed by Dan):
dHelper = dHelper %>% mutate_cond(FisheryCode == 'FLL', FisheryCode = 'LF')


Data = Data %>% 
  dplyr::filter(!(Gear == 'HOOK' | Gear == 'HATR' | Gear=='PSOB' | (Gear == 'PS' & SchoolType == 'UNCL'))) %>%  # I exclude HATR here but please check again if the HATR LF is good enough now
  dplyr::filter(!substring(Grid,1,1) == "9") %>%
  dplyr::left_join(dHelper,by=c("Gear"="Gear","Fleet"="Fleet","SchoolType"="SchoolType")) %>% 
  mutate_cond(FisheryCode=='PS',FisheryCode=SchoolType) %>%
  dplyr::mutate(Quarter = floor((Month-1)/3) + 1) %>%
  dplyr::mutate(LatCell = 0, LongCell = 0) %>%
  mutate_cond(substring(Grid,1,1) == "5",LatCell=1,LongCell=1) %>%
  mutate_cond(substring(Grid,1,1) == "6",LatCell=5,LongCell=5) %>%
  mutate_cond(substring(Grid,1,1) == "1",LatCell=30,LongCell=30) %>% # originally, it was 5x10, but based on new metadata, should be 30x30 (confirmed by Dan).
  mutate_cond(substring(Grid,1,1) == "2",LatCell=10,LongCell=20) %>%
  mutate_cond(substring(Grid,1,1) == "3",LatCell=10,LongCell=10) %>%
  mutate_cond(substring(Grid,1,1) == "4",LatCell=20,LongCell=20) %>%
  dplyr::filter(!(LatCell == 0 & LongCell == 0)) %>%
  dplyr::mutate(quadrant = as.numeric(substring(Grid,2,2)), Lat=999,Long=999)  %>%
  mutate_cond(quadrant ==1,Lat= as.numeric(substring(Grid,3,4)) + LatCell/2, Long = as.numeric(substring(Grid,5,7)) + LongCell/2) %>%
  mutate_cond(quadrant ==2,Lat= -as.numeric(substring(Grid,3,4)) - LatCell/2, Long = as.numeric(substring(Grid,5,7)) + LongCell/2) %>%
  dplyr::select(Year,Quarter,Month,Grid,Lat,Long,Fleet,Gear,SchoolType,FisheryCode,TnoFish,FirstClassLow,SizeInterval,REPORTING_QUALITY,C001:C150)

# Check this, should we assign everything to Area 1 (Long 75)
Data = plyr::ddply(Data,"Grid",.fun = function(d) {
  Lat = d$Lat[1]
  Long = d$Long[1]
  d$Area = 0
  d$Area =  ifelse(Lat > 10 & Long <= 75, 1, # originally, it was Long < 75, but produced unassiged areas for GI after GridCell correction
                   ifelse((Lat > -10 & Lat < 10 & Long  < 60) | (Lat > -15 & Lat < 10 & Long  > 60 & Long <= 75), 2, # originally, it was Long < 75, but produced unassiged areas for GI after GridCell correction
                          ifelse((Lat > -60 & Lat < -10 & Long > 20 & Long < 40) | (Lat > -30 & Lat < -10 & Long > 40 & Long  < 60), 3,
                                 ifelse((Lat > -60 & Lat < -30 & Long > 40 & Long < 60) | (Lat > -60 & Lat <= -15 & Long  > 60 & Long <= 150), 4,
                                        ifelse(Lat > -15 & Long > 75 & Long < 150, 5, 0)))))		
  return(d)})					

Data = plyr::ddply(Data,c("Area","FisheryCode"),.fun = function(d) {
  d$AssessmentArea = d$Area
  d = mutate_cond(d,FisheryCode=='FS' & Area ==1, AssessmentArea = 2)
  d = mutate_cond(d,FisheryCode=='FS' & Area ==4, AssessmentArea = 5)
  d = mutate_cond(d,FisheryCode=='LS' & Area ==1, AssessmentArea = 2)
  d = mutate_cond(d,FisheryCode=='LS' & Area ==4, AssessmentArea = 5)
  d = mutate_cond(d,FisheryCode=='LF', AssessmentArea = 5)
  d = mutate_cond(d,FisheryCode=='BB', AssessmentArea = 2)
  d = mutate_cond(d,FisheryCode=='GI' & (Area == 2 | Area==3), AssessmentArea = 1)
  d = mutate_cond(d,FisheryCode=='GI' & Area == 4, AssessmentArea = 5)
  d = mutate_cond(d,FisheryCode=='HD', AssessmentArea = 1)
  d = mutate_cond(d,FisheryCode=='TR' & Area == 1, AssessmentArea = 2)
  d = mutate_cond(d,FisheryCode=='TR' & Area == 4, AssessmentArea = 5)
  d = mutate_cond(d,FisheryCode=='OT' & (Area == 2 | Area==3), AssessmentArea = 1)
  d = mutate_cond(d,FisheryCode=='OT' & Area == 4, AssessmentArea = 5)
  return(d)})	

Data = plyr::ddply(Data,c("Area","FisheryCode"),.fun = function(d) {
  d$AssessmentAreaName = d$AssessmentArea
  d$ModelArea = d$AssessmentArea
  d = mutate_cond(d,AssessmentArea=='1',AssessmentAreaName = '1a')
  d = mutate_cond(d,AssessmentArea=='2',AssessmentAreaName = '1b')
  d = mutate_cond(d,AssessmentArea=='3',AssessmentAreaName = '2')
  d = mutate_cond(d,AssessmentArea=='4',AssessmentAreaName = '3')
  d = mutate_cond(d,AssessmentArea=='5',AssessmentAreaName = '4')
  d = mutate_cond(d,AssessmentArea=='1',ModelArea = '1')
  d = mutate_cond(d,AssessmentArea=='2',ModelArea = '1')
  d = mutate_cond(d,AssessmentArea=='3',ModelArea = '2')
  d = mutate_cond(d,AssessmentArea=='4',ModelArea = '3')
  d = mutate_cond(d,AssessmentArea=='5',ModelArea = '4')	
  return(d)})

Data = Data %>% 
  dplyr::mutate(ModelFishery = paste(FisheryCode, AssessmentAreaName)) %>% 
  dplyr::mutate(ModelFleet = as.numeric(factor(ModelFishery,levels=ModelFisheries))) %>% 
  dplyr::select(Year,Quarter,Month,Grid,Lat,Long,Fleet,Gear,SchoolType,Area,AssessmentArea,AssessmentAreaName, ModelArea,FisheryCode,ModelFishery,ModelFleet,FirstClassLow,SizeInterval,REPORTING_QUALITY,C001:C150)

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

###############
# data output to ss3 
###############

L_labels  =  c(Paste("L0",seq(10,98,4)), Paste("L",seq(102,198,4)))
work = data %>% 
  dplyr::filter(!(Fleet %in% c('TWN','SYC') & Gear == 'LL')) %>%
  dplyr::filter(!(ModelFishery == "LL 1a" & Year %in% c(1970:1995, 2010:2020))) %>% # please check if 2020-2022 data needs to be excluded
  dplyr::filter(!(ModelFishery == "LL 1b" & Year %in% c(1950:1959))) %>%
  dplyr::filter(!(ModelFishery == "LL 2" & Year %in% c(1950:1959))) %>%
  dplyr::filter(!(ModelFishery == "LL 3" & Year %in% c(1950:1959))) %>%
  dplyr::filter(!(ModelFishery == "LL 4" & Year %in% c(1950:1959,2001:2005,2015,2019))) %>%  # please check if 2020-2022 data needs to be excluded
  dplyr::filter(!(ModelFishery == "GI 4" & Year %in% c(1975:1987))) %>%
  dplyr::filter(!(ModelFishery == "HD 1a" & Year %in% c(1950:2007))) %>%
  dplyr::filter(!(ModelFishery == "OT 4" & Year %in% c(1983,2016))) %>%
  dplyr::filter(!(ModelFishery == "TR 4" & Year %in% c(2016:2019))) %>%  # please check if 2020-2022 data needs to be excluded
  dplyr::filter(!(ModelFishery == "TR 1b")) %>%
  dplyr::filter(!(ModelFishery == "TR 2")) 

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
  dplyr::mutate(Yr =  (13-1)+4*(Year-1950)+ Quarter, Seas = 1,Gender=0,Part=0,Nsamp = 5) %>%
  dplyr::select(Yr,Seas,ModelFleet,Gender,Part,Nsamp,L010:L198) %>%
  dplyr::arrange(ModelFleet,Yr)		
L_labels  =  c(Paste("L0",seq(10,98,4)), Paste("L",seq(102,198,4)))
work[,L_labels] = round(work[,L_labels],1)

# Save SS catch input
write.csv(work, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size.csv'), row.names = FALSE)
