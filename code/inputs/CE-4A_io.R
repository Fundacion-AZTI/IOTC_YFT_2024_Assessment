library(tidyverse)
library(reshape)
library(r4ss)
library(here)

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
# Region 4(5)   15S-20N, 75E-130E - western Indian Ocean, Bay of Bengal, Timor Sea 
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

# DEFINITION OF REGIONS 2010
# Note that the boundaries of Area R3 and R4 were changed for the assessment in 2010, 
# with part of Area R3 moved to Area R4
# Region 1(1)   10N, 40E-75E - Arabian Sea
# Region 1(2)   10S-10N, 40E-75E - western equatorial
# Region 1(2)   15S-10S, 60E-75E - western equatorial
# Region 2(3)   40S-10S, 35E-60E - Mzbqe Channel, excluding SE corner which is included in R4 (40S-30S, 40E-60E).
# Region 3(4)   40S-15S, 60E-120E and 40S-30S, 40E-60E; - southern Indian Ocean
# Region 4(5)   15S-20N, 75E-130E - western Indian Ocean, Bay of Bengal, Timor Sea 
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

ModelFisheries <- c('GI 1a','HD 1a','LL 1a','OT 1a','BB 1b','FS 1b','LL 1b','LS 1b','TR 1b','LL 2','LL 3','GI 4','LL 4','OT 4','TR 4','FS 2','LS 2','TR 2','FS 4','LS 4','LF 4')

Data = read.csv(file.path(shrpoint_path, 'data/raw', "IOTC-2024-WPTT26(AS) - YFT - Raised catches.csv"))

Data = Data %>% 
  transmute(YEAR,MONTH_START,FLEET_CODE,GEAR_CODE,FISHING_GROUND_CODE,CATCH_SCHOOL_TYPE_CODE,CATCH,CATCH_IN_NUMBERS) %>% 
  mutate(Year = YEAR, Month=MONTH_START, Fleet=FLEET_CODE, Gear = GEAR_CODE, SchoolType =CATCH_SCHOOL_TYPE_CODE, Grid=FISHING_GROUND_CODE, NCnoFish = CATCH_IN_NUMBERS,NCmtFish=CATCH) %>% 
  dplyr::select(Year,Month,Fleet, Gear,SchoolType,Grid,NCnoFish,NCmtFish)

dHelper = read.csv(file.path(shrpoint_path, 'data/raw', "IOTC-2024-WPTT26(AS) - YFT - Fishery mapping.csv"))
dHelper = dHelper %>% 
  mutate(Fleet=FLEET, Gear=GEAR_CODE, SchoolType=SCHOOL_TYPE_CODE,FisheryCode=FISHERY) %>% 
  dplyr::select(Fleet,Gear,SchoolType,FisheryCode)

Data = Data %>% 
  dplyr::left_join(dHelper,by=c("Gear"="Gear","Fleet"="Fleet","SchoolType"="SchoolType")) %>% 
  mutate_cond(FisheryCode=='PS',FisheryCode=paste(SchoolType)) %>% 
  dplyr::mutate(Catch = NCmtFish,Grid=as.character(Grid),Quarter = floor((Month-1)/3) + 1)

Data = plyr::ddply(Data,"Grid",.fun = function(d) {
  lat = get.lat.from.id(d$Grid[1]);
  long = get.long.from.id(d$Grid[1]);
  d$lat =lat
  d$long = long
  d$Area = 0
  d$Area =  ifelse(lat > 10 & long < 75,1,
                   ifelse((lat > -10 & lat < 10 & long  < 60) | (lat > -15 & lat < 10 & long  > 60 & long < 75),2,
                          ifelse((lat > -60 & lat < -10 & long > 20 & long < 40) | (lat > -30 & lat < -10 & long > 40 & long  < 60),3,
                                 ifelse((lat > -60 & lat < -30 & long > 40 & long < 60) | (lat > -60 & lat < -15 & long  > 60 & long < 150),4,
                                        ifelse(lat > -15 & long > 75 & long < 150,5,0)))))		
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
  dplyr::mutate(ModelFleet = as.numeric(factor(ModelFishery,levels=ModelFisheries)))

data = Data	

##################  
# catch by Fishery
##################  

# dat <- r4ss::SS_readdat(file.path('models/assessment_2021', 'data.ss'), verbose=FALSE)
# dat <- dat$catch
# dat <- dat %>%
# 		dplyr::filter(year > 0) %>%
# 		dplyr::mutate(yearqtr = qtr2yearqtr(year,1950,13))
# 
# work = data	%>% 
# 			group_by(Year,Quarter,ModelFishery) %>% 
# 			summarise(Catch = sum(Catch)) %>% 
# 			as.data.frame() %>%	
# 			dplyr::mutate(ModelFishery = factor(ModelFishery,levels=ModelFisheries)) %>%
# 			dplyr::mutate(ModelFleet = as.numeric(ModelFishery)) %>% 
# 			dplyr::mutate(YearQtr = Year + (Quarter-1)/4+1/8) %>%
# 			dplyr::arrange(ModelFleet,Year,Quarter)  %>%
# 			dplyr::select(Year,Quarter,YearQtr,ModelFishery,ModelFleet,Catch)  %>%
# 			as.data.frame()
# 
# png(filename = file.path('output/figures', 'catch_ts_compare.png'), width = 170, height = 190, units = 'mm', res = 300)
# par(mfrow=c(7,3),mar=c(2,3.1,2,1),oma=c(0,2,0,0))
# for (i in 1:length(ModelFisheries)) {
# 	plot(work[work$ModelFishery == ModelFisheries[i] ,'YearQtr'],work[work$ModelFishery == ModelFisheries[i],"Catch"],ylim=c(0,max(c(dat[dat$fleet == i ,'catch'],work[work$ModelFishery == ModelFisheries[i],"Catch"]))),type='n',xlab='',ylab='Catch');
# 	lines(dat[dat$fleet == i ,'yearqtr'],dat[dat$fleet == i ,'catch'],col='red4',lwd=2)
# 	lines(work[work$ModelFishery == ModelFisheries[i] ,'YearQtr'],work[work$ModelFishery == ModelFisheries[i],"Catch"],col='slate gray',lwd=2)
# 	mtext(paste(i,ModelFisheries[i],sep= "."),line=0.1,adj=0.5)
# }
# mtext("Catch ('000 mt)", side=2,outer=T)
# dev.off()

##################  
# catch for SS3
##################  
#  data.ss
work = data	%>% 
  group_by(Year,Quarter,ModelFishery) %>% 
  summarise(Catch = sum(Catch)) %>% as.data.frame() %>%	
  #spread(ModelFishery,Catch,fill=0) %>% 
  mutate(qtr =yearqtr2qtr(Year,Quarter,1950,13)) %>%
  mutate(ModelFleet = as.numeric(factor(ModelFishery,levels=ModelFisheries)))

# Save SS catch input
write.csv(work, file = file.path(shrpoint_path, 'data/ss_inputs', spat_config, 'catch.csv'), row.names = FALSE)
