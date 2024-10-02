# This script will do the preprocessing of the CE data regardless the number of areas in the SS model
rm(list = ls())

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))


# -------------------------------------------------------------------------
# Read data in:
Data = read.csv(file.path(shrpoint_path, 'data/raw', "tag.csv"))

# Some processing:
Data =  transmute(Data,TAG_TagSeries, TAG_Tag1, TAG_Tag2,TAG_Project,TAG_Vessel,TAG_Sp,TAG_Length, TAG_Gear,SIG_Date,SIG_LatDegree,SIG_LatMinute,SIG_LonDegree,SIG_LonMinute,
                  REC_TagSeries,REC_ID, REC_Length,REC_Sp,REC_Country,REC_DateFound, REC_DateCatch,REC_DateReturn,REC_Timestamp,REC_LatDegree,REC_LatMinute,REC_LonDegree,REC_LonMinute,REC_Gear, CAL_SET_ShoolType, REC_WhereFound, 
                  CAL_REC_FL,CAL_REC_DATE_AVG,CAL_SIG_LAT,CAL_SIG_LON,CAL_REC_LON,CAL_REC_LAT,AvgOfCAL_REC_LON,AvgOfCAL_REC_LAT, AvgOfCAL_RECSET_LON, AvgOfCAL_RECSET_LAT,REC_EditStatus) %>% 
  #filter(TAG_Sp == 'Y' & (is.na(REC_Sp) | REC_Sp=='Y')) %>%
  dplyr::filter(!is.na(TAG_Length)) %>%
  mutate(rel_date = as.Date(as.character(SIG_Date),format="%Y-%m-%d")) %>%		
  mutate(rel_year = as.numeric(format(rel_date,'%Y')),rel_month = as.numeric(format(rel_date,'%m')), rel_quarter=month2qrt(rel_month),rel_yrqtr=yearqtr2numeric(rel_year,rel_quarter)) %>% 
  mutate(rel_long = CAL_SIG_LON,rel_lat = CAL_SIG_LAT) %>% 
  mutate(rec_date = as.Date(as.character(CAL_REC_DATE_AVG),format="%Y-%m-%d")) %>% 
  mutate(rec_year = as.numeric(format(rec_date,'%Y')),rec_month = as.numeric(format(rec_date,'%m')), rec_quarter=month2qrt(rec_month),rec_yrqtr=yearqtr2numeric(rec_year,rec_quarter)) %>% 
  mutate() %>%
  mutate(rec_long = CAL_REC_LON) %>% 
  mutate_cond(is.na(rec_long),rec_long = AvgOfCAL_REC_LON, rec_long) %>%
  mutate_cond(is.na(rec_long),rec_long = AvgOfCAL_RECSET_LON, rec_long) %>%
  mutate_cond(is.na(rec_long),rec_long = CAL_SIG_LON, rec_long) %>%
  mutate(rec_lat = CAL_REC_LAT) %>% 
  mutate_cond(is.na(rec_lat),rec_lat = AvgOfCAL_REC_LAT,rec_lat) %>%
  mutate_cond(is.na(rec_lat),rec_lat = AvgOfCAL_RECSET_LAT,rec_lat) %>%
  mutate_cond(is.na(rec_lat),rec_lat = CAL_SIG_LAT,rec_lat) %>%
  mutate(rec_location=trim(as.character(REC_Country))) %>%
  mutate(rec_location = replace(rec_location, which(is.na(rec_location)),''))  %>%
  mutate(rec_location = replace(rec_location,which(rec_location=='At sea'),'SEA')) %>%
  mutate(rec_location = replace(rec_location,which(rec_location=='Seychelles' | rec_location=='SEYCHELLES'),'SEZ')) %>%	
  mutate(rec_location = replace(rec_location,which(rec_location!='SEA' & rec_location!='SEZ' & rec_location!=''),'OTH')) %>%			
  mutate(	tagseries=TAG_TagSeries,project=TAG_Project,tag_length=TAG_Length, rel_length=round(TAG_Length),rec_id=REC_ID, rec_length=REC_Length, rec_gear=trim(REC_Gear), rec_schooltype=CAL_SET_ShoolType, rec_editstatus = REC_EditStatus) %>%
  select(tagseries,project,tag_length,rel_length,SIG_Date,rel_date,rel_year,rel_month,rel_quarter,rel_yrqtr,rel_long,rel_lat,
         rec_id,rec_length,rec_gear,rec_schooltype,rec_location,rec_date,rec_year,rec_month, rec_quarter,rec_yrqtr,rec_long,rec_lat,rec_editstatus)

# Continue..
data = Data 

# Assign release region
data$rel_assessment_area = get_4Aarea_from_lonlat(data$rel_long, data$rel_lat)
# Do some area reassignment:
data$rel_assessment_area[data$rel_assessment_area %in% 4 & data$rel_long > 75] = 5
data$rel_assessment_area[data$rel_assessment_area %in% c(0,4)] = 3
table(data$rel_assessment_area)

# Continue..
data = data %>%  
  mutate(rel_grp = paste(rel_assessment_area,rel_yrqtr)) %>% 
  mutate(rel_assessment_area_name = rel_assessment_area, rel_model_area=rel_assessment_area)  %>%
  mutate(rel_assessment_area_name = replace(rel_assessment_area_name,which(rel_assessment_area_name==1),'1a')) %>%
  mutate(rel_assessment_area_name = replace(rel_assessment_area_name,which(rel_assessment_area_name==2),'1b')) %>%
  mutate(rel_assessment_area_name = replace(rel_assessment_area_name,which(rel_assessment_area_name==3),'2')) %>%
  mutate(rel_assessment_area_name = replace(rel_assessment_area_name,which(rel_assessment_area_name==4),'3')) %>%
  mutate(rel_assessment_area_name = replace(rel_assessment_area_name,which(rel_assessment_area_name==5),'4')) %>%
  mutate(rel_model_area = replace(rel_model_area,which(rel_model_area==1),1)) %>%	
  mutate(rel_model_area = replace(rel_model_area,which(rel_model_area==2),1)) %>%	
  mutate(rel_model_area = replace(rel_model_area,which(rel_model_area==3),2)) %>%	
  mutate(rel_model_area = replace(rel_model_area,which(rel_model_area==4),3)) %>%	
  mutate(rel_model_area = replace(rel_model_area,which(rel_model_area==5),4)) 


# -------------------------------------------------------------------------
# Now organize recapture data:
# recapture data
dataC = data %>% dplyr::filter(!is.na(rec_id) & !is.na(rec_yrqtr))

# Assign release region
dataC$rec_assessment_area = get_4Aarea_from_lonlat(dataC$rec_long, dataC$rec_lat)
# Do some area reassignment:
# This will need to be reviewed in the future, some small and probably irrelevant mistakes when assigning areas. 
# We are following Dan's steps
dataC$rec_assessment_area[dataC$rec_assessment_area %in% 0 & dataC$rec_long <= 60 & dataC$rec_lat >= -10] = 2 # following Dan's code
dataC$rec_assessment_area[dataC$rec_assessment_area %in% 0 & dataC$rec_long <= 40 & dataC$rec_lat < -10 & dataC$rec_lat >= -30] = 3 # following Dan's code

# Continue..
dataC = dataC %>%
  mutate(rec_assessment_area = replace(rec_assessment_area, which(rec_assessment_area == 0),2)) %>% 
  mutate(rec_assessment_area = replace(rec_assessment_area, which(rec_assessment_area == 1 & rec_gear=='Purse seine'),2))  %>%
  mutate(rec_assessment_area = replace(rec_assessment_area, which(rec_assessment_area %in% c(2,5) & rec_gear=='Gillnet'),1))  %>%
  mutate(rec_assessment_area = replace(rec_assessment_area, which(rec_assessment_area %in% c(2,5) & rec_gear=='Handline'),1))  %>%
  mutate(rec_assessment_area = replace(rec_assessment_area, which(rec_assessment_area ==1 & rec_gear=='Troll line'),2))

# Assign area label
dataC = dataC %>%     
  mutate(rec_assessment_area_name = rec_assessment_area, rec_model_area=rec_assessment_area)  %>%
  mutate(rec_assessment_area_name = replace(rec_assessment_area_name,which(rec_assessment_area_name==1),'1a')) %>%
  mutate(rec_assessment_area_name = replace(rec_assessment_area_name,which(rec_assessment_area_name==2),'1b')) %>%
  mutate(rec_assessment_area_name = replace(rec_assessment_area_name,which(rec_assessment_area_name==3),'2')) %>%
  mutate(rec_assessment_area_name = replace(rec_assessment_area_name,which(rec_assessment_area_name==4),'3')) %>%
  mutate(rec_assessment_area_name = replace(rec_assessment_area_name,which(rec_assessment_area_name==5),'4')) %>%
  mutate(rec_model_area = replace(rec_model_area,which(rec_model_area==1),1)) %>%	
  mutate(rec_model_area = replace(rec_model_area,which(rec_model_area==2),1)) %>%	
  mutate(rec_model_area = replace(rec_model_area,which(rec_model_area==3),2)) %>%	
  mutate(rec_model_area = replace(rec_model_area,which(rec_model_area==4),3)) %>%	
  mutate(rec_model_area = replace(rec_model_area,which(rec_model_area==5),4)) %>%	
  mutate(days_liberty = as.numeric(rec_date-rel_date))	 

# Some more changes:
dataC = dataC %>% ## LL
  mutate(rec_model_fleet=0) %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_gear =='Longline' & rec_assessment_area==1),'3'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_gear =='Longline' & rec_assessment_area==2),'7'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_gear =='Longline' & rec_assessment_area==3),'10'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_gear =='Longline' & rec_assessment_area==4),'11'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_gear =='Longline' & rec_assessment_area==5),'13'))  %>%    
  ## PS 2
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_gear =='Purse seine' & rec_assessment_area==2 & rec_yrqtr < 2007 & rec_schooltype=='BO' ),'8'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_gear =='Purse seine' & rec_assessment_area==2 & rec_yrqtr < 2007 & rec_schooltype=='BL' ),'6'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_gear =='Purse seine' & rec_assessment_area==2 & rec_yrqtr < 2007 & rec_schooltype=='mixed'),'8'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_gear =='Purse seine' & rec_assessment_area==2 & rec_yrqtr < 2007 & rec_schooltype=='IND'),'8'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_gear =='Purse seine' & rec_assessment_area==2 & rec_yrqtr >= 2007 & rec_schooltype=='BO'),'8'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_gear =='Purse seine' & rec_assessment_area==2 & rec_yrqtr >= 2007 & rec_schooltype=='BL'),'6'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==2 & rec_yrqtr >= 2007 & rec_schooltype=='mixed' & tag_length > 80),'6'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==2 & rec_yrqtr >= 2007 & rec_schooltype=='IND' & tag_length > 80),'6'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==2 & rec_yrqtr >= 2007 & rec_schooltype=='mixed' & days_liberty < 550),'8'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==2 & rec_yrqtr >= 2007 & rec_schooltype=='IND' & days_liberty < 550),'8'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==2 & rec_yrqtr >= 2007 & rec_schooltype=='mixed' & days_liberty >= 550),'6'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==2 & rec_yrqtr >= 2007 & rec_schooltype=='IND' & days_liberty >= 550),'6')) %>% 
  ## PS 3
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_gear =='Purse seine' & rec_assessment_area==3  & rec_schooltype=='BO' ),'17'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_gear =='Purse seine' & rec_assessment_area==3 & rec_schooltype=='BL' ),'16'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==3 & rec_schooltype=='mixed' & tag_length > 80),'16'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==3 & rec_schooltype=='IND' & tag_length > 80),'16'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==3 & rec_schooltype=='mixed' & days_liberty < 550),'17'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==3 & rec_schooltype=='IND' & days_liberty < 550),'17'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==3 & rec_schooltype=='mixed' & days_liberty >= 550),'16'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==3 & rec_schooltype=='IND' & days_liberty >= 550),'16')) %>% 
  ## PS 5
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_gear =='Purse seine' & rec_assessment_area==5  & rec_schooltype=='BO' ),'20'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_gear =='Purse seine' & rec_assessment_area==5 & rec_schooltype=='BL' ),'19'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==5 & rec_schooltype=='mixed' & tag_length > 80),'19'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==5 & rec_schooltype=='IND' & tag_length > 80),'19'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==5 & rec_schooltype=='mixed' & days_liberty < 550),'20'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==5 & rec_schooltype=='IND' & days_liberty < 550),'20'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==5 & rec_schooltype=='mixed' & days_liberty >= 550),'19'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Purse seine' & rec_assessment_area==5 & rec_schooltype=='IND' & days_liberty >= 550),'19'))  %>% 
  ## GI HD and TR
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Gillnet' & rec_assessment_area==1),'1'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Handline' & rec_assessment_area==1),'2'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_assessment_area==1),'4'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Pole line' & rec_assessment_area==2),'5'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Troll line' & rec_assessment_area==2),'9'))  %>% 		
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Troll line' & rec_assessment_area==3),'18'))  %>% 
  mutate(rec_model_fleet = replace(rec_model_fleet, which(rec_model_fleet == 0 & rec_gear =='Handline' & rec_assessment_area==3),'18'))  	

# Remove unassigned fleets:
dataC = dataC %>% dplyr::filter(rec_model_fleet !=0)

# Save processed data:
write.csv(data, file = file.path(shrpoint_path, 'data/processed', 'tag_release.csv'), row.names = FALSE)
write.csv(dataC, file = file.path(shrpoint_path, 'data/processed', 'tag_recapture.csv'), row.names = FALSE)
