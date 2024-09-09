#
#   READ TAGGING DATA AND TRANSFORM INTO 
#   SS3 FORMAT FROM LENGTH TO AGE
#.....................................

library(tidyverse)
library(reshape)
library(readxl)
library(here)
library(dplyr)
# 
# ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
# ✖ scales::col_factor() masks readr::col_factor()
# ✖ scales::discard()    masks purrr::discard()
# ✖ tidyr::expand()      masks reshape::expand()
# ✖ stats::filter()      masks dplyr::filter()
# ✖ stats::lag()         masks dplyr::lag()
# ✖ reshape::rename()    masks dplyr::rename()
# ✖ lubridate::stamp()   masks reshape::stamp()
# ℹ Use the conflicted package to force all conflicts to become errors
# Warning messages:
#   1: package ‘tidyverse’ was built under R version 4.2.3 
# 2: package ‘tibble’ was built under R version 4.2.3 
# 3: package ‘forcats’ was built under R version 4.2.3 
# 4: package ‘lubridate’ was built under R version 4.2.3 		
source('code/auxiliary_functions2.R')
source('sharepoint_path.R')
setwd(shrpoint_path)
attach('data/ss3_inputs/tag/tag.Rdata')




###########
# Data input
###########

 

Data =  transmute(Data,TAG_TagSeries, TAG_Tag1, TAG_Tag2,TAG_Project,TAG_Vessel,TAG_Sp,TAG_Length, TAG_Gear,SIG_Date,SIG_LatDegree,SIG_LatMinute,SIG_LonDegree,SIG_LonMinute,
				  REC_TagSeries,REC_ID, REC_Length,REC_Sp,REC_Country,REC_DateFound, REC_DateCatch,REC_DateReturn,REC_Timestamp,REC_LatDegree,REC_LatMinute,REC_LonDegree,REC_LonMinute,REC_Gear, CAL_SET_ShoolType, REC_WhereFound, 
				  CAL_REC_FL,CAL_REC_DATE_AVG,CAL_SIG_LAT,CAL_SIG_LON,CAL_REC_LON,CAL_REC_LAT,AvgOfCAL_REC_LON,AvgOfCAL_REC_LAT, AvgOfCAL_RECSET_LON, AvgOfCAL_RECSET_LAT,REC_EditStatus) %>% 
		#filter(TAG_Sp == 'Y' & (is.na(REC_Sp) | REC_Sp=='Y')) %>%
		dplyr::filter (!is.na(TAG_Length)) %>%
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

###########
# Data processing
###########
data = Data 

# Assign release region
data = data %>% 
	 mutate(rel_assessment_area = 0) %>% 
	 mutate(rel_assessment_area = replace(rel_assessment_area, which(rel_lat > 10 & rel_long <= 75),1))  %>% 
	 mutate(rel_assessment_area = replace(rel_assessment_area, which(rel_lat <= 10 & rel_lat >= -10 & rel_long <= 75),2)) %>% 
	 mutate(rel_assessment_area = replace(rel_assessment_area, which(rel_lat < -10 & rel_long <= 75),3)) %>% 
	 mutate(rel_assessment_area = replace(rel_assessment_area, which(rel_long > 75),5)) %>% 	 
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

# Assign release age  - simple slicing	
Len <- c(23.821,34.9831,43.1626,46.936,49.9479,52.8725,58.2097,68.1843,77.6614,87.5578,98.6306,106.399,114.182,121.998,125.114,127.895,130.377,132.592,134.569,136.334,137.908,139.314,140.568,141.688,142.687,143.578,144.374,146.017)  # Fon


LenMid <- c(10, (Len[-28] + (Len[-1] - Len[-28])/2))
data = plyr::ddply(data,'tag_length',.fun=function(d,L) {
		l = d[,'tag_length'][1]
		age = 0
		for(i in 1:(length(L)-1)) age <- ifelse(l >= L[i] & l < L[i+1], i, age)
		if(l >= L[length(L)]) age = length(L) 
		#age = ifelse(age>=15,15,age)
		d$rel_age= age
		d},LenMid)

			
# recapture data
dataC <- data %>% dplyr::filter(!is.na(rec_id) & !is.na(rec_yrqtr))
dataC = dataC %>% 
	 mutate(rec_assessment_area = 0) %>% 
	 mutate(rec_assessment_area = replace(rec_assessment_area, which(rec_lat >= 10 & rec_long < 75),1))  %>% 
	 mutate(rec_assessment_area = replace(rec_assessment_area, which(rec_lat < 10 & rec_lat >= -15  & rec_long < 75),2)) %>% 
	 mutate(rec_assessment_area = replace(rec_assessment_area, which(rec_lat < -10  & rec_lat > -40 & rec_long < 60 & rec_long > 20),3)) %>% 
	 mutate(rec_assessment_area = replace(rec_assessment_area, which(rec_lat >= -40 & rec_lat <= -15  & rec_long >60 & rec_long < 120),4)) %>% 
	 mutate(rec_assessment_area = replace(rec_assessment_area, which(rec_lat >= -15  & rec_long >= 75 & rec_long < 120),5)) %>% 
	 mutate(rec_assessment_area = replace(rec_assessment_area, which(rec_assessment_area == 0),2)) %>% 
	 mutate(rec_assessment_area = replace(rec_assessment_area, which(rec_assessment_area == 1 & rec_gear=='Purse seine'),2))  %>%
	 mutate(rec_assessment_area = replace(rec_assessment_area, which(rec_assessment_area %in% c(2,5) & rec_gear=='Gillnet'),1))  %>%
	 mutate(rec_assessment_area = replace(rec_assessment_area, which(rec_assessment_area %in% c(2,5) & rec_gear=='Handline'),1))  %>%
	 mutate(rec_assessment_area = replace(rec_assessment_area, which(rec_assessment_area ==1 & rec_gear=='Troll line'),2))  %>%
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
#tapply(dataC$rec_fleet,list(dataC$rec_fleet, dataC$rec_region), length)	
dataC = dataC %>% dplyr::filter(rec_model_fleet !=0)




############
# SS3 output
############

	
# Step 2 (TagNewProc) - age determined from simple slicing
# updated prossing: initial tag mortality 10%
# updated prossing: seperate reporting rate adjustment for at sea and seychelles recovery
# updated prossing: reporting rate adjument for outside seychlles recovery using esimated proportion of EU PS landings in Seychelles
		
work = data %>% 
	dplyr::filter(project=='RTTP')  %>%
	mutate(rel_age = replace(rel_age,which(rel_age>15),15)) %>%
	group_by(rel_assessment_area, rel_yrqtr,rel_year,rel_quarter,rel_age)  %>%
	summarise(number=n())  %>% 
	as.data.frame() %>%
	mutate(rel_yr=yearqtr2qtr(rel_year,rel_quarter,1950,13),season=1,tfill=999,gender=0,tag = 1:n())
work = work %>% mutate(number_prime = round(number * 0.725,1))
workC = dataC %>% 
	dplyr::filter(project=='RTTP')  %>%
	mutate(rel_age = replace(rel_age,which(rel_age>15),15)) %>%
	group_by(rel_assessment_area, rel_year,rel_quarter,rel_age, rec_year,rec_quarter,rec_model_fleet,rec_location)  %>%
	summarise(number=n(),number_prime=n())  %>% 
	as.data.frame() %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2005 & rec_quarter==1,number_prime = round(1/0.816*number/0.57,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2005 & rec_quarter==2,number_prime = round(1/0.621*number/0.57,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2005 & rec_quarter==3,number_prime = round(1/0.908*number/0.57,1))  %>% 	
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2005 & rec_quarter==4,number_prime = round(1/0.951*number/0.57,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2006 & rec_quarter==1,number_prime = round(1/0.921*number/0.61,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2006 & rec_quarter==2,number_prime = round(1/0.896*number/0.84,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2006 & rec_quarter==3,number_prime = round(1/0.979*number/0.89,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2006 & rec_quarter==4,number_prime = round(1/0.958*number/0.91,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2007 & rec_quarter==1,number_prime = round(1/0.934*number/0.93,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2007 & rec_quarter==2,number_prime = round(1/0.563*number/0.91,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2007 & rec_quarter==3,number_prime = round(1/0.975*number/0.91,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2007 & rec_quarter==4,number_prime = round(1/0.985*number/0.88,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2008 & rec_quarter==1,number_prime = round(1/0.877*number/0.88,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2008 & rec_quarter==2,number_prime = round(1/0.631*number/0.91,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2008 & rec_quarter==3,number_prime = round(1/0.953*number/0.94,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2008 & rec_quarter==4,number_prime = round(1/0.981*number/0.94,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2009 & rec_quarter==1,number_prime = round(1/0.518*number/0.94,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2009 & rec_quarter==2,number_prime = round(1/0.584*number/0.94,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2009 & rec_quarter==3,number_prime = round(1/0.956*number/0.94,1))  %>% 
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year==2009 & rec_quarter==4,number_prime = round(1/0.966*number/0.94,1))  %>% 	
	mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8,16,17) & rec_year > 2009,number_prime = round(1.1*number/0.94,1))  %>% 
	dplyr::filter(!((rec_location =='' | rec_location =='OTH') & rec_model_fleet %in% c(6,8,16,17)))  %>%
	group_by(rel_assessment_area, rel_year,rel_quarter,rel_age, rec_year,rec_quarter,rec_model_fleet)  %>%
	summarise(number=sum(number),number_prime=sum(number_prime))  %>%
	as.data.frame() %>%
	mutate(rec_yr=yearqtr2qtr(rec_year,rec_quarter,1950,13),season=1) %>% 
	left_join(select(work,c(rel_assessment_area,rel_year,rel_quarter,rel_age,tag)),by=c('rel_assessment_area','rel_year','rel_quarter','rel_age'))
	
tmp = work %>% select(tag,rel_assessment_area,rel_yr,season, tfill,gender,rel_age,number_prime) 
temp = workC %>% select(tag,rec_yr,season,rec_model_fleet,number_prime)
	
save(tmp,temp, file="data/ss3_inputs/tag/tag_ss3_input_30_08.RData")
	