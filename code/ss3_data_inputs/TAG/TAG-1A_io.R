rm(list = ls())

# Spatial configuration:
spat_config = '1A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source('code/auxiliary_functions.R')

# Read data in:
data = read.csv(file.path(shrpoint_path, 'data/processed', 'tag_release.csv'))
dataC = read.csv(file.path(shrpoint_path, 'data/processed', 'tag_recapture.csv'))

# Age slicing (using Farley growth)
data$rel_age = sapply(data$tag_length, age_slicing, mlen_at_age = Len_farl)
dataC$rel_age = sapply(dataC$tag_length, age_slicing, mlen_at_age = Len_farl)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Produce release SS3 inputs: 

# Reassign areas:
data = data %>% mutate(rel_model_area = 1)
data = data %>% mutate(rel_assessment_area = if_else(rel_assessment_area %in% c(2,3,5), 2, 1))
data = data %>% mutate(rel_assessment_area_name = if_else(rel_assessment_area_name %in% c('1b','2','4'), '1b', '1a'))

# Check new areas:
table(data$rel_model_area)
table(data$rel_assessment_area)
table(data$rel_assessment_area_name)

# Release data input
work = data %>% 
  dplyr::filter(project=='RTTP')  %>%
  mutate(rel_age = replace(rel_age,which(rel_age>15),15)) 

work = work %>%
  group_by(rel_assessment_area, rel_yrqtr,rel_year,rel_quarter,rel_age)  %>%
  summarise(number=n())  %>% 
  as.data.frame() %>%
  mutate(rel_yr=yearqtr2qtr(rel_year,rel_quarter,1950,13),season=1,tfill=999,gender=0,tag = 1:n())
work = work %>% mutate(number_prime = round(number * 0.725,1))
out_rel = work %>% select(tag,rel_assessment_area,rel_yr,season, tfill,gender,rel_age,number_prime) 

# Adapt release area to model area in SS:
out_rel$rel_assessment_area = 1

# Format for ss3
out_rel = out_rel %>% dplyr::rename(area = rel_assessment_area, yr = rel_yr, sex = gender, age = rel_age, Nrel = number_prime)

# Save (both, agg and aaf subconfigs):
write.csv(out_rel, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'aaf', 'tag-release.csv'), row.names = FALSE)
write.csv(out_rel, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'agg', 'tag-release.csv'), row.names = FALSE)

# -------------------------------------------------------------------------
# Produce recapture SS3 inputs: 

# Reassign areas:
dataC = dataC %>% mutate(rel_model_area = 1)
dataC = dataC %>% mutate(rel_assessment_area = if_else(rel_assessment_area %in% c(2,3,5), 2, 1))
dataC = dataC %>% mutate(rel_assessment_area_name = if_else(rel_assessment_area_name %in% c('1b','2','4'), '1b', '1a'))

# Check new areas:
table(dataC$rel_model_area)
table(dataC$rel_assessment_area)
table(dataC$rel_assessment_area_name)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Proceed by subconfig
spat_subconfig = 'agg'

# Reassign fleets:
recap_df = dataC %>% mutate(rec_model_fleet = if_else(rec_model_fleet %in% c(10,11,13), 7, # LL2 LL3 LL4 to LL1b
                                                      if_else(rec_model_fleet %in% c(12), 10, #GI4 to GI1b
                                                              if_else(rec_model_fleet %in% 14, 11, #OT4 to OT1b
                                                                      if_else(rec_model_fleet %in% c(15,18), 9, #TR4 TR2 to TR1b
                                                                              if_else(rec_model_fleet %in% c(16,19), 6, #FS2 FS4 to FS1b
                                                                                      if_else(rec_model_fleet %in% c(17,20), 8, #LS2 LS4 to LS1b
                                                                                              if_else(rec_model_fleet %in% 21, 12, rec_model_fleet)#LF4 to LF1b
                                                                                      )))))))
table(recap_df$rec_model_fleet)

# Produce recapture SS3 input
workC = recap_df %>% 
  dplyr::filter(project=='RTTP')  %>%
  mutate(rel_age = replace(rel_age,which(rel_age>15),15))

workC = workC %>%
  group_by(rel_assessment_area, rel_year,rel_quarter,rel_age, rec_year,rec_quarter,rec_model_fleet,rec_location)  %>%
  summarise(number=n(),number_prime=n())  %>% 
  as.data.frame() %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2005 & rec_quarter==1,number_prime = round(1/0.816*number/0.57,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2005 & rec_quarter==2,number_prime = round(1/0.621*number/0.57,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2005 & rec_quarter==3,number_prime = round(1/0.908*number/0.57,1))  %>% 	
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2005 & rec_quarter==4,number_prime = round(1/0.951*number/0.57,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2006 & rec_quarter==1,number_prime = round(1/0.921*number/0.61,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2006 & rec_quarter==2,number_prime = round(1/0.896*number/0.84,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2006 & rec_quarter==3,number_prime = round(1/0.979*number/0.89,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2006 & rec_quarter==4,number_prime = round(1/0.958*number/0.91,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2007 & rec_quarter==1,number_prime = round(1/0.934*number/0.93,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2007 & rec_quarter==2,number_prime = round(1/0.563*number/0.91,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2007 & rec_quarter==3,number_prime = round(1/0.975*number/0.91,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2007 & rec_quarter==4,number_prime = round(1/0.985*number/0.88,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2008 & rec_quarter==1,number_prime = round(1/0.877*number/0.88,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2008 & rec_quarter==2,number_prime = round(1/0.631*number/0.91,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2008 & rec_quarter==3,number_prime = round(1/0.953*number/0.94,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2008 & rec_quarter==4,number_prime = round(1/0.981*number/0.94,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2009 & rec_quarter==1,number_prime = round(1/0.518*number/0.94,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2009 & rec_quarter==2,number_prime = round(1/0.584*number/0.94,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2009 & rec_quarter==3,number_prime = round(1/0.956*number/0.94,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2009 & rec_quarter==4,number_prime = round(1/0.966*number/0.94,1))  %>% 	
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year > 2009,number_prime = round(1.1*number/0.94,1))  %>% 
  dplyr::filter(!((rec_location =='' | rec_location =='OTH') & rec_model_fleet %in% c(6,8)))  %>%
  group_by(rel_assessment_area, rel_year,rel_quarter,rel_age, rec_year,rec_quarter,rec_model_fleet)  %>%
  summarise(number=sum(number),number_prime=sum(number_prime))  %>%
  as.data.frame() %>%
  mutate(rec_yr=yearqtr2qtr(rec_year,rec_quarter,1950,13),season=1) %>% 
  left_join(select(work,c(rel_assessment_area,rel_year,rel_quarter,rel_age,tag)),by=c('rel_assessment_area','rel_year','rel_quarter','rel_age')) 
out_rec = workC %>% select(tag,rec_yr,season,rec_model_fleet,number_prime)

# Format for ss3
out_rec = out_rec %>% dplyr::rename(yr = rec_yr, fleet = rec_model_fleet, number = number_prime)

# Save:
write.csv(out_rec, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'tag-recapture.csv'), row.names = FALSE)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Proceed by subconfig
spat_subconfig = 'aaf'

# Reassign fleets:
# No need to do fleet reassignment
recap_df = dataC
table(recap_df$rec_model_fleet)

# Produce recapture SS3 input
workC = recap_df %>% 
  dplyr::filter(project=='RTTP')  %>%
  mutate(rel_age = replace(rel_age,which(rel_age>15),15))

workC = workC %>%
  group_by(rel_assessment_area, rel_year,rel_quarter,rel_age, rec_year,rec_quarter,rec_model_fleet,rec_location)  %>%
  summarise(number=n(),number_prime=n())  %>% 
  as.data.frame() %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2005 & rec_quarter==1,number_prime = round(1/0.816*number/0.57,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2005 & rec_quarter==2,number_prime = round(1/0.621*number/0.57,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2005 & rec_quarter==3,number_prime = round(1/0.908*number/0.57,1))  %>% 	
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2005 & rec_quarter==4,number_prime = round(1/0.951*number/0.57,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2006 & rec_quarter==1,number_prime = round(1/0.921*number/0.61,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2006 & rec_quarter==2,number_prime = round(1/0.896*number/0.84,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2006 & rec_quarter==3,number_prime = round(1/0.979*number/0.89,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2006 & rec_quarter==4,number_prime = round(1/0.958*number/0.91,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2007 & rec_quarter==1,number_prime = round(1/0.934*number/0.93,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2007 & rec_quarter==2,number_prime = round(1/0.563*number/0.91,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2007 & rec_quarter==3,number_prime = round(1/0.975*number/0.91,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2007 & rec_quarter==4,number_prime = round(1/0.985*number/0.88,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2008 & rec_quarter==1,number_prime = round(1/0.877*number/0.88,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2008 & rec_quarter==2,number_prime = round(1/0.631*number/0.91,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2008 & rec_quarter==3,number_prime = round(1/0.953*number/0.94,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2008 & rec_quarter==4,number_prime = round(1/0.981*number/0.94,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2009 & rec_quarter==1,number_prime = round(1/0.518*number/0.94,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2009 & rec_quarter==2,number_prime = round(1/0.584*number/0.94,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2009 & rec_quarter==3,number_prime = round(1/0.956*number/0.94,1))  %>% 
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year==2009 & rec_quarter==4,number_prime = round(1/0.966*number/0.94,1))  %>% 	
  mutate_cond(rec_location=='SEZ' & rec_model_fleet %in% c(6,8) & rec_year > 2009,number_prime = round(1.1*number/0.94,1))  %>% 
  dplyr::filter(!((rec_location =='' | rec_location =='OTH') & rec_model_fleet %in% c(6,8)))  %>%
  group_by(rel_assessment_area, rel_year,rel_quarter,rel_age, rec_year,rec_quarter,rec_model_fleet)  %>%
  summarise(number=sum(number),number_prime=sum(number_prime))  %>%
  as.data.frame() %>%
  mutate(rec_yr=yearqtr2qtr(rec_year,rec_quarter,1950,13),season=1) %>% 
  left_join(select(work,c(rel_assessment_area,rel_year,rel_quarter,rel_age,tag)),by=c('rel_assessment_area','rel_year','rel_quarter','rel_age')) 
out_rec = workC %>% select(tag,rec_yr,season,rec_model_fleet,number_prime)

# Format for ss3
out_rec = out_rec %>% dplyr::rename(yr = rec_yr, fleet = rec_model_fleet, number = number_prime)

# Save:
write.csv(out_rec, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'tag-recapture.csv'), row.names = FALSE)
