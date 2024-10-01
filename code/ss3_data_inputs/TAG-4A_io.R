rm(list = ls())

# Spatial configuration:
spat_config = '4A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))
source(here('code', 'parameters_for_plots.R'))


# -------------------------------------------------------------------------
# Read data in:
data = read.csv(file.path(shrpoint_path, 'data/processed', 'tag_release.csv'))
dataC = read.csv(file.path(shrpoint_path, 'data/processed', 'tag_recapture.csv'))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Produce release SS3 inputs: (Fonteneau)

# Age slicing
data$rel_age = sapply(data$tag_length, age_slicing, mlen_at_age = Len_font)
dataC$rel_age = sapply(dataC$tag_length, age_slicing, mlen_at_age = Len_font)

# Release data input
work = data %>% 
  dplyr::filter(project=='RTTP')  %>%
  mutate(rel_age = replace(rel_age,which(rel_age>15),15)) %>%
  group_by(rel_assessment_area, rel_yrqtr,rel_year,rel_quarter,rel_age)  %>%
  summarise(number=n())  %>% 
  as.data.frame() %>%
  mutate(rel_yr=yearqtr2qtr(rel_year,rel_quarter,1950,13),season=1,tfill=999,gender=0,tag = 1:n())
work = work %>% mutate(number_prime = round(number * 0.725,1))
out_rel = work %>% select(tag,rel_assessment_area,rel_yr,season, tfill,gender,rel_age,number_prime) 

# Adapt release area to model area in SS:
out_rel$rel_assessment_area[out_rel$rel_assessment_area==2] <- 1
out_rel$rel_assessment_area[out_rel$rel_assessment_area==3] <- 2

# Save:
write.csv(out_rel, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'tag-release-fonteneau.csv'), row.names = FALSE)

# Produce recapture SS3 input
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
out_rec = workC %>% select(tag,rec_yr,season,rec_model_fleet,number_prime)
# Save:
write.csv(out_rec, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'tag-recapture-fonteneau.csv'), row.names = FALSE)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Produce release SS3 inputs: (Fonteneau)

# Age slicing
data$rel_age = sapply(data$tag_length, age_slicing, mlen_at_age = Len_farl)
dataC$rel_age = sapply(dataC$tag_length, age_slicing, mlen_at_age = Len_farl)

# Release data input
work = data %>% 
  dplyr::filter(project=='RTTP')  %>%
  mutate(rel_age = replace(rel_age,which(rel_age>15),15)) %>%
  group_by(rel_assessment_area, rel_yrqtr,rel_year,rel_quarter,rel_age)  %>%
  summarise(number=n())  %>% 
  as.data.frame() %>%
  mutate(rel_yr=yearqtr2qtr(rel_year,rel_quarter,1950,13),season=1,tfill=999,gender=0,tag = 1:n())
work = work %>% mutate(number_prime = round(number * 0.725,1))
out_rel = work %>% select(tag,rel_assessment_area,rel_yr,season, tfill,gender,rel_age,number_prime) 

# Adapt release area to model area in SS:
out_rel$rel_assessment_area[out_rel$rel_assessment_area==2] <- 1
out_rel$rel_assessment_area[out_rel$rel_assessment_area==3] <- 2

# Save:
write.csv(out_rel, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'tag-release-farley.csv'), row.names = FALSE)

# Produce recapture SS3 input
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
out_rec = workC %>% select(tag,rec_yr,season,rec_model_fleet,number_prime)
# Save:
write.csv(out_rec, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'tag-recapture-farley.csv'), row.names = FALSE)
