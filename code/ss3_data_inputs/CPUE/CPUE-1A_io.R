rm(list = ls())

# Spatial configuration:
spat_config = '1A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))

# Read fishery definitions
fish_info = get_fisheries(spat_config)
n_fisheries = max(fish_info$fleet_number)

# -------------------------------------------------------------------------
# Read unscaled CPUE data
Data79 = read.csv(file.path(shrpoint_path, 'data/processed', 'cpue_unscaled.csv'))

# Assign new assessment area:
Data79 = Data79 %>% mutate(NewAssessmentAreaName = '1234')

# Perform scaling:
data79 = Data79
data79$var <- NA
data79$pr_7994_m8 = data79$pr
index = data79$AssessmentAreaName=='1b'
work = data79[index,]
work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * yftwts[['7994 m8']][1]
# calculate var for each region and qter considering the same weighting to the var as to the prior
# var(a/b X)= (a/b)^2 var(x)
work$var= work$std^2*(1/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']))^2*(yftwts[['7994 m8']][1]^2 )
data79[index,]=work

index = data79$AssessmentAreaName=='2'
work = data79[index,]
work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * yftwts[['7994 m8']][2]
work$var= work$std^2*(1/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']))^2*(yftwts[['7994 m8']][2]^2 )
data79[index,]=work

index = data79$AssessmentAreaName=='3'
work = data79[index,]
work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * yftwts[['7994 m8']][3]
work$var= work$std^2*(1/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']))^2*(yftwts[['7994 m8']][3]^2 )
data79[index,]=work

index = data79$AssessmentAreaName=='4'
work = data79[index,]
work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * yftwts[['7994 m8']][4]
work$var= work$std^2*(1/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']))^2*(yftwts[['7994 m8']][4]^2 )
data79[index,]=work

# Aggregate by assessment area:
data <- data79 %>% group_by(yq,yr,qtr,NewAssessmentAreaName) %>% summarize(pr7994_m8_2R=sum(pr_7994_m8),std=sqrt(sum(var)))
# mean(data$pr7994_m8_2R[data$yr>=1979 & data$yr<= 1994 & data$NewAssessmentAreaName==1234])

# Mean CV of 0.2 again:
data <- data %>% group_by(NewAssessmentAreaName) %>% mutate(stdcv02=std/mean(std)*0.2) %>% mutate(season=1,cv=stdcv02) %>% 
  mutate(fleet = n_fisheries+1) %>% 
  select(qtr,season,fleet,pr7994_m8_2R,cv) # confirm fleet number 

# Save data:
write.csv(data, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'scaled_cpue_Meancv_02.csv'), row.names = FALSE)