rm(list = ls())

# Spatial configuration:
spat_config = '2A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source('code/auxiliary_functions.R')

# Read unscaled CPUE data
Data79 = read.csv(file.path(shrpoint_path, 'data/processed', 'cpue_unscaled.csv'))

# -------------------------------------------------------------------------
# Assign new assessment area:
Data79 = Data79 %>% mutate(NewAssessmentAreaName = if_else(AssessmentAreaName %in% c('1b', '2'), '12', '34'))

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
#check values Region 1+2 = 2.297
mean(data$pr7994_m8_2R[data$yr>=1979 & data$yr<= 1994 & data$NewAssessmentAreaName==12])
#check values Region 3+4 = 1.455
mean(data$pr7994_m8_2R[data$yr>=1979 & data$yr<= 1994 & data$NewAssessmentAreaName==34])

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Aggregated subconfiguration
spat_subconfig = 'agg'

# Read fishery definitions
fish_info = get_fisheries(spat_config)
n_fisheries = max(fish_info$fleet_number)

# Mean CV of 0.2 again:
data_agg <- data %>% group_by(NewAssessmentAreaName) %>% mutate(stdcv02=std/mean(std)*0.2) %>% mutate(season=1,cv=stdcv02) %>% 
  mutate(fleet = case_when(NewAssessmentAreaName=='12' ~ n_fisheries+1, TRUE ~ n_fisheries+2)) %>% 
  select(qtr,season,fleet,pr7994_m8_2R,cv) # confirm fleet number 
data_agg = data_agg[order(data_agg$fleet, data_agg$qtr), ]

# Format for ss3:
cpue_df = data_agg %>% ungroup() %>% select(qtr, season, fleet, pr7994_m8_2R, cv)
cpue_df = cpue_df %>% dplyr::rename(year = qtr, seas = season, index = fleet, obs = pr7994_m8_2R, se_log = cv)
cpue_df$seas = 1

# Save data:
write.csv(cpue_df, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue.csv'), row.names = FALSE)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Areas-as-fleets subconfiguration
spat_subconfig = 'aaf'

# Read fishery definitions
fish_info = get_fisheries('4A_io')
n_fisheries = max(fish_info$fleet_number)

# Mean CV of 0.2 again:
data_agg <- data %>% group_by(NewAssessmentAreaName) %>% mutate(stdcv02=std/mean(std)*0.2) %>% mutate(season=1,cv=stdcv02) %>% 
  mutate(fleet = case_when(NewAssessmentAreaName=='12' ~ n_fisheries+1, TRUE ~ n_fisheries+2)) %>% 
  select(qtr,season,fleet,pr7994_m8_2R,cv) # confirm fleet number 
data_agg = data_agg[order(data_agg$fleet, data_agg$qtr), ]

# Format for ss3:
cpue_df = data_agg %>% ungroup() %>% select(qtr, season, fleet, pr7994_m8_2R, cv)
cpue_df = cpue_df %>% dplyr::rename(year = qtr, seas = season, index = fleet, obs = pr7994_m8_2R, se_log = cv)
cpue_df$seas = 1

# Save data:
write.csv(cpue_df, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue.csv'), row.names = FALSE)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# For other CPUE indices (FS, LS, ABBI), just copy the input files generated for the 4A model.
# Make sure to change the fleet number in your SS3 configuration

# Areas-as-fleets subconfiguration
spat_subconfig = 'aaf'
file.copy(from = file.path(shrpoint_path, 'data/ss3_inputs/4A_io', 'cpue-fs.csv'),
          to = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-fs.csv'))
file.copy(from = file.path(shrpoint_path, 'data/ss3_inputs/4A_io', 'cpue-ls.csv'),
          to = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-ls.csv'))
file.copy(from = file.path(shrpoint_path, 'data/ss3_inputs/4A_io', 'cpue-abbi.csv'),
          to = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-abbi.csv'))

# aggregated subconfiguration
spat_subconfig = 'agg'
file.copy(from = file.path(shrpoint_path, 'data/ss3_inputs/4A_io', 'cpue-fs.csv'),
          to = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-fs.csv'))
file.copy(from = file.path(shrpoint_path, 'data/ss3_inputs/4A_io', 'cpue-ls.csv'),
          to = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-ls.csv'))
file.copy(from = file.path(shrpoint_path, 'data/ss3_inputs/4A_io', 'cpue-abbi.csv'),
          to = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-abbi.csv'))


