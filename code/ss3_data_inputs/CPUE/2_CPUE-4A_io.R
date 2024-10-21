rm(list = ls())

# Spatial configuration:
spat_config = '4A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source('code/auxiliary_functions.R')

# Read fishery definitions
fish_info = get_fisheries(spat_config)
n_fisheries = max(fish_info$fleet_number)

# -------------------------------------------------------------------------
# Read unscaled LL CPUE data
Data79 = read.csv(file.path(shrpoint_path, 'data/processed', 'cpue_unscaled.csv'))

# Perform scaling:
data79 = Data79
data79$pr_7994_m8 = data79$pr
index = data79$AssessmentAreaName=='1b'
work = data79[index,]
work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * yftwts[['7994 m8']][1]
data79[index,]=work
index = data79$AssessmentAreaName=='2'
work = data79[index,]
work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * yftwts[['7994 m8']][2]
data79[index,]=work
index = data79$AssessmentAreaName=='3'
work = data79[index,]
work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * yftwts[['7994 m8']][3]
data79[index,]=work
index = data79$AssessmentAreaName=='4'
work = data79[index,]
work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * yftwts[['7994 m8']][4]
data79[index,]=work

# Prepare data for ss3:
data <- data79 %>% mutate(season = 1,cv = stdcv02) %>% 
  mutate(fleet = case_when(AssessmentAreaName=='1b' ~ n_fisheries+1,
                           AssessmentAreaName=='2' ~ n_fisheries+2,
                           AssessmentAreaName=='3' ~ n_fisheries+3, TRUE ~ n_fisheries+4)) %>% 
  select(qtr, season, fleet, pr_7994_m8, cv)

# Format for ss3:
cpue_df = data_agg %>% ungroup() %>% select(qtr, season, fleet, pr_7994_m8, cv)
cpue_df = cpue_df %>% dplyr::rename(year = qtr, seas = season, index = fleet, obs = pr_7994_m8, se_log = cv)
cpue_df$seas = 1

# Save LL CPUE data:
write.csv(cpue_df, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-ll.csv'), row.names = FALSE)

# -------------------------------------------------------------------------
# Read FS CPUE
cpue_fs = read.csv(file.path(shrpoint_path, 'data/raw/YFT/PS FSC/2024-cpue-standardization-iotc-yft-fsc.quarter-indices.just-essentials.csv'), sep = ';')
cpue_fs = cpue_fs %>% mutate(year = yearqtr2qtr(year, quarter, initial = 1950, base = 13), .after = date)
cpue_fs = cpue_fs %>% mutate(seas = 1, index =  n_fisheries + 5, .after = year)
cpue_fs = cpue_fs %>% dplyr::rename(obs = yft_adult_rate_Mean, se_log = yft_adult_rate_cv)
cpue_fs = cpue_fs %>% select(-c(date, quarter))

# Save FS data:
write.csv(cpue_fs, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-fs.csv'), row.names = FALSE)

# -------------------------------------------------------------------------
# Read LS CPUE
cpue_ls = read.csv(file.path(shrpoint_path, 'data/raw/YFT/PSLS/st-GLMM_FOB.csv'))
cpue_ls = cpue_ls %>% mutate(year = floor(Time), quarter = (Time%%1)*4 + 1, .after = Time)
cpue_ls = cpue_ls %>% mutate(year = yearqtr2qtr(year, quarter, initial = 1950, base = 13), .after = quarter)
cpue_ls = cpue_ls %>% mutate(seas = 1, index =  n_fisheries + 6, .after = year)
cpue_ls = cpue_ls %>% mutate(cv = SE/Est)
cpue_ls = cpue_ls %>% dplyr::rename(obs = Est, se_log = cv)
cpue_ls = cpue_ls %>% select(-c(Time, quarter, Lower, Upper, SE, Type))

# Save LS data:
write.csv(cpue_ls, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-ls.csv'), row.names = FALSE)

# -------------------------------------------------------------------------
# Read ABBI CPUE
cpue_abbi = read.csv(file.path(shrpoint_path, 'data/raw/YFT/ABBI/ABBI.csv'))
cpue_abbi = cpue_abbi %>% mutate(quarter = quarter*4 + 1)
cpue_abbi = cpue_abbi %>% mutate(year = yearqtr2qtr(yearN, quarter, initial = 1950, base = 13), .after = timestamp)
cpue_abbi = cpue_abbi %>% mutate(seas = 1, index =  n_fisheries + 7, .after = year)
cpue_abbi = cpue_abbi %>% mutate(cv = Itotal.sd/Itotal)
cpue_abbi$cv[is.na(cpue_abbi$cv)] = mean(cpue_abbi$cv, na.rm = TRUE) # replace missing CV with mean
cpue_abbi = cpue_abbi %>% dplyr::rename(obs = Itotal, se_log = cv)
cpue_abbi = cpue_abbi %>% select(-c(timestamp, Itotal.sd, species, yearN, quarter))

# Save abbi data:
write.csv(cpue_abbi, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-abbi.csv'), row.names = FALSE)
