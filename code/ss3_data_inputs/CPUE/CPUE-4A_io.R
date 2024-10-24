rm(list = ls())

# Spatial configuration:
spat_config = '4A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))

# Read fishery definitions
fish_info = get_fisheries(spat_config)
n_fisheries = max(fish_info$fleet_number)

# Types of data treatment:
cpue_treatment = c('qt', 'qt-seas1', 'qt-avgyr', 'yr', 'yr-seasonal', 'yr-constant')

# -------------------------------------------------------------------------
# Read unscaled LL CPUE data

for(k in seq_along(cpue_treatment)) {

  if(str_sub(cpue_treatment[k], 1, 2) == 'qt') Data79 = read.csv(file.path(shrpoint_path, 'data/processed', 'cpue_unscaled-qt.csv'))
  if(str_sub(cpue_treatment[k], 1, 2) == 'yr') Data79 = read.csv(file.path(shrpoint_path, 'data/processed', 'cpue_unscaled-yr.csv'))
  
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
  data <- data79 %>% mutate(season = 1, cv = stdcv02) %>% 
    mutate(fleet = case_when(AssessmentAreaName=='1b' ~ n_fisheries+1,
                             AssessmentAreaName=='2' ~ n_fisheries+2,
                             AssessmentAreaName=='3' ~ n_fisheries+3, TRUE ~ n_fisheries+4)) %>% 
    select(qtr, season, fleet, pr_7994_m8, cv)
  
  # Format for ss3:
  cpue_df = data %>% ungroup() %>% select(qtr, season, fleet, pr_7994_m8, cv)
  cpue_df = cpue_df %>% dplyr::rename(year = qtr, seas = season, index = fleet, obs = pr_7994_m8, se_log = cv)
  cpue_df$seas = 1  
  
  # Modify data based on criterium:
  if(cpue_treatment[k] == 'qt') {
    out_data = cpue_df
    qt_data = out_data # save it for later
    # Save LL CPUE data:
    write.csv(out_data, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-ll-qt.csv'), row.names = FALSE)
  }
  if(cpue_treatment[k] == 'qt-seas1') {
    out_data = cpue_df %>% mutate(quarter = ssts2q(year)) %>% dplyr::filter(quarter == 1) %>% select(-quarter)
    # Save LL CPUE data:
    write.csv(out_data, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-ll-qt-seas1.csv'), row.names = FALSE)
  }
  if(cpue_treatment[k] == 'qt-avgyr') {
    out_data = cpue_df %>% mutate(year_int = floor(ssts2yq(year))) %>% 
                  group_by(year_int, seas, index) %>% summarise(obs = mean(obs), se_log = mean(se_log)) %>%
                  dplyr::rename(year = year_int) %>% mutate(year = yearqtr2qtr(year, 1, initial = 1950, base = 13))
    # Save LL CPUE data:
    write.csv(out_data, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-ll-qt-avgyr.csv'), row.names = FALSE)
  }
  if(cpue_treatment[k] == 'yr') {
    out_data = cpue_df
    # Save LL CPUE data:
    write.csv(out_data, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-ll-yr.csv'), row.names = FALSE)
  }
  if(cpue_treatment[k] == 'yr-seasonal') {
    pattern_df = qt_data %>% mutate(year_int = floor(ssts2yq(year))) %>% group_by(year_int, seas, index) %>% 
                  group_split(.keep = TRUE) %>%
                  purrr::map_dfr(~ prop_quarter_cpue(.x))
    pattern_df = pattern_df %>% select(-c(se_log)) %>% dplyr::rename(mult = obs, ssyear = year)
    cpue_df = cpue_df %>% mutate(year_int = floor(ssts2yq(year)))
    out_data = left_join(cpue_df, pattern_df)
    out_data = out_data %>% mutate(obs = obs*mult) %>% select(-c(year, mult, year_int)) %>%
                  relocate(ssyear, .before = seas) %>% dplyr::rename(year = ssyear)
    # Save LL CPUE data:
    write.csv(out_data, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-ll-yr-seasonal.csv'), row.names = FALSE)
  }
  if(cpue_treatment[k] == 'yr-constant') {
    out_data = cpue_df %>% slice( rep(row_number(), each = 4) ) # repeat every row 4 times
    std_years = NULL
    for(j in 1:nrow(cpue_df)) {
      std_years = c(std_years, (cpue_df$year[j]):(cpue_df$year[j]+3))
    }
    out_data$year = std_years    
    # Save LL CPUE data:
    write.csv(out_data, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-ll-yr-constant.csv'), row.names = FALSE)
  }

}

# -------------------------------------------------------------------------
# Read FS CPUE
cpue_fs = read.csv(file.path(shrpoint_path, 'data/raw/indices/PS FSC/2024-cpue-standardization-iotc-yft-fsc.quarter-indices.just-essentials.csv'), sep = ';')
cpue_fs = cpue_fs %>% mutate(year = yearqtr2qtr(year, quarter, initial = 1950, base = 13), .after = date)
cpue_fs = cpue_fs %>% mutate(seas = 1, index =  n_fisheries + 5, .after = year)
cpue_fs = cpue_fs %>% dplyr::rename(obs = yft_adult_rate_Mean, se_log = yft_adult_rate_cv)
cpue_fs = cpue_fs %>% select(-c(date, quarter))

# Save FS data:
write.csv(cpue_fs, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-fs.csv'), row.names = FALSE)

# -------------------------------------------------------------------------
# Read LS CPUE
cpue_ls = read.csv(file.path(shrpoint_path, 'data/raw/indices/PSLS/st-GLMM_FOB.csv'))
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
cpue_abbi = read.csv(file.path(shrpoint_path, 'data/raw/indices/ABBI/ABBI.csv'))
cpue_abbi = cpue_abbi %>% mutate(quarter = quarter*4 + 1)
cpue_abbi = cpue_abbi %>% mutate(year = yearqtr2qtr(yearN, quarter, initial = 1950, base = 13), .after = timestamp)
cpue_abbi = cpue_abbi %>% mutate(seas = 1, index =  n_fisheries + 7, .after = year)
cpue_abbi = cpue_abbi %>% mutate(cv = Itotal.sd/Itotal)
cpue_abbi$cv[is.na(cpue_abbi$cv)] = mean(cpue_abbi$cv, na.rm = TRUE) # replace missing CV with mean
cpue_abbi = cpue_abbi %>% dplyr::rename(obs = Itotal, se_log = cv)
cpue_abbi = cpue_abbi %>% select(-c(timestamp, Itotal.sd, species, yearN, quarter))

# Save abbi data:
write.csv(cpue_abbi, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-abbi.csv'), row.names = FALSE)
