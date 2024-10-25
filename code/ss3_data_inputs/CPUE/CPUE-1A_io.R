rm(list = ls())

# Spatial configuration:
spat_config = '1A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source('code/auxiliary_functions.R')

# Types of data treatment:
cpue_treatment = c('qt', 'qt-seas1', 'qt-avgyr', 'yr', 'yr-seasonal', 'yr-constant')

# -------------------------------------------------------------------------

for(k in seq_along(cpue_treatment)) {
  
  if(str_sub(cpue_treatment[k], 1, 2) == 'qt') Data79 = read.csv(file.path(shrpoint_path, 'data/processed', 'cpue_unscaled-qt.csv'))
  if(str_sub(cpue_treatment[k], 1, 2) == 'yr') Data79 = read.csv(file.path(shrpoint_path, 'data/processed', 'cpue_unscaled-yr.csv'))
  
  # Change assessment areas
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

  # Read fishery definitions
  fish_info = get_fisheries(spat_config)
  n_fisheries = max(fish_info$fleet_number)
  
  # Mean CV of 0.2 again:
  data_agg <- data %>% group_by(NewAssessmentAreaName) %>% mutate(stdcv02=std/mean(std)*0.2) %>% mutate(season=1,cv=stdcv02) %>% 
    mutate(fleet = n_fisheries+1) %>% 
    select(qtr,season,fleet,pr7994_m8_2R,cv) # confirm fleet number 
  
  # Format for ss3:
  cpue_df = data_agg %>% ungroup() %>% select(qtr, season, fleet, pr7994_m8_2R, cv)
  cpue_df = cpue_df %>% dplyr::rename(year = qtr, seas = season, index = fleet, obs = pr7994_m8_2R, se_log = cv)
  cpue_df$seas = 1
  
  # -------------------------------------------------------------------------
  # Aggregated subconfiguration
  spat_subconfig = 'agg'
    
  # Modify data based on criterium:
  if(cpue_treatment[k] == 'qt') {
    out_data = cpue_df
    qt_data = out_data # save it for later
    # Save LL CPUE data:
    write.csv(out_data, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-ll-qt.csv'), row.names = FALSE)
  }
  if(cpue_treatment[k] == 'qt-seas1') {
    out_data = cpue_df %>% mutate(quarter = ssts2q(year)) %>% dplyr::filter(quarter == 1) %>% select(-quarter)
    # Save LL CPUE data:
    write.csv(out_data, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-ll-qt-seas1.csv'), row.names = FALSE)
  }
  if(cpue_treatment[k] == 'qt-avgyr') {
    out_data = cpue_df %>% mutate(year_int = floor(ssts2yq(year))) %>% 
      group_by(year_int, seas, index) %>% summarise(obs = mean(obs), se_log = mean(se_log)) %>%
      dplyr::rename(year = year_int) %>% mutate(year = yearqtr2qtr(year, 1, initial = 1950, base = 13))
    # Save LL CPUE data:
    write.csv(out_data, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-ll-qt-avgyr.csv'), row.names = FALSE)
  }
  if(cpue_treatment[k] == 'yr') {
    out_data = cpue_df
    # Save LL CPUE data:
    write.csv(out_data, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-ll-yr.csv'), row.names = FALSE)
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
    write.csv(out_data, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-ll-yr-seasonal.csv'), row.names = FALSE)
  }
  if(cpue_treatment[k] == 'yr-constant') {
    out_data = cpue_df %>% slice( rep(row_number(), each = 4) ) # repeat every row 4 times
    std_years = NULL
    for(j in 1:nrow(cpue_df)) {
      std_years = c(std_years, (cpue_df$year[j]):(cpue_df$year[j]+3))
    }
    out_data$year = std_years    
    # Save LL CPUE data:
    write.csv(out_data, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-ll-yr-constant.csv'), row.names = FALSE)
  }
    
}

# -------------------------------------------------------------------------
# Areas-as-fleets subconfiguration
spat_subconfig = 'aaf'

file.copy(from = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'agg', 'cpue-ll-qt.csv'),
          to = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-ll-qt.csv'))
file.copy(from = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'agg', 'cpue-ll-qt-seas1.csv'),
          to = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-ll-qt-seas1.csv'))
file.copy(from = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'agg', 'cpue-ll-qt-avgyr.csv'),
          to = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-ll-qt-avgyr.csv'))
file.copy(from = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'agg', 'cpue-ll-yr.csv'),
          to = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-ll-yr.csv'))
file.copy(from = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'agg', 'cpue-ll-yr-seasonal.csv'),
          to = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-ll-yr-seasonal.csv'))
file.copy(from = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'agg', 'cpue-ll-yr-constant.csv'),
          to = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, spat_subconfig, 'cpue-ll-yr-constant.csv'))

  
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
