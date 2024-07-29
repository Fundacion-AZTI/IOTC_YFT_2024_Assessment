#
#       STEPWISE APPROACH
#       start- 00_BC -4A_io_lin_v33022_FixedParam2_Fl11
#
#............................................................


library(r4ss)

proj_dir = here::here()
setwd(proj_dir)

source("code/analysingModels/auxiliary_functions_4analysingModels.R")
# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)

# SS base files path (in Sharepoint):
SS_base = 'models/base_win_vs_lin/4A_io_lin_v33022_FixedParam2_Fl11'

# SS configuration path (in Sharepoint):
SS_config = 'models/update'

# SS input data path (in Sharepoint):
SS_data = 'data/ss3_inputs/4A_io'

# -------------------------------------------------------------------------

# Read base SS inputs (from 2021 assessment)
base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))

### Start updating ---------------------------------------

### v0: with copy paste slightly different results due to the difference in number of digits ### -------------------------------------

config_name = '00_BC'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir)

# Temporary files:
dat_0 = base_dat
ctl_0 = base_ctl
fore_0 = base_fore
start_0 = base_start

# If want to make changes, made them here:

# Write SS files:
SS_writedat(dat_0, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_0, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_0, dir = tmp_dir, overwrite = T)
SS_writestarter(start_0, dir = tmp_dir, overwrite = T)

# Run model:
r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')

#### end ####


### Update catch data until 2022 --------------------------------------------
  
  config_name = '01_update_catch'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  dat_1 = dat_0
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  # Updated catch data frame:
  catch_df = read.csv(file.path(shrpoint_path, SS_data, 'catch.csv'))
  updated_catch = data.frame(year = catch_df[,'qtr'], seas = 1, fleet = catch_df[,'ModelFleet'], 
                             catch = catch_df[,'Catch'], catch_se = 0.01)
  updated_catch = updated_catch %>% add_row(data.frame(year = -999, seas = 1, fleet = 1, catch = 0, catch_se = 0.01), .before = 1) # shouldnt we also add for all fleets?
  dat_1$catch = updated_catch
  # Last year = 2022:
  dat_1$endyr = 304
  
  # Write SS files:
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  # Run model:
  r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')

#### end ####


### Update cpue data until 2022 --------------------------------------------
  
  config_name = '02_update_cpue'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  
  dat_1 = dat_0
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  # Updated catch data frame:
  catch_df = read.csv(file.path(shrpoint_path, SS_data, 'catch.csv'))
  updated_catch = data.frame(year = catch_df[,'qtr'], seas = 1, fleet = catch_df[,'ModelFleet'], 
                             catch = catch_df[,'Catch'], catch_se = 0.01)
  updated_catch = updated_catch %>% add_row(data.frame(year = -999, seas = 1, fleet = 1, catch = 0, catch_se = 0.01), .before = 1) # shouldnt we also add for all fleets?
  dat_1$catch = updated_catch
  # Last year = 2022:
  dat_1$endyr = 304
  
  #update length
  cpue_df = read.csv(file.path(shrpoint_path, SS_data, 'scaled_cpue.csv'))
  
  dat_1$CPUE <- cpue_df
  
  
  # Write SS files:
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  # Run model:
  r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')

#### end ####




### Update length data until 2022 --------------------------------------------

  config_name = '03_update_length'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  
  dat_1 = dat_0
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  # Updated length data frame:
  catch_df = read.csv(file.path(shrpoint_path, SS_data, 'catch.csv'))
  updated_catch = data.frame(year = catch_df[,'qtr'], seas = 1, fleet = catch_df[,'ModelFleet'], 
                             catch = catch_df[,'Catch'], catch_se = 0.01)
  updated_catch = updated_catch %>% add_row(data.frame(year = -999, seas = 1, fleet = 1, catch = 0, catch_se = 0.01), .before = 1) # shouldnt we also add for all fleets?
  dat_1$catch = updated_catch
  # Last year = 2022:
  dat_1$endyr = 304
  
  #update cpue
  cpue_df = read.csv(file.path(shrpoint_path, SS_data, 'scaled_cpue.csv'))
  dat_1$CPUE <- cpue_df
  
  #update length
  length_df = read.csv(file.path(shrpoint_path, SS_data, 'size.csv'))
  dat_1$lencomp <- length_df %>% subset(!is.na(ModelFleet ))
  
  # Write SS files:
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  # Run model:
  r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')

#### end ####
  
  
  
  
  ### Update ONLY length data until 2022 --------------------------------------------
  
  config_name = '03_update_only_length_until_296'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  
  dat_1 = dat_0
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  # Updated length data frame:
  # catch_df = read.csv(file.path(shrpoint_path, SS_data, 'catch.csv'))
  # updated_catch = data.frame(year = catch_df[,'qtr'], seas = 1, fleet = catch_df[,'ModelFleet'], 
  #                            catch = catch_df[,'Catch'], catch_se = 0.01)
  # updated_catch = updated_catch %>% add_row(data.frame(year = -999, seas = 1, fleet = 1, catch = 0, catch_se = 0.01), .before = 1) # shouldnt we also add for all fleets?
  # dat_1$catch = updated_catch
  # Last year = 2022:
  # dat_1$endyr = 304
  
  #update cpue
  # cpue_df = read.csv(file.path(shrpoint_path, SS_data, 'scaled_cpue.csv'))
  # dat_1$CPUE <- cpue_df
  # 
  #update length
  length_df = read.csv(file.path(shrpoint_path, SS_data, 'size.csv'))
  dat_1$lencomp <- length_df %>% subset(!is.na(ModelFleet )) %>% filter(Yr<=296)
  
  # Write SS files:
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  # Run model:
  r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
  
  #### end ####
  