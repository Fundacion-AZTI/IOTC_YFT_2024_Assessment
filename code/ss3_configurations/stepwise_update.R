#
#       STEPWISE APPROACH
#       start- 00_BC -4A_io_lin_v33022_FixedParam2_Fl11
#
#............................................................

library(dplyr)
library(r4ss)
library(here)

proj_dir = here::here()
setwd(proj_dir)

#source("code/analysingModels/auxiliary_functions_4analysingModels.R")
source("code/auxiliary_functions.R")
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
  
  #update cpue
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


#.......................................................................
### Update length data until 2022 --------------------------------------------
  #.......................................................................
  remotes::install_github("r4ss/r4ss@signif") 
  config_name = '03_update_length'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  SS_base = 'models/update/02_update_cpue'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  
  # Updated length data frame:

  #update length
  length_df = read.csv(file.path(shrpoint_path, SS_data, 'size-original.csv'))
  dat_1$lencomp <- length_df %>% subset(!is.na(ModelFleet )) #%>% subset(!(Yr<230 & ModelFleet==21))
  fleetNames <- read.csv(file=file.path("data","ss3_inputs","4A_io","FleetNames.csv"))
  
  dat_1$fleetnames <- fleetNames$FleetNames
  # Read base SS inputs (from 2021 assessment)

  # Write SS files:
  dat_1$lencomp$Nsamp=5
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
     SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
  # Run model:
  r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')

#### end ####
  
  
  
  #.......................................................................
  ### Update ONLY length data until  --------------------------------------------
  #.......................................................................
  
  config_name = '03_update_only_length_until_296'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  
  dat_1 = dat_0
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  
  #ONLY update length
  length_df = read.csv(file.path(shrpoint_path, SS_data, 'size.csv'))
  dat_1$lencomp <- length_df %>% subset(!is.na(ModelFleet )) %>% subset(!(Yr<230 & ModelFleet==21))
  
  # Write SS files:
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  # Run model:
  r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
  
  #### end ####
  
  #.......................................................................
    ### Update ONLY length LL --------------------------------------------
  #.......................................................................
  config_name = '03_update_cpue_updateLC_LL'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  SS_base = 'models/update/02_update_cpue'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  
  #ONLY update length FL3, 7, 10,11,13
  length_df = read.csv(file.path(shrpoint_path, SS_data, 'size.csv'))
  LL_flt <- c(3,7,10,11,13)
  length_df <- length_df %>%  rename_with(.cols = 1:length(names(length_df)),~ names(dat_1$lencomp))
  dat_1$lencomp <- base_dat$lencomp %>% subset(!(FltSvy %in% c(LL_flt))) %>% add_row(length_df[length_df$FltSvy %in% c(3,7,10,11,13),])
  
  
  # Write SS files:
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  # Run model:
  r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
  
  #.......................................................................
  ### Update ONLY length LL LF --------------------------------------------
  #.......................................................................
  
  config_name = '03_update_cpue_updateLC_LL_LF'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  SS_base = 'models/update/02_update_cpue'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  
  length_df = read.csv(file.path(shrpoint_path, SS_data, 'size.csv'))
  LL_flt <- c(3,7,10,11,13,21)
  length_df <- length_df %>%  rename_with(.cols = 1:length(names(length_df)),~ names(dat_1$lencomp))
  dat_1$lencomp <- base_dat$lencomp %>% subset(!(FltSvy %in% c(LL_flt))) %>% add_row(length_df[length_df$FltSvy %in% c(3,7,10,11,13),])
  
  
  # Write SS files:
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  # Run model:
  r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
  
  
  #.......................................................................
    ### Update ONLY length LL LF PSFSC --------------------------------------------
  #.......................................................................
  
  config_name = '03_update_cpue_updateLC_LL_LF_PSfsc'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  SS_base = 'models/update/02_update_cpue'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  
  #ONLY update length FL3, 7, 10,11,13
  LL_flt <- c(3,7,10,11,13,21)
  PSfsc_flt <- c(6,16,19)
  
  length_df = read.csv(file.path(shrpoint_path, SS_data, 'size.csv'))
  length_df <- length_df %>%  rename_with(.cols = 1:length(names(length_df)),~ names(dat_1$lencomp))
  dat_1$lencomp <- base_dat$lencomp %>% subset(!(FltSvy %in% c(LL_flt,PSfsc_flt ))) %>% add_row(length_df[length_df$FltSvy %in% c(LL_flt,PSfsc_flt ),])
  
  
  # Write SS files:
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  # Run model:
  r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
  
  
  #.......................................................................
  
  ### Update ONLY length LL LF PSFSC --------------------------------------------
  
  config_name = '03_update_cpue_updateLC_LL_LF_PSfsc'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  SS_base = 'models/update/02_update_cpue'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  
  #ONLY update length FL3, 7, 10,11,13
  LL_flt <- c(3,7,10,11,13,21)
  PSfsc_flt <- c(6,16,19)
  
  length_df = read.csv(file.path(shrpoint_path, SS_data, 'size.csv'))
  length_df <- length_df %>%  rename_with(.cols = 1:length(names(length_df)),~ names(dat_1$lencomp))
  dat_1$lencomp <- base_dat$lencomp %>% subset(!(FltSvy %in% c(LL_flt,PSfsc_flt ))) %>% add_row(length_df[length_df$FltSvy %in% c(LL_flt,PSfsc_flt ),])
  
  
  # Write SS files:
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  # Run model:
  r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
  
  
  #.......................................................................
  
  ### Update ONLY length LL LF PSFSC HD --------------------------------------------
  
  config_name = '03_update_cpue_updateLC_LL_LF_PSfsc_HD'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  SS_base = 'models/update/02_update_cpue'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  
  #ONLY update length FL3, 7, 10,11,13
  LL_flt <- c(3,7,10,11,13,21)
  PSfsc_flt <- c(6,16,19)
  HD_flt <- 2
  length_df = read.csv(file.path(shrpoint_path, SS_data, 'size.csv'))
  length_df <- length_df %>%  rename_with(.cols = 1:length(names(length_df)),~ names(dat_1$lencomp))
  dat_1$lencomp <- base_dat$lencomp %>% subset(!(FltSvy %in% c(LL_flt,PSfsc_flt,HD_flt ))) %>% 
    add_row(length_df[length_df$FltSvy %in% c(LL_flt,PSfsc_flt,HD_flt  ),])
  
  
  # Write SS files:
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  # Run model:
  r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
  
  
  #.......................................................................
  
  ### Update ONLY length LL LF PSFSC HD LS--------------------------------------------
  
  config_name = '03_update_cpue_updateLC_LL_LF_PSfsc_HD_PSls'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  SS_base = 'models/update/02_update_cpue'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  
  #.......................................................................
  
  #ONLY update length FL3, 7, 10,11,13
  LL_flt <- c(3,7,10,11,13,21)
  PSfsc_flt <- c(6,16,19)
  HD_flt <- 2
  PSLS_flt <- c(8,17,20)
  length_df = read.csv(file.path(shrpoint_path, SS_data, 'size.csv'))
  length_df <- length_df %>%  rename_with(.cols = 1:length(names(length_df)),~ names(dat_1$lencomp))
  dat_1$lencomp <- base_dat$lencomp %>% subset(!(FltSvy %in% c(LL_flt,PSfsc_flt,HD_flt,PSLS_flt  ))) %>% 
    add_row(length_df[length_df$FltSvy %in% c(LL_flt,PSfsc_flt,HD_flt,PSLS_flt  ),])
  
  
  # Write SS files:
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  # Run model:
  r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
  
  
  #.......................................................................
  
  ### Update ONLY length LL LF PSFSC HD LS GI -------------------------------------------
  
  config_name = '03_update_cpue_updateLC_LL_LF_PSfsc_HD_PSls_GI'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  SS_base = 'models/update/02_update_cpue'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  
  #ONLY update length FL3, 7, 10,11,13
  LL_flt <- c(3,7,10,11,13,21)
  PSfsc_flt <- c(6,16,19)
  HD_flt <- 2
  PSLS_flt <- c(8,17,20)
  GI_flt <- c(1,4,12)
  length_df = read.csv(file.path(shrpoint_path, SS_data, 'size.csv'))
  length_df <- length_df %>%  rename_with(.cols = 1:length(names(length_df)),~ names(dat_1$lencomp))
  dat_1$lencomp <- base_dat$lencomp %>% subset(!(FltSvy %in% c(LL_flt,PSfsc_flt,HD_flt,PSLS_flt,GI_flt  ))) %>% 
    add_row(length_df[length_df$FltSvy %in% c(LL_flt,PSfsc_flt,HD_flt,PSLS_flt,GI_flt  ),])
  
  
  # Write SS files:
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  # Run model:
  r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
  
  
  #.......................................................................
  
  ### Update ONLY length LL LF PSFSC HD LS BB -------------------------------------------
  
  config_name = '03_update_cpue_updateLC_LL_LF_PSfsc_HD_PSls_GI_BB'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  SS_base = 'models/update/02_update_cpue'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  
  #ONLY update length FL3, 7, 10,11,13
  LL_flt <- c(3,7,10,11,13,21)
  PSfsc_flt <- c(6,16,19)
  HD_flt <- 2
  PSLS_flt <- c(8,17,20)
  GI_flt <- c(1,4,12)
  BB_flt <- c(5)
  length_df = read.csv(file.path(shrpoint_path, SS_data, 'size.csv'))
  length_df <- length_df %>%  rename_with(.cols = 1:length(names(length_df)),~ names(dat_1$lencomp))
  dat_1$lencomp <- base_dat$lencomp %>% subset(!(FltSvy %in% c(LL_flt,PSfsc_flt,HD_flt,PSLS_flt, GI_flt, BB_flt  ))) %>% 
    add_row(length_df[length_df$FltSvy %in% c(LL_flt,PSfsc_flt,HD_flt,PSLS_flt, GI_flt, BB_flt  ),])
  
  
  # Write SS files:
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  # Run model:
  r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
  
  
  #.......................................................................
  
  ### Update ONLY length LL LF PSFSC HD LS GI BB TR-------------------------------------------
  
  config_name = '03_update_cpue_updateLC_LL_LF_PSfsc_HD_PSls_GI_BB_TR'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  SS_base = 'models/update/02_update_cpue'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  
  #ONLY update length FL3, 7, 10,11,13
  LL_flt <- c(3,7,10,11,13,21)
  PSfsc_flt <- c(6,16,19)
  HD_flt <- 2
  PSLS_flt <- c(8,17,20)
  GI_flt <- c(1,4,12)
  BB_flt <- c(5)
  TR_flt <- c(9,15,18)
  length_df = read.csv(file.path(shrpoint_path, SS_data, 'size.csv'))
  length_df <- length_df %>%  rename_with(.cols = 1:length(names(length_df)),~ names(dat_1$lencomp))
  dat_1$lencomp <- base_dat$lencomp %>% subset(!(FltSvy %in% c(LL_flt,PSfsc_flt,HD_flt,PSLS_flt, GI_flt, BB_flt, TR_flt))) %>% 
    add_row(length_df[length_df$FltSvy %in% c(LL_flt,PSfsc_flt,HD_flt,PSLS_flt, GI_flt, BB_flt,  TR_flt),])
  
  
  # Write SS files:
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  
  #.......................................................................
  
  ### Update ONLY length LL LF PSFSC HD LS GI BB TR OT-------------------------------------------
  
  config_name = '03_update_cpue_updateLC_LL_LF_PSfsc_HD_PSls_GI_BB_TR_OT'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  SS_base = 'models/update/02_update_cpue'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  
  #ONLY update length FL3, 7, 10,11,13
  LL_flt <- c(3,7,10,11,13,21)
  PSfsc_flt <- c(6,16,19)
  HD_flt <- 2
  PSLS_flt <- c(8,17,20)
  GI_flt <- c(1,4,12)
  BB_flt <- c(5)
  TR_flt <- c(9,15,18)
  OT_flt <- c(4,14)
  length_df = read.csv(file.path(shrpoint_path, SS_data, 'size.csv'))
  length_df <- length_df %>%  rename_with(.cols = 1:length(names(length_df)),~ names(dat_1$lencomp))
  dat_1$lencomp <- base_dat$lencomp %>% subset(!(FltSvy %in% c(LL_flt,PSfsc_flt,HD_flt,PSLS_flt, GI_flt, BB_flt, TR_flt, OT_flt))) %>% 
    add_row(length_df[length_df$FltSvy %in% c(LL_flt,PSfsc_flt,HD_flt,PSLS_flt, GI_flt, BB_flt,  TR_flt, OT_flt),])
  
  
  # Write SS files:
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  
  
  #.......................................................................
  ### Take care of warnings --------------------------------------------
  
  config_name = '04_update_warnings'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  # SS base files path (in Sharepoint):
  SS_base = 'models/update/03_update_length'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  # fleets info: surveytime read as: 1 normally is -1 for fishing fleet
  dat_1$fleetinfo$surveytiming[1:dat_1$Nfleet] <- -1
  # Adjustment: reset upper end of F_reporting_ages to be nages-2 
  start_1$F_age_range[2] <- dat_1$Nages-2
  #Block:1 2 ends in: 336 after retroyr+1:  305
  ctl_1$Block_Design[[1]][4] <- dat_1$endyr+1
  ctl_1$Block_Design[[4]][4] <- dat_1$endyr+1
  #1st iteration warning: catch logL > 50% total logL; check configuration; suggest start with larger R0
  ctl_1$SR_parms["SR_LN(R0)","INIT"] <- 12 
  #wanning:Note 2 Information:  N parameters that are on or within 1% of min-max bound: 1; check results, variance may be suspect
  
  ctl_1$age_selex_parms["AgeSel_P_1_FISHERY14(14)","LO"] <- 0
  # Write SS files:
  
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl( ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  # Run model:
  r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
  
  #### end ####
  
  
  #.......................................................................
  ### natural mortality 0.462 (based on maximum age 11.7 IO, lower option 0.3 based on maximum age 18, atlantic) --------------------------------------------
  
  config_name = '05_update_M'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  # SS base files path (in Sharepoint):
  SS_base = 'models/update/04_update_warnings'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  #SS_base = 'models/update/05_update_M'
  #base_ctlnew = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  #ctl_new = base_ctlnew
  # #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate;_5=Maunder_M;_6=Age-range_Lorenzen
  ctl_1$natM_type <- 2
  ctl_1$Lorenzen_refage <- 4.07*4 #fully mature
  ctl_1$natM <- NULL
  ctl_1$MG_parms <- ctl_1$MG_parms %>% add_row(  ctl_1$MG_parms[1,],.before=1)
   
  #row.names(ctl_1$MG_parms )[1] <- "NatM_p_1_Fem_GP_1"
  ctl_1$MG_parms[1,1:7] <- c(0.1,0.6,0.462,0.462,0,0,-2)
  
  
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl( ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  #.......................................................................
  ### GROWTH JESSICA FARLEY  - 2 STAGE vB best otpion--------------------------------------------
  # transtiion age 0.82 years (53 cm)
  

  
  config_name = '06_update_Growth'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  #read parameters
  dataGrowth <- read.csv(file=file.path("data","ss3_inputs","FarleyGrowth_Inputss3.csv"))
  # Temporary files:
  # SS base files path (in Sharepoint):
  SS_base = 'models/update/05_update_M'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  ctl_1$Growth_Age_for_L2 <- 999
  
  ctl_1$MG_parms["L_at_Amax_Fem_GP_1","INIT"] <- 167.47
  ctl_1$MG_parms["L_at_Amin_Fem_GP_1","INIT"] <- 29.9463
  ctl_1$MG_parms["VonBert_K_Fem_GP_1","INIT"] <- dataGrowth[1,]
  ctl_1$MG_parms["Age_K_2_Fem_GP_1","INIT"] <- dataGrowth[2,]
  ctl_1$MG_parms["Age_K_3_Fem_GP_1","INIT"] <- dataGrowth[3,]
  ctl_1$MG_parms["Age_K_4_Fem_GP_1","INIT"] <- dataGrowth[4,]
  ctl_1$MG_parms["Age_K_5_Fem_GP_1","INIT"] <- dataGrowth[5,]
  ctl_1$MG_parms["Age_K_6_Fem_GP_1","INIT"] <- dataGrowth[6,]
  ctl_1$MG_parms["Age_K_7_Fem_GP_1","INIT"] <- dataGrowth[7,]
  ctl_1$MG_parms["Age_K_8_Fem_GP_1","INIT"] <- dataGrowth[8,]
  ctl_1$MG_parms["Age_K_9_Fem_GP_1","INIT"] <- dataGrowth[9,]
  ctl_1$MG_parms["Age_K_10_Fem_GP_1","INIT"] <- dataGrowth[10,]
  ctl_1$MG_parms["Age_K_11_Fem_GP_1","INIT"] <- dataGrowth[11,]
  ctl_1$MG_parms["Age_K_12_Fem_GP_1","INIT"] <- dataGrowth[12,]
  ctl_1$MG_parms["Age_K_12_Fem_GP_1","INIT"] <- dataGrowth[13,]
  
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl( ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  
  #.......................................................................
  ### GROWTH JESSICA FARLEY  - 2 STAGE vB best otpion--------------------------------------------
  # transtiion age 0.82 years (53 cm)
  
  
  
  config_name = '06b_update_GrowthTaggingData'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  #read parameters
  dataTagRelFarley <- read.csv(file=file.path("data","ss3_inputs","tag","Realeased_tag_GrowthFarley.csv"))
  dataTagRecFarley <- read.csv(file=file.path("data","ss3_inputs","tag","Recovered_tag_GrowthFarley.csv"))
  # Temporary files:
  # SS base files path (in Sharepoint):
  SS_base = 'models/update/06_update_Growth'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  names(dataTagRelFarley) <- names(dat_1$tag_releases)
  dat_1$tag_releases <- dataTagRelFarley
  names(dataTagRecFarley) <- names(dat_1$tag_recaps)
  dat_1$tag_recaps <- dataTagRecFarley
  dat_1$N_tag_groups <- max(dat_1$tag_releases$TG)
  dat_1$N_recap_events<- nrow(dat_1$tag_recaps)
  
  ctl_1$N_tag_groups <- max(dat_1$tag_releases$TG)
  ctl_1$TG_custom
  ctl_1$TG_Loss_init<- base_ctl$TG_Loss_init[1:122,]
  ctl_1$TG_Loss_chronic<- base_ctl$TG_Loss_chronic[1:122,]
  ctl_1$TG_overdispersion<- base_ctl$TG_overdispersion[1:122,]
 # ctl_1$TG_Report_fleet
 # ctl_1$TG_Report_fleet_decay
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl( ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  #.......................................................................
  ### MATURITY--------------------------------------------
  # FUNCTIONAL L50 101.7
  
  
  
  config_name = '07_update_Maturity'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  #read parameters
  dataMat <- read.csv(file=file.path("data","ss3_inputs","maturityInput_ss3.csv"))
    # Temporary files:
  # SS base files path (in Sharepoint):
  SS_base = 'models/update/06b_update_GrowthTaggingData'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  ctl_1$maturity_option <- 1
  ctl_1$First_Mature_Age <- 1
  ctl_1$Age_Maturity <- NULL
  ctl_1$MG_parms["Mat50%_Fem_GP_1","LO"] <- 50
  ctl_1$MG_parms["Mat50%_Fem_GP_1","HI"] <- 150
  ctl_1$MG_parms["Mat50%_Fem_GP_1","INIT"] <- dataMat$L50
  ctl_1$MG_parms["Mat50%_Fem_GP_1","PRIOR"] <- dataMat$L50
  ctl_1$MG_parms["Mat_slope_Fem_GP_1","LO"] <- -1
  ctl_1$MG_parms["Mat_slope_Fem_GP_1","HI"] <- 0
  ctl_1$MG_parms["Mat_slope_Fem_GP_1","INIT"] <- dataMat$slope
  ctl_1$MG_parms["Mat_slope_Fem_GP_1","PRIOR"] <- dataMat$slope 
  
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl( ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
  
  

  
  #.......................................................................
  ### Update PS selectivity --------------------------------------------
  #.......................................................................
  #remotes::install_github("r4ss/r4ss@signif") 
  config_name = '08_selectivity_PS'
  tmp_dir = file.path(shrpoint_path, SS_config, config_name)
  dir.create(tmp_dir)
  
  # Temporary files:
  # SS base files path (in Sharepoint):
  SS_base = 'models/update/07_update_Maturity'
  # Temporary files:
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  base_dat$lencomp <- base_dat$lencomp %>% subset(!(fleet==21 & year<240))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  
  
  #ESTIMATE NEW SETTINGS FOR PURSE SEINERS
  
ctl_1$size_selex_parms["SizeSel_Spline_Code_FISHERY6(6)",1:3] <- c(0,2,2)
ctl_1$size_selex_parms["SizeSel_Spline_Code_FISHERY8(8)",1:3] <- c(0,2,2)
  # Read base SS inputs (from 2021 assessment)
  
  # Write SS files:
  
  SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
  SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
  SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
  SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
  
  # Run model:
  r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-maxfn 0 -phase 50 -nohess')
  
 #after running read control file change the parameters in the control file
  SS_base = 'models/update/08_selectivity_PS'
  
   new_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
  
  
  ctl_1$size_selex_parms["SizeSel_Spline_Code_FISHERY6(6)",1:3] <- c(0,2,0)
  ctl_1$size_selex_parms["SizeSel_Spline_GradLo_FISHERY6(6)",] <- new_ctl$size_selex_parms["SizeSel_Spline_GradLo_FISHERY6(6)",]
  ctl_1$size_selex_parms["SizeSel_Spline_GradHi_FISHERY6(6)",] <-  new_ctl$size_selex_parms["SizeSel_Spline_GradHi_FISHERY6(6)",]
  ctl_1$size_selex_parms["SizeSel_Spline_Knot_1_FISHERY6(6)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_1_FISHERY6(6)",]
  ctl_1$size_selex_parms["SizeSel_Spline_Knot_2_FISHERY6(6)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_2_FISHERY6(6)",]
  ctl_1$size_selex_parms["SizeSel_Spline_Knot_3_FISHERY6(6)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_3_FISHERY6(6)",]
  ctl_1$size_selex_parms["SizeSel_Spline_Knot_4_FISHERY6(6)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_4_FISHERY6(6)",]
 ctl_1$size_selex_parms["SizeSel_Spline_Knot_5_FISHERY6(6)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_5_FISHERY6(6)",]
 ctl_1$size_selex_parms["SizeSel_Spine_Val_1_FISHERY6(6)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_1_FISHERY6(6)",] 
 ctl_1$size_selex_parms["SizeSel_Spine_Val_2_FISHERY6(6)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_2_FISHERY6(6)",] 
 ctl_1$size_selex_parms["SizeSel_Spine_Val_3_FISHERY6(6)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_3_FISHERY6(6)",] 
 ctl_1$size_selex_parms["SizeSel_Spine_Val_4_FISHERY6(6)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_4_FISHERY6(6)",] 
 ctl_1$size_selex_parms["SizeSel_Spine_Val_5_FISHERY6(6)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_5_FISHERY6(6)",]
  
 ctl_1$size_selex_parms["SizeSel_Spline_Code_FISHERY8(8)",1:3] <- c(0,2,0)
 ctl_1$size_selex_parms["SizeSel_Spline_GradLo_FISHERY8(8)",] <- new_ctl$size_selex_parms["SizeSel_Spline_GradLo_FISHERY8(8)",]
 ctl_1$size_selex_parms["SizeSel_Spline_GradHi_FISHERY8(8)",] <-  new_ctl$size_selex_parms["SizeSel_Spline_GradHi_FISHERY8(8)",]
 ctl_1$size_selex_parms["SizeSel_Spline_Knot_1_FISHERY8(8)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_1_FISHERY8(8)",]
 ctl_1$size_selex_parms["SizeSel_Spline_Knot_2_FISHERY8(8)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_2_FISHERY8(8)",]
 ctl_1$size_selex_parms["SizeSel_Spline_Knot_3_FISHERY8(8)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_3_FISHERY8(8)",]
 ctl_1$size_selex_parms["SizeSel_Spline_Knot_4_FISHERY8(8)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_4_FISHERY8(8)",]
 ctl_1$size_selex_parms["SizeSel_Spline_Knot_5_FISHERY8(8)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_5_FISHERY8(8)",]
 ctl_1$size_selex_parms["SizeSel_Spine_Val_1_FISHERY8(8)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_1_FISHERY8(8)",] 
 ctl_1$size_selex_parms["SizeSel_Spine_Val_2_FISHERY8(8)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_2_FISHERY8(8)",] 
 ctl_1$size_selex_parms["SizeSel_Spine_Val_3_FISHERY8(8)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_3_FISHERY8(8)",] 
 ctl_1$size_selex_parms["SizeSel_Spine_Val_4_FISHERY8(8)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_4_FISHERY8(8)",] 
 ctl_1$size_selex_parms["SizeSel_Spine_Val_5_FISHERY8(8)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_5_FISHERY8(8)",]
 
 
 #### end ####
  
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
 
 
 #.......................................................................
 ### Remove some of the lenght frequency that were no considered in 2021 --------------------------------------------
 #.......................................................................
 
 #.......................................................................
 ### Modify boundaries --------------------------------------------
 #.......................................................................
 #remotes::install_github("r4ss/r4ss@signif") 
 config_name = '09_boundaries'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/08_selectivity_PS'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 fleetNames <- read.csv(file=file.path("data","ss3_inputs","4A_io","FleetNames.csv"))
 dat_1$fleetnames <- fleetNames$FleetNames
 
 #ESTIMATE NEW SETTINGS FOR PURSE SEINERS
 
 ctl_1$age_selex_parms["AgeSel_P_1_FISHERY13(13)",1] <- 3
 ctl_1$age_selex_parms["AgeSel_P_2_FISHERY3(3)",1] <- 0
 ctl_1$age_selex_parms["AgeSel_P_1_FISHERY5(5)",1] <- 0
 ctl_1$age_selex_parms["AgeSel_P_2_FISHERY7(7)",1] <- 0 
 ctl_1$age_selex_parms["AgeSel_P_2_FISHERY7(7)",2] <- 8 
 ctl_1$age_selex_parms["AgeSel_P_2_FISHERY10(10)",2] <- 8 
 ctl_1$age_selex_parms["AgeSel_P_1_FISHERY10(10)",1] <- 6 
 ctl_1$age_selex_parms["AgeSel_P_1_FISHERY10(10)",2] <- 20 
 ctl_1$age_selex_parms["AgeSel_P_1_FISHERY4(4)",1] <- -5 
 ctl_1$age_selex_parms["AgeSel_P_1_FISHERY9(9)",1] <- 0 
 ctl_1$age_selex_parms["AgeSel_P_1_FISHERY9(9)",1]  <- 0 
 ctl_1$age_selex_parms["AgeSel_P_2_FISHERY10(10)",1] <- 0   
 ctl_1$age_selex_parms["AgeSel_P_1_FISHERY11(11)",2] <- 20  
 ctl_1$age_selex_parms["AgeSel_P_1_FISHERY12(12)",1] <- 0  
 ctl_1$age_selex_parms["AgeSel_P_1_FISHERY12(12)",2] <- 14 
 ctl_1$age_selex_parms["AgeSel_P_3_FISHERY12(12)",2] <- 8
 ctl_1$age_selex_parms["AgeSel_P_1_FISHERY7(7)",2] <- 20
 ctl_1$age_selex_parms["AgeSel_P_1_FISHERY13(13)",1:2] <- c(4,20)
 ctl_1$age_selex_parms["AgeSel_P_2_FISHERY13(13)",1:2] <- c(0,8)
 ctl_1$age_selex_parms["AgeSel_P_2_FISHERY21(21)",1] <- 0
 ctl_1$age_selex_parms["AgeSel_P_1_FISHERY14(14)",1] <- -5 
 ctl_1$age_selex_parms["AgeSel_P_1_FISHERY1(1)",1:2] <- c(0,16) 
 ctl_1$age_selex_parms["AgeSel_P_2_FISHERY2(2)",1] <- 0
 ctl_1$age_selex_parms["AgeSel_P_1_FISHERY3(3)",2] <- 20
 ctl_1$age_selex_parms_tv["AgeSel_P_1_FISHERY1(1)_BLK1repl_213",1:2] <- c(-2,16)
 ctl_1$age_selex_parms_tv["AgeSel_P_1_FISHERY1(1)_BLK1repl_261",1:2] <- c(-2,16)
 # Read base SS inputs (from 2021 assessment)
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-maxfn 0 -phase 50 -nohess')
 
 
 
 #.......................................................................
 ### Modify boundaries --------------------------------------------
 #.......................................................................
 #remotes::install_github("r4ss/r4ss@signif") 
 config_name = '10_recDevs'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/09_boundaries'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 fleetNames <- read.csv(file=file.path("data","ss3_inputs","4A_io","FleetNames.csv"))
 dat_1$fleetnames <- fleetNames$FleetNames
 
 #ESTIMATE NEW SETTINGS FOR PURSE SEINERS
 
 ctl_1$MG_parms["RecrDist_GP_1_area_4_month_1",11] <- 296
 ctl_1$MainRdevYrLast <- 296
 ctl_1$last_yr_fullbias_adj <-299
 ctl_1$first_recent_yr_nobias_adj <- 304
 # Read base SS inputs (from 2021 assessment)
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-maxfn 0 -phase 50 -nohess')
 
 
 
 #.......................................................................
 ### Adding report quality --------------------------------------------
 #.......................................................................
 #remotes::install_github("r4ss/r4ss@signif") 
 config_name = '11_RQ'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/10_recDevs'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 fleetNames <- read.csv(file=file.path("data","ss3_inputs","4A_io","FleetNames.csv"))
 dat_1$fleetnames <- fleetNames$FleetNames
 
 length_df = read.csv(file.path(shrpoint_path, SS_data, 'size-original.csv'))
 dat_1$lencomp <- length_df #%>% mutate(Nsamp=-5/6*Nsamp+5) #%>% subset(!(Yr<230 & ModelFleet==21))
 dat_1$lencomp$Nsamp <- ifelse(length_df$Nsamp<=2,5,2)
 
 #ESTIMATE NEW SETTINGS FOR PURSE SEINERS
 

 # Read base SS inputs (from 2021 assessment)
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-maxfn 0 -phase 50 -nohess')
 
 
 ### Adding regular grig  --------------------------------------------
 #.......................................................................
 #remotes::install_github("r4ss/r4ss@signif") 
 config_name = '12_cwp5x5'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/10_recDevs'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 fleetNames <- read.csv(file=file.path("data","ss3_inputs","4A_io","FleetNames.csv"))
 dat_1$fleetnames <- fleetNames$FleetNames
 
 length_df = read.csv(file.path(shrpoint_path, SS_data, 'size-cwp55.csv'))
 dat_1$lencomp <- length_df #%>% mutate(Nsamp=-5/6*Nsamp+5) #%>% subset(!(Yr<230 & ModelFleet==21))
 dat_1$lencomp$Nsamp <- 5
 
 #ESTIMATE NEW SETTINGS FOR PURSE SEINERS
 
 
 # Read base SS inputs (from 2021 assessment)
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-maxfn 0 -phase 50 -nohess')
 
 
 
 ### Adding regular grig and report quality --------------------------------------------
 #.......................................................................
 #remotes::install_github("r4ss/r4ss@signif") 
 config_name = '12_cwp5x5_RQ'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/10_recDevs'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 fleetNames <- read.csv(file=file.path("data","ss3_inputs","4A_io","FleetNames.csv"))
 dat_1$fleetnames <- fleetNames$FleetNames
 
 length_df = read.csv(file.path(shrpoint_path, SS_data, 'size-cwp55.csv'))
 dat_1$lencomp <- length_df #%>% mutate(Nsamp=-5/6*Nsamp+5) #%>% subset(!(Yr<230 & ModelFleet==21))
 dat_1$lencomp$Nsamp <- ifelse(length_df$Nsamp<=2,5,2)
 
 #ESTIMATE NEW SETTINGS FOR PURSE SEINERS
 
 
 # Read base SS inputs (from 2021 assessment)
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-maxfn 0 -phase 50 -nohess')
 
 
 ### Adding regular grig and report quality --------------------------------------------
 #.......................................................................
 #remotes::install_github("r4ss/r4ss@signif") 
 config_name = '12_cwp5x5_RQ'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/12_cwp5x5_RQ'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 fleetNames <- read.csv(file=file.path("data","ss3_inputs","4A_io","FleetNames.csv"))
 dat_1$fleetnames <- fleetNames$FleetNames
 
 length_df = read.csv(file.path(shrpoint_path, SS_data, 'size-cwp55.csv'))
 dat_1$lencomp <- length_df #%>% mutate(Nsamp=-5/6*Nsamp+5) #%>% subset(!(Yr<230 & ModelFleet==21))
 dat_1$lencomp$Nsamp <- ifelse(length_df$Nsamp<=2,5,2)
 
 #ESTIMATE NEW SETTINGS FOR PURSE SEINERS
 
 
 # Read base SS inputs (from 2021 assessment)
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-maxfn 0 -phase 50 -nohess')
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 #.......................................................................
 ### Update PS selectivity --------------------------------------------
 #.......................................................................
 #remotes::install_github("r4ss/r4ss@signif") 
 config_name = '09_ReportQuality'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/08_selectivity_PS'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 length_df = read.csv(file.path(shrpoint_path, SS_data, 'size_irregular-RQ-smalEq2.csv'))
 length_df$Nsamp=ifelse(length_df$Nsamp==0,2.5,5)

 dat_1$lencomp <- length_df %>% subset(!is.na(ModelFleet )) %>% select(!yrqtr)
 fleetNames <- read.csv(file=file.path("data","ss3_inputs","4A_io","FleetNames.csv"))
 dat_1$fleetnames <- fleetNames$FleetNames

 # Read base SS inputs (from 2021 assessment)
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 ### Update PS selectivity --------------------------------------------
 #.......................................................................
 
 config_name = '09_IrregularSize'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/08_selectivity_PS'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 length_df = read.csv(file.path(shrpoint_path, SS_data, 'size-irregular.csv'))
 #length_df$Nsamp=ifelse(length_df$Nsamp==0,2.5,5)
 
 dat_1$lencomp <- length_df %>% subset(!is.na(ModelFleet ))
 fleetNames <- read.csv(file=file.path("data","ss3_inputs","4A_io","FleetNames.csv"))
 dat_1$fleetnames <- fleetNames$FleetNames
 
 # Read base SS inputs (from 2021 assessment)
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
