#
#       STEPWISE APPROACH
#       start- 00_BC -4A_io_lin_v33022_FixedParam2_Fl11
#
#............................................................
#remotes::install_github("r4ss/r4ss@signif") 
remotes::install_github("r4ss/r4ss",force=TRUE)
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



spat_config = '4A_io'
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
  
  # Read base SS inputs (from 2021 assessment)
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  # #fleet names   NOT WORKING
   fish_names = get_fisheries(spat_config)$fleet_name
   fish_names = gsub(pattern = ' ', replacement = '_', x = fish_names)
   dat_1$fleetinfo$fleetname <- paste0(1:25,"_",c(fish_names,c(fish_names[c(7,10,11,13)])))

  # Updated catch data frame:
  catch_df = read.csv(file.path(shrpoint_path, SS_data, 'catch.csv'))
  updated_catch = data.frame(year = catch_df[,'qtr'], seas = 1, fleet = catch_df[,'ModelFleet'], 
                             catch = catch_df[,'Catch'], catch_se = 0.01)
  updated_catch = updated_catch %>% add_row(data.frame(year = -999, seas = 1, fleet = 1, catch = 0, catch_se = 0.01), .before = 1) # shouldnt we also add for all fleets?
  dat_1$catch = updated_catch
  # Last year = 2022:
  dat_1$endyr = 308
  
  #FORECAST
  #_Bmark_years: beg_bio, end_bio, beg_selex, end_selex, beg_relF, end_relF,
  #beg_recr_dist, end_recr_dist, beg_SRparm, end_SRparm (enter actual year, or values of 0 or -integer to be rel. endyr
  fore_1$Bmark_years<- c(308,308,301,308,301,308,296,308,13,308)
  
  #_Fcast_years for averaging:  beg_selex, end_selex, beg_relF, end_relF, beg_mean recruits,
  #end_recruits  (enter actual year, or values of 0 or -integer to be rel. endyr)
  
  fore_1$Fcast_years <- c(308,308,308,308,13,308) 
  fore_1$FirstYear_for_caps_and_allocations <- 309
  fore_1$Ydecl <- 308
  fore_1$Yinit <- 308
  
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
  SS_base = 'models/update/01_update_catch'
  
  base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
  base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
  base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
  base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
  
  # Updated catch data frame:
    
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start

  
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
  
  config_name = '03_update_only_length_until_308'
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
  config_name = 'sensitivites_LengthData/03_update_cpue_updateLC_LL'
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
  
  ctl_1$age_selex_parms["AgeSel_P_1_14_OT_4(14)","LO"] <- 0
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
  row.names(ctl_1$MG_parms[1,]) <-c("NatM_Lorenzen_Fem_GP_1")
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
  
  dat_1$Nages <- 40
  start_1$F_age_range[2] <- 40-2
  #extendeing maturity at age until age 40
    
  extMat<- c(base_ctl$Age_Maturity,rep(1,40-28))
  names(extMat) <- paste0("Age_",0:40)
  extMat <- as.data.frame(extMat)
  ctl_1$Age_Maturity <- extMat
  
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
  
  base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
  
  dat_1 = base_dat
  ctl_1 = base_ctl
  fore_1 = base_fore
  start_1 = base_start
  
  
  
  #ESTIMATE NEW SETTINGS FOR PURSE SEINERS
  
ctl_1$size_selex_parms["SizeSel_Spline_Code_6_FS_1b(6)",1:3] <- c(0,2,2)
ctl_1$size_selex_parms["SizeSel_Spline_Code_8_LS_1b(8)",1:3] <- c(0,2,2)
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
  
  
  ctl_1$size_selex_parms["SizeSel_Spline_Code_6_FS_1b(6)",1:3] <- c(0,2,0)
  ctl_1$size_selex_parms["SizeSel_Spline_GradLo_6_FS_1b(6)",] <- new_ctl$size_selex_parms["SizeSel_Spline_GradLo_6_FS_1b(6)",]
  ctl_1$size_selex_parms["SizeSel_Spline_GradHi_6_FS_1b(6)",] <-  new_ctl$size_selex_parms["SizeSel_Spline_GradHi_6_FS_1b(6)",]
  ctl_1$size_selex_parms["SizeSel_Spline_Knot_1_6_FS_1b(6)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_1_6_FS_1b(6)",]
  ctl_1$size_selex_parms["SizeSel_Spline_Knot_2_6_FS_1b(6)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_2_6_FS_1b(6)",]
  ctl_1$size_selex_parms["SizeSel_Spline_Knot_3_6_FS_1b(6)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_3_6_FS_1b(6)",]
  ctl_1$size_selex_parms["SizeSel_Spline_Knot_4_6_FS_1b(6)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_4_6_FS_1b(6)",]
 ctl_1$size_selex_parms["SizeSel_Spline_Knot_5_6_FS_1b(6)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_5_6_FS_1b(6)",]
 ctl_1$size_selex_parms["SizeSel_Spine_Val_1_6_FS_1b(6)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_1_6_FS_1b(6)",] 
 ctl_1$size_selex_parms["SizeSel_Spine_Val_2_6_FS_1b(6)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_2_6_FS_1b(6)",] 
 ctl_1$size_selex_parms["SizeSel_Spine_Val_3_6_FS_1b(6)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_3_6_FS_1b(6)",] 
 ctl_1$size_selex_parms["SizeSel_Spine_Val_4_6_FS_1b(6)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_4_6_FS_1b(6)",] 
 ctl_1$size_selex_parms["SizeSel_Spine_Val_5_6_FS_1b(6)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_5_6_FS_1b(6)",]
  
 ctl_1$size_selex_parms["SizeSel_Spline_Code_8_LS_1b(8)",1:3] <- c(0,2,0)
 ctl_1$size_selex_parms["SizeSel_Spline_GradLo_8_LS_1b(8)",] <- new_ctl$size_selex_parms["SizeSel_Spline_GradLo_8_LS_1b(8)",]
 ctl_1$size_selex_parms["SizeSel_Spline_GradHi_8_LS_1b(8)",] <-  new_ctl$size_selex_parms["SizeSel_Spline_GradHi_8_LS_1b(8)",]
 ctl_1$size_selex_parms["SizeSel_Spline_Knot_1_8_LS_1b(8)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_1_8_LS_1b(8)",]
 ctl_1$size_selex_parms["SizeSel_Spline_Knot_2_8_LS_1b(8)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_2_8_LS_1b(8)",]
 ctl_1$size_selex_parms["SizeSel_Spline_Knot_3_8_LS_1b(8)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_3_8_LS_1b(8)",]
 ctl_1$size_selex_parms["SizeSel_Spline_Knot_4_8_LS_1b(8)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_4_8_LS_1b(8)",]
 ctl_1$size_selex_parms["SizeSel_Spline_Knot_5_8_LS_1b(8)",] <- new_ctl$size_selex_parms["SizeSel_Spline_Knot_5_8_LS_1b(8)",]
 ctl_1$size_selex_parms["SizeSel_Spine_Val_1_8_LS_1b(8)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_1_8_LS_1b(8)",] 
 ctl_1$size_selex_parms["SizeSel_Spine_Val_2_8_LS_1b(8)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_2_8_LS_1b(8)",] 
 ctl_1$size_selex_parms["SizeSel_Spine_Val_3_8_LS_1b(8)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_3_8_LS_1b(8)",] 
 ctl_1$size_selex_parms["SizeSel_Spine_Val_4_8_LS_1b(8)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_4_8_LS_1b(8)",] 
 ctl_1$size_selex_parms["SizeSel_Spine_Val_5_8_LS_1b(8)",] <- new_ctl$size_selex_parms["SizeSel_Spine_Val_5_8_LS_1b(8)",]
 
 
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
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 #ESTIMATE NEW SETTINGS FOR PURSE SEINERS
 # ctl_1$age_selex_parms["AgeSel_P_1_13_LL_4(13)",1] <- 3
 # ctl_1$age_selex_parms["AgeSel_P_2_3_LL_1a(3)",1] <- 0
 # ctl_1$age_selex_parms["AgeSel_P_1_5_BB_1b(5)",1] <- 0
 # ctl_1$age_selex_parms["AgeSel_P_2_7_LL_1b(7)",1] <- 0 
 # ctl_1$age_selex_parms["AgeSel_P_2_7_LL_1b(7)",2] <- 8 
 # ctl_1$age_selex_parms["AgeSel_P_2_10_LL_2(10)",2] <- 8 
 # ctl_1$age_selex_parms["AgeSel_P_1_10_LL_2(10)",1] <- 6 
 # ctl_1$age_selex_parms["AgeSel_P_1_10_LL_2(10)",2] <- 20 
 # ctl_1$age_selex_parms["AgeSel_P_1_4_OT_1a(4)",1] <- -5 
 # ctl_1$age_selex_parms["AgeSel_P_1_9_TR_1b(9)",1] <- 0 
 # ctl_1$age_selex_parms["AgeSel_P_1_9_TR_1b(9)",1]  <- 0 
 # ctl_1$age_selex_parms["AgeSel_P_2_10_LL_2(10)",1] <- 0   
 # ctl_1$age_selex_parms["AgeSel_P_1_10_LL_3(11)",2] <- 20  
 # ctl_1$age_selex_parms["AgeSel_P_1_12_GI_4(12)",1] <- 0  
 # ctl_1$age_selex_parms["AgeSel_P_1_12_GI_4(12)",2] <- 14 
 # ctl_1$age_selex_parms["AgeSel_P_3_12_GI_4(12)",2] <- 8
 # ctl_1$age_selex_parms["AgeSel_P_1_7_LL_1b(7)",2] <- 20
 # ctl_1$age_selex_parms["AgeSel_P_1_13_LL_4(13)",1:2] <- c(4,20)
 # ctl_1$age_selex_parms["AgeSel_P_2_13_LL_4(13)",1:2] <- c(0,8)
 # ctl_1$age_selex_parms["AgeSel_P_2_21_LF_4(21)",1] <- 0
 # ctl_1$age_selex_parms["AgeSel_P_1_14_OT_4(14)",1] <- -5 
 # ctl_1$age_selex_parms["AgeSel_P_1_1_GI_1a(1)",1:2] <- c(0,16) 
 # ctl_1$age_selex_parms["AgeSel_P_2_2_HD_1a(2)",1] <- 0
 # ctl_1$age_selex_parms["AgeSel_P_1_3_LL_1a(3)",2] <- 20
 # ctl_1$age_selex_parms_tv["AgeSel_P_1_1_GI_1a(1)_BLK1repl_213",1:2] <- c(-2,16)
 # ctl_1$age_selex_parms_tv["AgeSel_P_1_1_GI_1a(1)_BLK1repl_261",1:2] <- c(-2,16)
 # Read base SS inputs (from 2021 assessment)
 
 idx <- 1:length(ctl_1$age_selex_parms$LO)
 for(i in idx){
     pr <- base_ctl$age_selex_parms[i,3]
     sd <- abs(0.2*pr)
     ctl_1$age_selex_parms[i,c(1:5)] <- c(min(pr-4*sd,pr+4*sd),max(pr-4*sd,pr+4*sd),pr,pr,sd)
      }
 

 idxRep <- grep(paste0("1_GI_1a",collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:2] <- c(-2,2)
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 
 
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
 

 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start

 ctl_1$MG_parms["RecrDist_GP_1_area_4_month_1",11] <- 300
 ctl_1$MainRdevYrLast <- 300
 ctl_1$last_yr_fullbias_adj <-303
 ctl_1$first_recent_yr_nobias_adj <- 308
 # Read base SS inputs (from 2021 assessment)
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
 
 
 
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
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
 
 
 ### Adding regular grig  ####
 #### Adding LC data 5x5 ####
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
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
 
 
 
 ### Adding regular grig and report quality --------------------------------------------

 config_name = '13_cwp5x5_RQ'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/12_cwp5x5'
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
 

 length_df = read.csv(file.path(shrpoint_path, SS_data, 'size-cwp55.csv'))
 dat_1$lencomp <- length_df #%>% mutate(Nsamp=-5/6*Nsamp+5) #%>% subset(!(Yr<230 & ModelFleet==21))
 dat_1$lencomp$Nsamp <- ifelse(length_df$Nsamp<=2,5,2)
 
 
 
 idxRep <- grep(paste0("1_GI_1a",collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:2] <- c(-2.5,2)
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 
 #### Making free the parameter LL That was fixed ####
 
 config_name = '14_cwp5x5_RQ_LL11_p2_free'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/13_cwp5x5_RQ'
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
 
 
 idxRep <- grep(paste0("LL_3",collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[2],7] <- 3
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 #### biased adjust  ####
 #...........................................
 
 config_name = '15_BiasCorrectionRamp_hess'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/14_cwp5x5_RQ_LL11_p2_free_hess'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 
 idxRep <- grep(paste0("LL_3",collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[2],7] <- 3
 
 ctl_1$last_early_yr_nobias_adj  <- 69.5
 ctl_1$first_yr_fullbias_adj <- 166.2
 ctl_1$last_yr_fullbias_adj <- 304.4
 ctl_1$first_recent_yr_nobias_adj <- 308.7
 ctl_1$max_bias_adj <- 0.669
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')

 #....................................................
 #### DIVINDING IN 2 THE FLEETS 7,10 AND 13 THE YEAR 213-2000  ####
 #.......................................................................
 
 
 config_name = 'sensitivities_16/16_NsampLL5_LL_log'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/13_cwp5x5_RQ'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
  #ctl_1$age_selex_parms["AgeSel_P_2_FISHERY11(11)",]$PHASE <-3
dat_1$Nfleets <- 28
newfleets <- base_dat$fleetinfo[base_dat$fleetinfo$fleetname %in% base_dat$fleetinfo$fleetname[c(7,10,13)],]
newfleets$fleetname <- c("22_LL_1b_A2000","23_LL_2_A2000","24_LL_4_A2000")
row.names(newfleets) <- c("22","23","24")

#fleetinfo

dat_1$fleetinfo$fleetname[c(22:25)] <- paste0(25:28,"_CPUE_",c("LL_1b","LL_2","LL_3","LL_4"))

dat_1$fleetinfo <- rbind(base_dat$fleetinfo[1:21,],
                         newfleets,
                         dat_1$fleetinfo[22:25,])

row.names(dat_1$fleetinfo)[25:28] <- as.character(25:28)
dat_1$fleetinfo$fleetname[c(7,10,13)] <-  c("7_LL_1b_P2000","10_LL_2_P2000","13_LL_4_P2000")

#fleetinfo1

newfleets <- dat_1$fleetinfo1[,c(7,10,13)]
names(newfleets)<- dat_1$fleetinfo$fleetname[22:24]

dat_1$fleetinfo1 <- cbind(base_dat$fleetinfo1[,1:21],
                         newfleets,
                         base_dat$fleetinfo1[,22:25])
names(dat_1$fleetinfo1) <- dat_1$fleetinfo$fleetname

#fleetinfo2

newfleets <- dat_1$fleetinfo2[,c(7,10,13)]
names(newfleets)<- dat_1$fleetinfo$fleetname[c(22:24)]

dat_1$fleetinfo2 <- cbind(base_dat$fleetinfo2[,1:21],
                          newfleets,
                          base_dat$fleetinfo2[,22:25])
names(dat_1$fleetinfo2) <- dat_1$fleetinfo$fleetname

#catch

dat_1$catch$fleet[dat_1$catch$fleet==13 & dat_1$catch$year>=213] <- 24
dat_1$catch$fleet[dat_1$catch$fleet==10 & dat_1$catch$year>=213] <- 23
dat_1$catch$fleet[dat_1$catch$fleet==7 & dat_1$catch$year>=213] <- 22

#checking
sum(base_dat$catch$catch)-sum(dat_1$catch$catch)
sum(base_dat$catch$year)-sum(dat_1$catch$year)

#write.csv(dat_1$catch,file=file.path("data","ss3_inputs","4A_io","catch_28fleets.csv"),row.names=FALSE)


#cpue
CPUEinfonew <- base_dat$CPUEinfo[c(7,10,13),]
CPUEinfonew$fleet <- 22:24
row.names(CPUEinfonew) <- dat_1$fleetinfo$fleetname[22:24]

CPUEinfonew2 <- base_dat$CPUEinfo[22:25,]
CPUEinfonew2$fleet <- 25:28

dat_1$CPUEinfo<- rbind(base_dat$CPUEinfo[1:21,],
                        CPUEinfonew,
                        CPUEinfonew2)
row.names(dat_1$CPUEinfo) <- dat_1$fleetinfo$fleetname

dat_1$CPUE$index[dat_1$CPUE$index==25 ] <- 28
dat_1$CPUE$index[dat_1$CPUE$index==24 ] <- 27
dat_1$CPUE$index[dat_1$CPUE$index==23 ] <- 26
dat_1$CPUE$index[dat_1$CPUE$index==22 ] <- 25

#write.csv(dat_1$CPUE,file=file.path("data","ss3_inputs","4A_io","cpue_28fleets.csv"),row.names=FALSE)

#len_info

newfleets <- base_dat$len_info[c(7,10,13),]
row.names(newfleets) <- dat_1$fleetinfo$fleetname[22:24]
row.names(base_dat$len_info)
dat_1$len_info<- rbind(base_dat$len_info[1:21,], newfleets, base_dat$len_info[22:25,]) 
row.names(dat_1$len_info) <- dat_1$fleetinfo$fleetname
#LENCOMP

dat_1$lencomp$fleet[dat_1$lencomp$fleet==13 & dat_1$lencomp$year>=213] <- 24
dat_1$lencomp$fleet[dat_1$lencomp$fleet==10 & dat_1$lencomp$year>=213] <- 23
dat_1$lencomp$fleet[dat_1$lencomp$fleet==7 & dat_1$lencomp$year>=213] <- 22

summary(dat_1$lencomp)
dat_1$lencomp$Nsamp[dat_1$lencomp$fleet==3] <- 5
dat_1$lencomp$Nsamp[dat_1$lencomp$fleet==7] <- 5
dat_1$lencomp$Nsamp[dat_1$lencomp$fleet==10] <- 5
dat_1$lencomp$Nsamp[dat_1$lencomp$fleet==11] <- 5
dat_1$lencomp$Nsamp[dat_1$lencomp$fleet==13] <- 5

#write.csv(dat_1$lencomp,file=file.path("data","ss3_inputs","4A_io","lencomp_28fleets.csv"),row.names=FALSE)

#TG

dat_1$tag_recaps$fleet[dat_1$tag_recaps$fleet==13 & dat_1$tag_recaps$year>=213] <- 24
dat_1$tag_recaps$fleet[dat_1$tag_recaps$fleet==10 & dat_1$tag_recaps$year>=213] <- 23
dat_1$tag_recaps$fleet[dat_1$tag_recaps$fleet==7 & dat_1$tag_recaps$year>=213] <- 22

sum(dat_1$tag_recaps$Nrecap) - sum(dat_1$tag_recaps$Nrecap)

#write.csv(dat_1$tag_recaps,file=file.path("data","ss3_inputs","4A_io","tag_recaps_28fleets.csv"),row.names=FALSE)

#CONTROL

#Q_options
ctl_1$Q_options$fleet <- c(25:28)
ctl_1$Q_options$link_info[2:4] <- 25
ctl_1$Q_options

#Q_parms
row.names(ctl_1$Q_parms) <- paste0("LNQ_base_",dat_1$fleetinfo$fleetname[25:28], "(",25:28,")")
ctl_1$Q_parms


#size_selex_types

newfleets <-rbind(base_ctl$size_selex_types[c(7,10,13),])
row.names(newfleets) <- dat_1$fleetinfo$fleetname[c(22:24)]

ctl_1$size_selex_types <- rbind(base_ctl$size_selex_types[1:21,],
                               newfleets,
                               base_ctl$size_selex_types[22:25,])
row.names(ctl_1$size_selex_types) <- dat_1$fleetinfo$fleetname

#age_selex_types

newfleets <-rbind(base_ctl$age_selex_types[c(7,10,13),])
row.names(newfleets) <- dat_1$fleetinfo$fleetname[c(22:24)]

ctl_1$age_selex_types <- rbind(base_ctl$age_selex_types[1:21,],
                          newfleets,
                          base_ctl$age_selex_types[22:25,])
row.names(ctl_1$age_selex_types) <- dat_1$fleetinfo$fleetname

# 
# idx <- grep(paste0(c("FISHERY7","FISHERY10","FISHERY13"),collapse="|"), 
#             row.names(ctl_1$age_selex_types), value=FALSE)
# ctl_1$age_selex_types[idx,1] <- 12 #logistic

#parms

idx <- grep(paste0(base_dat$fleetinfo$fleetname[c(7,10,13)],collapse="|"), 
            row.names(base_ctl$age_selex_parms), value=FALSE)

newfleets <- base_ctl$age_selex_parms[idx,]
row.names(newfleets) <- gsub("13",  "24",row.names(newfleets))
row.names(newfleets) <- gsub("10",  "23",row.names(newfleets))
row.names(newfleets) <- gsub("7",  "22",row.names(newfleets))

row.names(newfleets) <- gsub("LL_4",  "LL_4_A2000",row.names(newfleets))
row.names(newfleets) <- gsub("LL_2",  "LL_2_A2000",row.names(newfleets))
row.names(newfleets) <- gsub("LL_1b",  "LL_1b_A2000",row.names(newfleets))

 

idx7 <- grep(paste0(c(base_dat$fleetinfo$fleetname[1:6]),collapse="|"), 
             row.names(base_ctl$age_selex_parms), value=FALSE)

idx10 <- grep(paste0(c(base_dat$fleetinfo$fleetname[8:9]),collapse="|"), 
              row.names(base_ctl$age_selex_parms), value=FALSE)
idx13 <- grep(paste0(c(base_dat$fleetinfo$fleetname[11:12]),collapse="|"), 
              row.names(base_ctl$age_selex_parms), value=FALSE)
idxH13 <- grep(paste0(c(base_dat$fleetinfo$fleetname[14:21]),collapse="|"), 
               row.names(base_ctl$age_selex_parms), value=FALSE)

idxRep <- grep(paste0(c(base_dat$fleetinfo$fleetname[21]),collapse="|"), 
               row.names(base_ctl$age_selex_parms), value=FALSE)

parm7 <- base_ctl$age_selex_parms[idxRep,]

pr1 <- 8.17171
sd1 <- pr1*0.2
pr2 <- 1.63616
sd2 <- pr2*0.2
parm7[1,1:5] <- c(pr1-4*sd1,pr1+4*sd1,pr1,pr1,sd1)
parm7[2,1:5] <- c(pr2-4*sd2,pr+4*sd2,pr2,pr2,sd2)
row.names(parm7) <- gsub("21","7", row.names(parm7))
row.names(parm7) <- gsub("LF_4","LL_1b_P2000", row.names(parm7))


parm10 <- base_ctl$age_selex_parms[idxRep,]
pr1 <- 7.77856
sd1 <- pr1*0.2
pr2 <- 1.24968
sd2 <- pr2*0.2
parm10[1,1:5] <- c(pr1-4*sd1,pr1+4*sd1,pr1,pr1,sd1)
parm10[2,1:5] <- c(pr2-4*sd2,pr+4*sd2,pr2,pr2,sd2)
row.names(parm10) <- gsub("21","10", row.names(parm10))
row.names(parm10) <- gsub("LF_4","LL_2_P2000", row.names(parm10))

parm13 <- base_ctl$age_selex_parms[idxRep,]
pr1 <- 7.56423
sd1 <- pr1*0.2
pr2 <- 1.95029
sd2 <- pr2*0.2
parm13[1,1:5] <- c(pr1-4*sd1,pr1+4*sd1,pr1,pr1,sd1)
parm13[2,1:5] <- c(pr2-4*sd2,pr+4*sd2,pr2,pr2,sd2)

row.names(parm13) <- gsub("21","13", row.names(parm13))
row.names(parm13) <- gsub("LF_4","LL_4_P2000", row.names(parm13))

new_age_selex_parms <- rbind(base_ctl$age_selex_parms[idx7,],#1:6
                             parm7,  #7
                             base_ctl$age_selex_parms[idx10,],  #8:9
                             parm10,  #10
                             base_ctl$age_selex_parms[idx13,],  #11:12
                             parm13, #13
                             base_ctl$age_selex_parms[idxH13,], #13
                             newfleets)
#write.csv(new_age_selex_parms, file=file.path("data","ss3_inputs","4A_io","age_selex_parms.csv"),row.names=TRUE)
          
ctl_1$age_selex_parms <- new_age_selex_parms

#TAG
dat_1$tag_recaps$fleet[dat_1$tag_recaps$fleet==21]
tg_rep7 <- base_ctl$TG_Report_fleet[21,]
row.names(tg_rep7) <-gsub("21","7",row.names(tg_rep7))
tg_rep10 <- base_ctl$TG_Report_fleet[21,]
row.names(tg_rep10) <-gsub("21","10",row.names(tg_rep10))
tg_rep13 <- base_ctl$TG_Report_fleet[21,]
row.names(tg_rep13) <-gsub("21","13",row.names(tg_rep13))

tg_rep22 <- base_ctl$TG_Report_fleet[7,]
row.names(tg_rep22) <-gsub("7","22",row.names(tg_rep22))
tg_rep23 <- base_ctl$TG_Report_fleet[10,]
row.names(tg_rep23) <-gsub("10","23",row.names(tg_rep23))
tg_rep24 <- base_ctl$TG_Report_fleet[13,]
row.names(tg_rep24) <-gsub("13","24",row.names(tg_rep24))


tag_recaps_new <- rbind(base_ctl$TG_Report_fleet[1:6,],
                        tg_rep7,
                        base_ctl$TG_Report_fleet[8:9,],
                        tg_rep10,
                        base_ctl$TG_Report_fleet[11:12,],
                        tg_rep13,
                        base_ctl$TG_Report_fleet[14:21,],
                        tg_rep22,
                        tg_rep23,
                        tg_rep24 )
#write.csv(tag_recaps_new, file=file.path("data","ss3_inputs","4A_io","ctl_tag_recaps_28fleets.csv"),row.names=TRUE)
ctl_1$TG_Report_fleet <- tag_recaps_new


new_fleet_decay <- base_ctl$TG_Report_fleet_decay[c(7,10,13),]
row.names(new_fleet_decay) <- gsub("13","24", row.names(new_fleet_decay))
row.names(new_fleet_decay) <- gsub("10","23", row.names(new_fleet_decay))
row.names(new_fleet_decay) <- gsub("7","22", row.names(new_fleet_decay))

ctl_1$TG_Report_fleet_decay <- rbind(base_ctl$TG_Report_fleet_decay,
                                     new_fleet_decay)



 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 
 
 #....................................................
 #### DIVINDING IN 2 THE FLEETS 7,10 AND 13 THE YEAR 213-2000 AND FLEET 7 DN  ####
 #.......................................................................
 
 
 config_name = 'sensitivities_16/16_NsampLL5_LL_log_LL1b_DN'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/sensitivities_16/16_NsampLL5_LL_log'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 
 idx <- grep(paste0(base_dat$fleetinfo$fleetname[7],collapse="|"), 
             row.names(ctl_1$age_selex_types), value=FALSE)
 ctl_1$age_selex_types[idx,1] <- 20 #DN
 
 
 idx <- grep(paste0(base_dat$fleetinfo$fleetname[7],collapse="|"), 
             row.names(ctl_1$age_selex_parms), value=FALSE)
 
 newfleets <- base_ctl$age_selex_parms[idx,]

 idxRep <- grep(paste0(base_dat$fleetinfo$fleetname[14],collapse="|"), 
                row.names(base_ctl$age_selex_parms), value=FALSE)
 
 base_ctl$age_selex_parms[idxRep,] 
 
 idx7 <- grep(paste0(c(base_dat$fleetinfo$fleetname[1:6]),collapse="|"), 
              row.names(base_ctl$age_selex_parms), value=FALSE)
 idxH7 <- grep(paste0(c(base_dat$fleetinfo$fleetname[8:24]),collapse="|"), 
               row.names(base_ctl$age_selex_parms), value=FALSE)
 
 parm <- base_ctl$age_selex_parms[idxRep,]
 
 pr1 <- 10
 sd1 <- pr1*0.2
 pr2 <-  -0.85
 sd2 <- abs(pr2*0.2)
 pr3 <- -1.4
 sd3 <- abs(pr3*0.2)
 pr4 <- -0.64
 sd4 <- pr4*0.2 
 pr5 <- -6
 sd5 <- abs(pr5*0.2)
 pr6 <- -8.5
 sd6 <- abs(pr6*0.2)
 
 parm[1,1:6] <- c(min(pr1-4*sd1,pr1+4*sd1),max(pr1-4*sd1,pr1+4*sd1),pr1,pr1,sd1,0)
 parm[2,1:6] <- c(min(pr2-4*sd2,pr2+4*sd2),max(pr2-4*sd2,pr2+4*sd2),pr2,pr2,sd2,0)
 parm[3,1:6] <- c(min(pr3-4*sd3,pr3+4*sd3),max(pr3-4*sd3,pr3+4*sd3),pr3,pr3,sd3,0)
 parm[4,1:6] <- c(min(pr4-4*sd4,pr4+4*sd4),max(pr4-4*sd4,pr4+4*sd1),pr4,pr4,sd4,0)
 parm[5,1:6] <- c(min(pr5-4*sd5,pr5+4*sd5),max(pr5-4*sd5,pr5+4*sd5),pr5,pr5,sd5,0)
 parm[6,1:6] <- c(min(pr6-4*sd6,pr6+4*sd6),max(pr6-4*sd6,pr6+4*sd6),pr6,pr6,sd6,0)
 parm[1:4,7] < abs(parm[1:4,7])
 row.names(parm) <- gsub("14","7", row.names(parm))
 row.names(parm) <- gsub("OT_4","LL_1b_P2000", row.names(parm))
 
 new_age_selex_parms <- rbind(base_ctl$age_selex_parms[idx7,],#1:6
                              parm,  #7
                              base_ctl$age_selex_parms[idxH7,])
 
 ctl_1$age_selex_parms <- new_age_selex_parms
 
 idxRep <- grep(paste0("LL_1b_P2000",collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:3] <- c(-6,0,-3,-3,0.6)
 ctl_1$age_selex_parms[idxRep[4],1:3] <- c(-20,0,-10,-10,2)
 
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 
 
 
 #....................................................
 #### DIVINDING IN 2 THE FLEETS 7,10 AND 13 THE YEAR 213-2000 AND FLEET 7 DN  ####
 #.......................................................................
 
 
 config_name = 'sensitivities_16/16_NsampLL5_LL_log_LL1b_LL4_DN'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/sensitivities_16/16_NsampLL5_LL_log_LL1b_DN'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 #
 
 idx <- 1:length(ctl_1$age_selex_parms$LO)
 for(i in idx){
   if(base_ctl$age_selex_parms[i,3]>0){
     ctl_1$age_selex_parms[i,c(1:5)] <- 
       c(floor(base_ctl$age_selex_parms[i,3]*0.25),ceiling(base_ctl$age_selex_parms[i,3]*(1+1)),
         base_ctl$age_selex_parms[i,3],base_ctl$age_selex_parms[i,3],abs(base_ctl$age_selex_parms[i,3]*0.2))
   }else{
     ctl_1$age_selex_parms[i,c(1:5)] <- 
       c(floor(base_ctl$age_selex_parms[i,3]*(1+1)),ceiling(base_ctl$age_selex_parms[i,3]*0.25),
         base_ctl$age_selex_parms[i,3],base_ctl$age_selex_parms[i,3],abs(base_ctl$age_selex_parms[i,3]*0.2))
   }}
 
 #add prior??
 
 idx <- grep(paste0(dat_1$fleetinfo$fleetname[13],collapse="|"), 
             row.names(ctl_1$age_selex_types), value=FALSE)
 ctl_1$age_selex_types[idx,1] <- 20 #DN
 
 
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[14],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep,] 
 
 idx13 <- grep(paste0(c(dat_1$fleetinfo$fleetname[1:12]),collapse="|"), 
              row.names(ctl_1$age_selex_parms), value=FALSE)
 idxH13 <- grep(paste0(c(dat_1$fleetinfo$fleetname[14:24]),collapse="|"), 
               row.names(ctl_1$age_selex_parms), value=FALSE)
 
 parm <- ctl_1$age_selex_parms[idxRep,]
 
 pr1 <- 7
 sd1 <- pr1*0.2
 pr2 <-  -0.38
 sd2 <- pr2*0.2
 pr3 <- -10.7
 sd3 <- abs(pr3*0.2)
 pr4 <- -0.47
 sd4 <- pr4*0.2 
 pr5 <- -6
 sd5 <- pr5*0.2
 pr6 <- -7
 sd6 <- abs(pr6*0.2)
 
 parm[1,1:6] <- c(min(pr1-4*sd1,pr1+4*sd1),max(pr1-4*sd1,pr1+4*sd1),pr1,pr1,sd1,0)
 parm[2,1:6] <- c(min(pr2-4*sd2,pr2+4*sd2),max(pr2-4*sd2,pr2+4*sd2),pr2,pr2,sd2,0)
 parm[3,1:6] <- c(min(pr3-4*sd3,pr3+4*sd3),max(pr3-4*sd3,pr3+4*sd3),pr3,pr3,sd3,0)
 parm[4,1:6] <- c(min(pr4-4*sd4,pr4+4*sd4),max(pr4-4*sd4,pr4+4*sd1),pr4,pr4,sd4,0)
 parm[5,1:6] <- c(min(pr5-4*sd5,pr5+4*sd5),max(pr5-4*sd5,pr5+4*sd5),pr5,pr5,sd5,0)
 parm[6,1:6] <- c(min(pr6-4*sd6,pr6+4*sd6),max(pr6-4*sd6,pr6+4*sd6),pr6,pr6,sd6,0)
 
 row.names(parm) <- gsub("14","13", row.names(parm))
 row.names(parm) <- gsub("OT_4","LL_4", row.names(parm))
 parm[2,7] <- 5
 
 new_age_selex_parms <- rbind(ctl_1$age_selex_parms[idx13,],#1:12
                              parm,  #13
                              ctl_1$age_selex_parms[idxH13,])
 #write.csv(new_age_selex_parms, file=file.path("data","ss3_inputs","4A_io","age_selex_parms.csv"),row.names=TRUE)
 
 ctl_1$age_selex_parms <- new_age_selex_parms
 
#based on run
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[7],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 pr1 <- -2.5
 sd1 <- pr1*0.2
 
 ctl_1$age_selex_parms[idxRep[3],1:5] <- c(min(pr1-4*sd1,pr1+4*sd1),max(pr1-4*sd1,pr1+4*sd1),pr1,pr1,sd1)
 pr1 <- 1.2
 sd1 <- pr1*0.2
 ctl_1$age_selex_parms[idxRep[4],1:5] <- c(min(pr1-4*sd1,pr1+4*sd1),max(pr1-4*sd1,pr1+4*sd1),pr1,pr1,sd1)
 
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[1],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:2] <- c(-10,9)
 
 
 
 idxRep <- grep(paste0("LL_1b_P2000",collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:3] <- c(-6,0,-3,-3,0.6)
 ctl_1$age_selex_parms[idxRep[4],1:3] <- c(-20,0,-10,-10,2)

 idxRep <- grep(paste0("LL_1b_P2000",collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:3] <- c(-6,0,-3,-3,0.6)
 ctl_1$age_selex_parms[idxRep[4],1:3] <- c(-20,0,-10,-10,2)
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 #### DIVINDING IN 2 THE FLEETS 7,10 AND 13 THE YEAR 213-2000 AND FLEET 7 DN  ####
 #.......................................................................
 
 
 config_name = 'sensitivities_16/16B_NsampLL5_LL_log_LL1b_LL4_DN'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/sensitivities_16/16_NsampLL5_LL_log_LL1b_LL4_DN'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 #
 
 idx <- 1:length(ctl_1$age_selex_parms$LO)
 for(i in idx){
   if(base_ctl$age_selex_parms[i,3]>0){
     ctl_1$age_selex_parms[i,c(1:5)] <- 
       c(floor(base_ctl$age_selex_parms[i,3]*0.25),ceiling(base_ctl$age_selex_parms[i,3]*(1+1)),
         base_ctl$age_selex_parms[i,3],base_ctl$age_selex_parms[i,3],abs(base_ctl$age_selex_parms[i,3]*0.2))
   }else{
     ctl_1$age_selex_parms[i,c(1:5)] <- 
       c(floor(base_ctl$age_selex_parms[i,3]*(1+1)),ceiling(base_ctl$age_selex_parms[i,3]*0.25),
         base_ctl$age_selex_parms[i,3],base_ctl$age_selex_parms[i,3],abs(base_ctl$age_selex_parms[i,3]*0.2))
   }}

 
 #based on run

 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[1],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:2] <- c(-10,9)
 
 
 
 idxRep <- grep(paste0("LL_1b_P2000",collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:3] <- c(-8,0,-3,-3,0.6)
 ctl_1$age_selex_parms[idxRep[4],1:3] <- c(-20,0,-10,-10,2)
 
 idxRep <- grep(paste0("LL_4_P2000",collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:5] <- c(-40,0,-21,-21,4.2)
 ctl_1$age_selex_parms[idxRep[4],1:5] <- c(-6,0,-1,-1,0.2)
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 
 ##### FREE PARAMETER 2 FROM DN SHAPE LL4  ####
 #................................................................
 config_name = 'sensitivities_16/16C_NsampLL5_LL_log_LL1b_LL4_DN_freeParLL_HESS'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/sensitivities_16/16B_NsampLL5_LL_log_LL1b_LL4_DN'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 #
 
 
 #based on run
 
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[7],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[2],7] <- 5
 
 
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 
 #### FREE PARAMETER 2 FROM DN SHAPE LL4  ####
 #................................................................
 config_name = 'sensitivities_16/16E_NsampLL5_LL_log_LL1b_LL4_DN-prior'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/sensitivities_16/16E_NsampLL5_LL_log_LL1b_LL4_DN'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 #
 
 idx <- 1:length(ctl_1$age_selex_parms$LO)
 for(i in idx){
   if(base_ctl$age_selex_parms[i,3]>0){
     ctl_1$age_selex_parms[i,c(1:6)] <- 
       c(floor(base_ctl$age_selex_parms[i,3]*0.25),ceiling(base_ctl$age_selex_parms[i,3]*(1+1)),
         base_ctl$age_selex_parms[i,3],base_ctl$age_selex_parms[i,3],abs(base_ctl$age_selex_parms[i,3]*0.2),6)
   }else{
     ctl_1$age_selex_parms[i,c(1:6)] <- 
       c(floor(base_ctl$age_selex_parms[i,3]*(1+1)),ceiling(base_ctl$age_selex_parms[i,3]*0.25),
         base_ctl$age_selex_parms[i,3],base_ctl$age_selex_parms[i,3],abs(base_ctl$age_selex_parms[i,3]*0.2),6)
   }}
 
 
 #based on run
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[7],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:2] <- c(-10,9)
 
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[1],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:2] <- c(-10,9)
 
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[13],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 ctl_1$age_selex_parms[idxRep[3],1:3] <- c(-40,-5,-23,-23)
 ctl_1$age_selex_parms[idxRep[4],1:3] <- c(-20,0,-2,-2)
 

 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 
 #### FREE PARAMETER 2 FROM DN SHAPE LL4  ####
 #................................................................
 config_name = 'sensitivities_16/16D_NsampLL5_LL_log_LL1b_LL4_DN'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/sensitivities_16/16C_NsampLL5_LL_log_LL1b_LL4_DN_freeParLL_HESS'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 #
 
 idx <- 1:length(ctl_1$age_selex_parms$LO)
 for(i in idx){
   if(base_ctl$age_selex_parms[i,3]>0){
     ctl_1$age_selex_parms[i,c(1:5)] <- 
       c(floor(base_ctl$age_selex_parms[i,3]*0.25),ceiling(base_ctl$age_selex_parms[i,3]*(1+1)),
         base_ctl$age_selex_parms[i,3],base_ctl$age_selex_parms[i,3],abs(base_ctl$age_selex_parms[i,3]*0.2))
   }else{
     ctl_1$age_selex_parms[i,c(1:5)] <- 
       c(floor(base_ctl$age_selex_parms[i,3]*(1+1)),ceiling(base_ctl$age_selex_parms[i,3]*0.25),
         base_ctl$age_selex_parms[i,3],base_ctl$age_selex_parms[i,3],abs(base_ctl$age_selex_parms[i,3]*0.2))
   }}
 
 
 #based on run
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[7],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:2] <- c(-10,9)
 
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[1],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:2] <- c(-10,9)
 
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[13],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 ctl_1$age_selex_parms[idxRep[3],1:3] <- c(-40,-5,-23,-23)
 ctl_1$age_selex_parms[idxRep[4],1:3] <- c(-20,0,-2,-2)
 
 ctl_1$last_early_yr_nobias_adj  <- -5.9
 ctl_1$first_yr_fullbias_adj <- 163.3
 ctl_1$last_yr_fullbias_adj <- 303.3
 ctl_1$first_recent_yr_nobias_adj <- 309.1
 ctl_1$max_bias_adj <- 0.7052
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 
 #....................................................
 #### DIVINDING IN 2 THE FLEETS 7,10 AND 13 THE YEAR 213-2000 AND FLEET 7 DN
 #.......................................................................
 
 
 config_name = 'sensitivities_16/16_NsampLL5_LL_log_LL1b_LL4_LL2_DN'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/sensitivities_16/16E_NsampLL5_LL_log_LL1b_LL4_DN'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 #
 
 idx <- 1:length(ctl_1$age_selex_parms$LO)
 for(i in idx){
   if(base_ctl$age_selex_parms[i,3]>0){
     ctl_1$age_selex_parms[i,c(1:5)] <- 
       c(floor(base_ctl$age_selex_parms[i,3]*0.25),ceiling(base_ctl$age_selex_parms[i,3]*(1+1)),
         base_ctl$age_selex_parms[i,3],base_ctl$age_selex_parms[i,3],abs(base_ctl$age_selex_parms[i,3]*0.2))
   }else{
     ctl_1$age_selex_parms[i,c(1:5)] <- 
       c(floor(base_ctl$age_selex_parms[i,3]*(1+1)),ceiling(base_ctl$age_selex_parms[i,3]*0.25),
         base_ctl$age_selex_parms[i,3],base_ctl$age_selex_parms[i,3],abs(base_ctl$age_selex_parms[i,3]*0.2))
   }}
 

 
 idx <- grep(paste0(dat_1$fleetinfo$fleetname[10],collapse="|"), 
             row.names(ctl_1$age_selex_types), value=FALSE)
 ctl_1$age_selex_types[idx,1] <- 20 #DN
 
 
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[14],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep,] 
 
 idx10 <- grep(paste0(c(dat_1$fleetinfo$fleetname[1:9]),collapse="|"), 
               row.names(ctl_1$age_selex_parms), value=FALSE)
 idxH10 <- grep(paste0(c(dat_1$fleetinfo$fleetname[11:24]),collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 parm <- ctl_1$age_selex_parms[idxRep,]
 
 pr1 <- 7
 sd1 <- pr1*0.2
 pr2 <-  -0.38
 sd2 <- pr2*0.2
 pr3 <- -10.7
 sd3 <- abs(pr3*0.2)
 pr4 <- -0.47
 sd4 <- pr4*0.2 
 pr5 <- -6
 sd5 <- pr5*0.2
 pr6 <- -7
 sd6 <- abs(pr6*0.2)
 
 parm[1,1:6] <- c(min(pr1-4*sd1,pr1+4*sd1),max(pr1-4*sd1,pr1+4*sd1),pr1,pr1,sd1,0)
 parm[2,1:6] <- c(min(pr2-4*sd2,pr2+4*sd2),max(pr2-4*sd2,pr2+4*sd2),pr2,pr2,sd2,0)
 parm[3,1:6] <- c(min(pr3-4*sd3,pr3+4*sd3),max(pr3-4*sd3,pr3+4*sd3),pr3,pr3,sd3,0)
 parm[4,1:6] <- c(min(pr4-4*sd4,pr4+4*sd4),max(pr4-4*sd4,pr4+4*sd1),pr4,pr4,sd4,0)
 parm[5,1:6] <- c(min(pr5-4*sd5,pr5+4*sd5),max(pr5-4*sd5,pr5+4*sd5),pr5,pr5,sd5,0)
 parm[6,1:6] <- c(min(pr6-4*sd6,pr6+4*sd6),max(pr6-4*sd6,pr6+4*sd6),pr6,pr6,sd6,0)
 
 row.names(parm) <- gsub("14","10", row.names(parm))
 row.names(parm) <- gsub("OT_4","LL_2", row.names(parm))
 parm[2,7] <- 5
 
 new_age_selex_parms <- rbind(ctl_1$age_selex_parms[idx10,],#1:12
                              parm,  #13
                              ctl_1$age_selex_parms[idxH10,])
 #write.csv(new_age_selex_parms, file=file.path("data","ss3_inputs","4A_io","age_selex_parms.csv"),row.names=TRUE)
 
 ctl_1$age_selex_parms <- new_age_selex_parms
 
 #based on run
 #idxRep <- grep(paste0(c("FISHERY7"),collapse="|"), 
 #               row.names(ctl_1$age_selex_parms), value=FALSE)
 # 
 # pr1 <- -2.5
 # sd1 <- pr1*0.2
 # 
 # ctl_1$age_selex_parms[idxRep[3],1:5] <- c(min(pr1-4*sd1,pr1+4*sd1),max(pr1-4*sd1,pr1+4*sd1),pr1,pr1,sd1)
 # pr1 <- 0.2
 # sd1 <- pr1*0.2
 # ctl_1$age_selex_parms[idxRep[4],1:5] <- c(min(pr1-4*sd1,pr1+4*sd1),max(pr1-4*sd1,pr1+4*sd1),pr1,pr1,sd1)
 
 
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[7],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:2] <- c(-10,9)
 
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[1],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:2] <- c(-10,9)
 
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[13],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 ctl_1$age_selex_parms[idxRep[3],1:3] <- c(-40,-5,-23,-23)
 ctl_1$age_selex_parms[idxRep[4],1:3] <- c(-20,0,-2,-2)
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 #....................................................
 #### DIVINDING IN 2 THE FLEETS 7,10 AND 13 THE YEAR 213-2000 AND FLEET 7 DN
 #.......................................................................
 
 
 config_name = 'sensitivities_16/16B_NsampLL5_LL_log_LL1b_LL4_LL2_DN'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/sensitivities_16/16_NsampLL5_LL_log_LL1b_LL4_LL2_DN'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 #
 
 idx <- 1:length(ctl_1$age_selex_parms$LO)
 for(i in idx){
   if(base_ctl$age_selex_parms[i,3]>0){
     ctl_1$age_selex_parms[i,c(1:5)] <- 
       c(floor(base_ctl$age_selex_parms[i,3]*0.25),ceiling(base_ctl$age_selex_parms[i,3]*(1+1)),
         base_ctl$age_selex_parms[i,3],base_ctl$age_selex_parms[i,3],abs(base_ctl$age_selex_parms[i,3]*0.2))
   }else{
     ctl_1$age_selex_parms[i,c(1:5)] <- 
       c(floor(base_ctl$age_selex_parms[i,3]*(1+1)),ceiling(base_ctl$age_selex_parms[i,3]*0.25),
         base_ctl$age_selex_parms[i,3],base_ctl$age_selex_parms[i,3],abs(base_ctl$age_selex_parms[i,3]*0.2))
   }}
 

 
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[7],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:2] <- c(-10,9)
 
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[1],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 
 ctl_1$age_selex_parms[idxRep[3],1:2] <- c(-10,9)
 
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[13],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 ctl_1$age_selex_parms[idxRep[3],1:4] <- c(-40,-5,-23,-23)
 ctl_1$age_selex_parms[idxRep[4],1:4] <- c(-20,0,-2,-2)
 
 idxRep <- grep(paste0(dat_1$fleetinfo$fleetname[10],collapse="|"), 
                row.names(ctl_1$age_selex_parms), value=FALSE)
 ctl_1$age_selex_parms[idxRep[2],1:4] <- c(-1,5,0,0)
 ctl_1$age_selex_parms[idxRep[3],1:4] <- c(-40,0,-20,-20)
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 #### double normal p2000 LL1B ADN LL4 BUT LL2 ONLY ONE FLEET ####
 #.................................................
 
 
 config_name = 'sensitivities_16/16F_NsampLL5_LL1b_LL4_DN'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/sensitivities_16/16E_NsampLL5_LL_log_LL1b_LL4_DN'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 #ctl_1$age_selex_parms["AgeSel_P_2_FISHERY11(11)",]$PHASE <-3
 dat_1$Nfleets <- 27
 # newfleets <- base_dat$fleetinfo[base_dat$fleetinfo$fleetname %in% base_dat$fleetinfo$fleetname[c(7,10,13)],]
 # newfleets$fleetname <- c("22_LL_1b_A2000","23_LL_2_A2000","24_LL_4_A2000")
 # row.names(newfleets) <- c("22","23","24")
 
 #fleetinfo
 
 
 dat_1$fleetinfo <- rbind(base_dat$fleetinfo[1:22,],
                          base_dat$fleetinfo[24:28,])
 
 row.names(dat_1$fleetinfo)[23:27] <- as.character(23:27)
 dat_1$fleetinfo$fleetname[10] <-  c("10_LL_2")
 dat_1$fleetinfo$fleetname[23] <-  c("23_LL_4_A2000")
 dat_1$fleetinfo$fleetname[c(24:27)] <- paste0(24:27,"_CPUE_",c("LL_1b","LL_2","LL_3","LL_4"))
 
 #fleetinfo1
 
 dat_1$fleetinfo1 <- cbind(base_dat$fleetinfo1[,1:22],
                           base_dat$fleetinfo1[,24:28])
 names(dat_1$fleetinfo1) <- dat_1$fleetinfo$fleetname
 
 #fleetinfo2
 
 dat_1$fleetinfo2 <- cbind(base_dat$fleetinfo2[,1:22],
                           base_dat$fleetinfo2[,24:28])
 names(dat_1$fleetinfo2) <- dat_1$fleetinfo$fleetname
 
 #catch
 
 dat_1$catch$fleet[dat_1$catch$fleet==23] <- 10
 dat_1$catch$fleet[dat_1$catch$fleet==24] <- 23
 #checking
 sum(base_dat$catch$catch)-sum(dat_1$catch$catch)
 sum(base_dat$catch$year)-sum(dat_1$catch$year)
 
 #write.csv(dat_1$catch,file=file.path("data","ss3_inputs","4A_io","catch_28fleets.csv"),row.names=FALSE)
 
 
 #cpue
 
 
 dat_1$CPUEinfo<- rbind(base_dat$CPUEinfo[1:22,],
                        base_dat$CPUEinfo[24:28,])
 row.names(dat_1$CPUEinfo) <- dat_1$fleetinfo$fleetname
 dat_1$CPUEinfo$fleet <- 1:27
 
 dat_1$CPUE$index[dat_1$CPUE$index==25 ] <- 24
 dat_1$CPUE$index[dat_1$CPUE$index==26 ] <- 25
 dat_1$CPUE$index[dat_1$CPUE$index==27 ] <- 26
 dat_1$CPUE$index[dat_1$CPUE$index==28 ] <- 27
 
 
 
 
 #write.csv(dat_1$CPUE,file=file.path("data","ss3_inputs","4A_io","cpue_28fleets.csv"),row.names=FALSE)
 
 #len_info
 
 dat_1$len_info<- rbind(base_dat$len_info[1:22,], base_dat$len_info[24:28,]) 
 row.names(dat_1$len_info) <- dat_1$fleetinfo$fleetname
 #LENCOMP
 dat_1$lencomp$Nsamp[dat_1$lencomp$fleet==3] <- 5
 dat_1$lencomp$Nsamp[dat_1$lencomp$fleet==7] <- 5
 dat_1$lencomp$Nsamp[dat_1$lencomp$fleet==10] <- 5
 dat_1$lencomp$Nsamp[dat_1$lencomp$fleet==11] <- 5
 dat_1$lencomp$Nsamp[dat_1$lencomp$fleet==13] <- 5
 
 dat_1$lencomp$fleet[dat_1$lencomp$fleet==23 ] <- 10
 dat_1$lencomp$fleet[dat_1$lencomp$fleet==24 ] <- 23
 summary(dat_1$lencomp)
 
 #write.csv(dat_1$lencomp,file=file.path("data","ss3_inputs","4A_io","lencomp_28fleets.csv"),row.names=FALSE)
 
 #TG
 
 dat_1$tag_recaps$fleet[dat_1$tag_recaps$fleet==23 ] <- 10
 dat_1$tag_recaps$fleet[dat_1$tag_recaps$fleet==24 ] <- 23
 
 
 sum(dat_1$tag_recaps$Nrecap) - sum(dat_1$tag_recaps$Nrecap)
 
 #write.csv(dat_1$tag_recaps,file=file.path("data","ss3_inputs","4A_io","tag_recaps_28fleets.csv"),row.names=FALSE)
 
 #CONTROL
 
 #Q_options
 ctl_1$Q_options$fleet <- c(24:27)
 ctl_1$Q_options$link_info[2:4] <- 24
 ctl_1$Q_options
 row.names(ctl_1$Q_options) <- dat_1$fleetinfo$fleetname[24:27]
 #Q_parms
 row.names(ctl_1$Q_parms) <- paste0("LNQ_base_",dat_1$fleetinfo$fleetname[24:27], "(",24:27,")")
 ctl_1$Q_parms
 
 
 #size_selex_types
 
 ctl_1$size_selex_types <- rbind(base_ctl$size_selex_types[1:22,],
                                 base_ctl$size_selex_types[24:28,])
 row.names(ctl_1$size_selex_types) <- dat_1$fleetinfo$fleetname
 
 #age_selex_types
 
 ctl_1$age_selex_types <- rbind(base_ctl$age_selex_types[1:22,],
                                base_ctl$age_selex_types[24:28,])
 row.names(ctl_1$age_selex_types) <- dat_1$fleetinfo$fleetname
 
 # 
 # idx <- grep(paste0(c("FISHERY7","FISHERY10","FISHERY13"),collapse="|"), 
 #             row.names(ctl_1$age_selex_types), value=FALSE)
 # ctl_1$age_selex_types[idx,1] <- 12 #logistic
 
 #parms
 
 
 idx22 <- grep(paste0(c(base_dat$fleetinfo$fleetname[1:22]),collapse="|"), 
               row.names(base_ctl$age_selex_parms), value=FALSE)
 
 idxH24 <- grep(paste0(c(base_dat$fleetinfo$fleetname[24:28]),collapse="|"), 
                row.names(base_ctl$age_selex_parms), value=FALSE)
 
 
 new_age_selex_parms <- rbind(base_ctl$age_selex_parms[idx22,],#1:22
                              base_ctl$age_selex_parms[idxH24,])  #24:28
 #write.csv(new_age_selex_parms, file=file.path("data","ss3_inputs","4A_io","age_selex_parms.csv"),row.names=TRUE)
 
 idx <- grep(paste0(base_dat$fleetinfo$fleetname[10],collapse="|"), 
             row.names(new_age_selex_parms), value=FALSE)
 
 
 row.names(new_age_selex_parms[idx,]) <- gsub("_P2000",  "",row.names(new_age_selex_parms[idx,]))
 
 idx <- grep(paste0(base_dat$fleetinfo$fleetname[24],collapse="|"), 
             row.names(new_age_selex_parms), value=FALSE)
 
 row.names(new_age_selex_parms[idx,]) <- gsub("24",  "23",row.names(new_age_selex_parms[idx,]))
 
 ctl_1$age_selex_parms <- new_age_selex_parms
 
 #TAG
 
 tg_rep24 <- base_ctl$TG_Report_fleet[24,]
 row.names(tg_rep24) <-gsub("24","23",row.names(tg_rep24))
 
 
 tag_recaps_new <- rbind(base_ctl$TG_Report_fleet[1:22,],
                         base_ctl$TG_Report_fleet[24,] )
 #write.csv(tag_recaps_new, file=file.path("data","ss3_inputs","4A_io","ctl_tag_recaps_28fleets.csv"),row.names=TRUE)
 ctl_1$TG_Report_fleet <- tag_recaps_new
 
 
 new_fleet_decay <- base_ctl$TG_Report_fleet_decay[c(24),]
 row.names(new_fleet_decay) <- gsub("24","23", row.names(new_fleet_decay))
 
 ctl_1$TG_Report_fleet_decay <- rbind(base_ctl$TG_Report_fleet_decay[1:22,],
                                      new_fleet_decay)
 
 
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 
 ##### VARIABLE CV CPUE  ####
 
 #.................................................
 
 
 config_name = '18_CPUEvariable'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/sensitivities_16/16E_fixPeak_hess'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss_new'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 #base_dat$lencomp <- base_dat$lencomp #%>% subset(!(fleet==21 & year<240))
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 cpue_df = read.csv(file.path(shrpoint_path, SS_data, 'scaled_cpue_Meancv_02.csv'))
 
 dat_1$CPUE <- cpue_df
 names(dat_1$CPUE)  <- names(base_dat$CPUE)
 dat_1$CPUE$index[dat_1$CPUE$index==25 ] <- 28
 dat_1$CPUE$index[dat_1$CPUE$index==24 ] <- 27
 dat_1$CPUE$index[dat_1$CPUE$index==23 ] <- 26
 dat_1$CPUE$index[dat_1$CPUE$index==22 ] <- 25
 
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 
 #.................................................
 
 ##### EFFORT CREEP
 
 #.................................................
 
 
 config_name = 'sensitivities_Effort_Creep/19_EffortCreep'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/sensitivities_16/16E_fixPeak_hess'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 

 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 cpue_dat_effcreep = apply_eff_creep(base_dat$CPUE, yr_col = 'year', fleet_col = 'index',
                                     cpue_col = 'obs', cv_col = 'se_log', rate = 0.005)
cpue_dat_effcreep$month <- 1
 dat_1$CPUE <-  cpue_dat_effcreep[,c(1,5,2:4)]
summary(dat_1$CPUE)
 
 
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 
 ##### EFFORT CREEP
 
 #.................................................
 
 
 config_name = 'sensitivities_Effort_Creep/19_EffortCreep_prior'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/sensitivities_16/16E_fixPeak_prior_hess'
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
 
 cpue_dat_effcreep = apply_eff_creep(base_dat$CPUE, yr_col = 'year', fleet_col = 'index',
                                     cpue_col = 'obs', cv_col = 'se_log', rate = 0.005)
 cpue_dat_effcreep$month <- 1
 dat_1$CPUE <-  cpue_dat_effcreep[,c(1,5,2:4)]
 summary(dat_1$CPUE)
 
 
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 #
 
 #.................................................
 
 ##### EFFORT CREEP  ####
 
 #.................................................
 
 
 config_name = '22_TwoBlockCPUE'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/16_LLsplit_LL1b_LL4_DN'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
#
 start_1$init_values_src <- 0
 
 dat_1$Nfleets <- 31
 newfleets <- base_dat$fleetinfo[base_dat$fleetinfo$fleetname %in% base_dat$fleetinfo$fleetname[c(25,26,28)],]
 newfleets$fleetname <- c("29_CPUE_LL_1b_A2000","30_CPUE_LL_2_A2000","31_CPUE_LL_4_A2000")
 row.names(newfleets) <- c("29","30","31")
 
 #fleetinfo
 
 dat_1$fleetinfo$fleetname[c(25,26,28)] <- paste0(c(25,26,28),"_CPUE_",c("LL_1b","LL_2","LL_4"),"_P2000")
 
 dat_1$fleetinfo <- rbind(base_dat$fleetinfo[1:24,],
                          dat_1$fleetinfo[25:28,],
                          newfleets)
 
 row.names(dat_1$fleetinfo)[29:31] <- as.character(29:31)
 #fleetinfo1
 
 newfleets <- dat_1$fleetinfo1[,c(25,26,28)]
 names(newfleets)<- dat_1$fleetinfo$fleetname[29:31]
 
 dat_1$fleetinfo1 <- cbind( base_dat$fleetinfo1[,1:28],newfleets)
 names(dat_1$fleetinfo1) <- dat_1$fleetinfo$fleetname
 
 
 
 #fleetinfo2
 
 newfleets <- dat_1$fleetinfo2[,c(25,26,28)]
 names(newfleets)<- dat_1$fleetinfo$fleetname[c(29:31)]
 
 dat_1$fleetinfo2 <- cbind(base_dat$fleetinfo2[,1:28],
                           newfleets)
 names(dat_1$fleetinfo2) <- dat_1$fleetinfo$fleetname
 
 
 #cpue
 CPUEinfonew <- base_dat$CPUEinfo[c(25,26,28),]
 CPUEinfonew$fleet <- 29:31
 row.names(CPUEinfonew) <- dat_1$fleetinfo$fleetname[29:31]
 
 #CPUEinfonew2 <- base_dat$CPUEinfo[22:25,]
 #CPUEinfonew2$fleet <- 25:28
 
 dat_1$CPUEinfo<- rbind(base_dat$CPUEinfo[1:28,],
                        CPUEinfonew)
 row.names(dat_1$CPUEinfo) <- dat_1$fleetinfo$fleetname
 
 dat_1$CPUE$index[dat_1$CPUE$index==25 & dat_1$CPUE$year>=213] <- 29
 dat_1$CPUE$index[dat_1$CPUE$index==26  & dat_1$CPUE$year>=213] <- 30
 dat_1$CPUE$index[dat_1$CPUE$index==28  & dat_1$CPUE$year>=213] <- 31

 #len_info
 
 newfleets <- base_dat$len_info[c(25,26,28),]
 row.names(newfleets) <- dat_1$fleetinfo$fleetname[29:31]
 row.names(base_dat$len_info)
 dat_1$len_info<- rbind(base_dat$len_info, newfleets) 
 row.names(dat_1$len_info) <- dat_1$fleetinfo$fleetname
 
 
 
 #CONTROL
 
 #Q_options
 newfleets <- base_ctl$Q_options[2:4,]
 newfleets$fleet <- 29:31
 ctl_1$Q_options <- rbind(base_ctl$Q_options[1:4,],
                          newfleets)
# ctl_1$Q_options$fleet <- c(25:28)
 #ctl_1$Q_options$link_info[2:4] <- 25
 row.names(ctl_1$Q_options) <- dat_1$fleetinfo$fleetname[25:31]
 #Q_parms
 newfleets <- ctl_1$Q_parms[2:4,]
 ctl_1$Q_parms <- rbind(base_ctl$Q_parms, newfleets)
 row.names(ctl_1$Q_parms) <- paste0("LNQ_base_",dat_1$fleetinfo$fleetname[25:31], "(",25:31,")")
 ctl_1$Q_parms
 
 #size_selex_types
 newfleets <-rbind(base_ctl$size_selex_types[c(25,26,28),])
 row.names(newfleets) <- dat_1$fleetinfo$fleetname[c(29:31)]
 
 ctl_1$size_selex_types <- rbind(base_ctl$size_selex_types,
                                 newfleets)
 row.names(ctl_1$size_selex_types) <- dat_1$fleetinfo$fleetname
 
 #age_selex_types
 
 newfleets <-rbind(base_ctl$age_selex_types[c(25,26,28),])
 row.names(newfleets) <- dat_1$fleetinfo$fleetname[c(29:31)]
 
 ctl_1$age_selex_types <- rbind(base_ctl$age_selex_types,
                                newfleets)
 row.names(ctl_1$age_selex_types) <- dat_1$fleetinfo$fleetname
 ctl_1$age_selex_types[29:31,4] <- c(22,23,24)
 
 # Write SS files:
 
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 
 #### sensitivity last 8 rec dev #####
 
 config_name = '23_Last8recdev_RemLC_B300'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 

 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/16_LLsplit_LL1b_LL4_DN'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 #
 start_1$init_values_src <- 0
 #
 dat_1$len
 dat_1$lencomp <- base_dat$lencomp[dat_1$lencomp$year<=300,]
 # Write SS files:
 file.copy(file.path(shrpoint_path, SS_base, 'ss3.par'), file.path(tmp_dir, 'ss3.par'))
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 
 
 #### Remove bias correction last years #####
 
 config_name = '23B_Last8recdev_RemBCR_B300'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/16_LLsplit_LL1b_LL4_DN'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 #
 ctl_1$last_yr_fullbias_adj <- 300
 ctl_1$first_recent_yr_nobias_adj <- 301

 # Write SS files:
 file.copy(file.path(shrpoint_path, SS_base, 'ss3.par'), file.path(tmp_dir, 'ss3.par'))
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 
 
 
 
 #### Remove bias correction last years #####
 
 config_name = '23B_BCR1'
 tmp_dir = file.path(shrpoint_path, SS_config, config_name)
 dir.create(tmp_dir)
 
 
 # Temporary files:
 # SS base files path (in Sharepoint):
 SS_base = 'models/update/16_LLsplit_LL1b_LL4_DN'
 # Temporary files:
 
 base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
 base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
 base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
 base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
 
 
 dat_1 = base_dat
 ctl_1 = base_ctl
 fore_1 = base_fore
 start_1 = base_start
 
 #

 ctl_1$max_bias_adj <- -1
 # Write SS files:
 file.copy(file.path(shrpoint_path, SS_base, 'ss3.par'), file.path(tmp_dir, 'ss3.par'))
 SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
 SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
 SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
 SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
 
 # Run model:
 r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')
 