require(r4ss)
require(here)
require(dplyr)

# Set working directiry using here():
proj_dir = here::here()
setwd(proj_dir)

# Sharepoint path:
source('sharepoint_path.R')

# SS base files path (in Sharepoint):
SS_base = 'models/base/4A_io'

# SS configuration path (in Sharepoint):
SS_config = 'models/configurations/4A_io'

# SS input data path (in Sharepoint):
SS_data = 'data/ss_inputs/4A_io'


# -------------------------------------------------------------------------

# Read base SS inputs (from 2021 assessment)
base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))


# -------------------------------------------------------------------------
# Start implementing configurations ---------------------------------------


# Run 2021 assessment with new SS exe -------------------------------------

config_name = '0_last_assessment'
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


# Update catch data until 2022 --------------------------------------------

config_name = '1_update_catch'
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


# -------------------------------------------------------------------------

# Continue....


