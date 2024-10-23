rm(list = ls())

# Sharepoint path:
source('sharepoint_path.R')

# Spatial config
spat_config = '4A_io'

# SS base files path (in working folder):
SS_base = file.path('models/base', spat_config)

# SS configuration path (in working folder):
SS_config = file.path('models/configurations', spat_config)

# SS input data path (in working folder):
SS_data = file.path('data/ss3_inputs', spat_config)

# Specify if you just want to create folders with SS3 inputs, or 
# also run SS3 models and make plots
# Consider that running all models and making plots may increase the computational time considerably
run_model = FALSE
make_plots = FALSE

# -------------------------------------------------------------------------

# Read base SS inputs
base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))


# -------------------------------------------------------------------------
# Start implementing configurations ---------------------------------------

# 1_BaseCase

config_name = '1_BaseCase'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir)

# Temporary files:
dat_1 = base_dat
ctl_1 = base_ctl
fore_1 = base_fore
start_1 = base_start

# Write SS files:
SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
SS_writestarter(start_1, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path('code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}

# -------------------------------------------------------------------------

# 2_updateCatch

config_name = '2_updateCatch'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir)

# Temporary files
dat_2 = dat_1
ctl_2 = ctl_1
fore_2 = fore_1
start_2 = start_1

# Assign fleet names
fish_names = get_fisheries(spat_config)$fleet_name
dat_2$fleetinfo$fleetname <- paste0(1:25,"_",c(fish_names, c(fish_names[c(7,10,11,13)])))

# Updated catch data frame:
catch_df = read.csv(file.path(shrpoint_path, SS_data, 'catch.csv'))
dat_2$catch = updated_catch
dat_2$endyr = 308 # last year = 2023

# Change also forecast file:
fore_2$Bmark_years<- c(308,308,301,308,301,308,296,308,13,308)
fore_2$Fcast_years <- c(308,308,308,308,13,308) 
fore_2$FirstYear_for_caps_and_allocations <- 309
fore_2$Ydecl <- 308
fore_2$Yinit <- 308

# Write SS files:
SS_writedat(dat_2, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_2, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_2, dir = tmp_dir, overwrite = T)
SS_writestarter(start_2, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path('code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}


# -------------------------------------------------------------------------

# 3_updateCPUE

config_name = '3_updateCPUE'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir)

# Temporary files
dat_3 = dat_2
ctl_3 = ctl_2
fore_3 = fore_2
start_3 = start_2

#update cpue
cpue_df = read.csv(file.path(shrpoint_path, SS_data, 'cpue-ll.csv'))
cpue_df$se_log = 0.2 # assuming cv = 0.2
dat_3$CPUE <- cpue_df

# Write SS files:
SS_writedat(dat_3, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_3, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_3, dir = tmp_dir, overwrite = T)
SS_writestarter(start_3, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path('code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}

# -------------------------------------------------------------------------

# 4_updateLength

config_name = '4_updateLength'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir)

# Temporary files
dat_4 = dat_3
ctl_4 = ctl_3
fore_4 = fore_3
start_4 = start_3

#update length
length_df = read.csv(file.path(shrpoint_path, SS_data, 'size-original.csv'))
dat_4$lencomp = length_df
dat_4$lencomp$Nsamp = 5 # assume Nsamp = 5 as in 2021

# Write SS files:
SS_writedat(dat_4, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_4, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_4, dir = tmp_dir, overwrite = T)
SS_writestarter(start_4, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path('code', 'ss3_win.exe'), extras = '-nohess')
if(make_plots) {
  tmp_out = r4ss::SS_output(tmp_dir, covar = FALSE)
  r4ss::SS_plots(tmp_out)
}