rm(list = ls())

# Sharepoint path and aux functions:
source('sharepoint_path.R')
source('code/auxiliary_functions.R')

# Run or just create folders?
run_model = TRUE
# Make plots?
make_plot = TRUE

# Spatial configuration:
spat_config = '1A_io'
spat_subconfig = 'agg'

# SS base files path (in Sharepoint):
SS_base = file.path('models/base', spat_config, spat_subconfig)

# SS configuration path (in Sharepoint):
SS_config = file.path('models/configurations', spat_config, spat_subconfig)

# -------------------------------------------------------------------------

# Read base SS inputs (from 2021 assessment)
base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))
base_fore$Forecast = -1 # dont do forecast for now

# -------------------------------------------------------------------------
# Start implementing configurations ---------------------------------------

# 1: catch + cpue + length + caal ------------------------------------------------

config_name = '1_base'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Temporary files:
dat_1 = base_dat
ctl_1 = base_ctl
fore_1 = base_fore
start_1 = base_start

# Turn off age and tag:
ctl_1$lambdas = rbind(#data.frame(like_comp = 5, fleet = 1:16, phase = 2, value = 0, sizefreq_method = 1), # age
                      data.frame(like_comp = 15, fleet = 1:dat_1$N_tag_groups, phase = 2, value = 0, sizefreq_method = 1), # tag
                      data.frame(like_comp = 16, fleet = 1:dat_1$N_tag_groups, phase = 2, value = 0, sizefreq_method = 1), # tag negative binom
                      ctl_1$lambdas)
ctl_1$N_lambdas = nrow(ctl_1$lambdas)
ctl_1$TG_Report_fleet[,7] = -6 # fix all tag params

# Write SS files:
SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
SS_writestarter(start_1, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
# Plot results
if(make_plot) {
  tmp_mod = SS_output(dir = tmp_dir, covar = FALSE, forecast = FALSE)
  # SSplotBiology(tmp_mod, subplots = 21)
  # SSplotIndices(tmp_mod, subplots = 5)
  # SSplotComps(tmp_mod, subplots = 1)
  SS_plots(tmp_mod)
}


# 2: Remove movement ------------------------------------------------------

config_name = '2_noMov'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Temporary files:
dat_2 = base_dat
ctl_2 = ctl_1
fore_2 = base_fore
start_2 = base_start

# Remove movement:
ctl_2$N_moveDef = 0
ctl_2$MG_parms = ctl_2$MG_parms %>% dplyr::filter(!row_number() %in% c(28:31))

# Write SS files:
SS_writedat(dat_2, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_2, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_2, dir = tmp_dir, overwrite = T)
SS_writestarter(start_2, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
# Plot results
if(make_plot) {
  tmp_mod = SS_output(dir = tmp_dir, covar = FALSE, forecast = FALSE)
  SS_plots(tmp_mod)
}

# 3: 2 + rev devs from 50 -------------------------------------------------

config_name = '3_noMov-rec50'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Temporary files:
dat_3 = base_dat
ctl_3 = ctl_2
fore_3 = base_fore
start_3 = base_start

# rev devs from 50:
ctl_3$MainRdevYrFirst = 50

# Write SS files:
SS_writedat(dat_3, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_3, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_3, dir = tmp_dir, overwrite = T)
SS_writestarter(start_3, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
# Plot results
if(make_plot) {
  tmp_mod = SS_output(dir = tmp_dir, covar = FALSE, forecast = FALSE)
  SS_plots(tmp_mod)
}

# 4: 3 + size weight^2 --------------------------------------------------

config_name = '4_noMov-rec50-sizeW'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir, showWarnings = FALSE)

# Temporary files:
dat_4 = base_dat
ctl_4 = ctl_3
fore_4 = base_fore
start_4 = base_start

# ideally, should be done in ctl in DoVar_adjust, but ^2 is not possible, so do it in dat:
dat_4$lencomp$Nsamp = dat_4$lencomp$Nsamp^2

# Write SS files:
SS_writedat(dat_4, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_4, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_4, dir = tmp_dir, overwrite = T)
SS_writestarter(start_4, dir = tmp_dir, overwrite = T)

# Run model:
if(run_model) r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
# Plot results
if(make_plot) {
  tmp_mod = SS_output(dir = tmp_dir, covar = FALSE, forecast = FALSE)
  SS_plots(tmp_mod)
}
