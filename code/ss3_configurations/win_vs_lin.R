#
#       CREATING SCENARIOS
#
#
#.............................................................
#-------------------------------------------------------------------------
#  Test running new version SS V330221
#  Comparing linux and windows
#  Fixing parameter 2 Fishery 11 running windows and linux
#  Running windows Fixing parameter 2 Fishery 11 and reading ss3.par from linux
#      due to the lower log_LKL in linux
#  Results: A scenario with windows and linux with the same log_LKL
#
#--------------------------------------------------------------

library(r4ss)

proj_dir = here::here()
setwd(proj_dir)

source("code/analysingModels/auxiliary_functions_4analysingModels.R")
# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)

# SS base files path (in Sharepoint):
SS_base = 'models/base/4A_io'

# SS configuration path (in Sharepoint):
SS_config = 'models/configurations/4A_io'
SS_win_lin= 'models/base_win_vs_lin'

# SS input data path (in Sharepoint):
SS_data = 'data/ss3_inputs/4A_io'

# -------------------------------------------------------------------------

# Read base SS inputs (from 2021 assessment)
base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))


# -------------------------------------------------------------------------
### Start implementing configurations ---------------------------------------



### 4A_io_win_v33022: Run 2021 assessment with new SS3 in windows ### -------------------------------------

config_name = '4A_io_win_v33022'
tmp_dir = file.path(shrpoint_path, SS_win_lin, config_name)
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


#-------------------------------------------------------------------------------
### 4A_io_lin_v33022: Run 2021 assessment with new SS3 in linux ### 
#-------------------------------------------------------------------------------

config_name = '4A_io_lin_v33022'
tmp_dir = file.path(shrpoint_path, SS_win_lin, config_name)
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

# Run model IN LINUX:
r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022'), extras = '-nohess')

#### end ####




#-------------------------------------------------------------------------------
### 4A_io_win_v33022_FixedParam2_Fl11: Run with new SS3 in windows, fixing param2_FL11### 
#-------------------------------------------------------------------------------

config_name = '4A_io_win_v33022_FixedParam2_Fl11'
tmp_dir = file.path(shrpoint_path, SS_win_lin, config_name)
dir.create(tmp_dir)

# Temporary files:
dat_0 = base_dat
ctl_0 = base_ctl
fore_0 = base_fore
start_0 = base_start

# If want to make changes, made them here:
idx <- which(row.names(ctl_0$age_selex_parms)=="AgeSel_P_2_FISHERY11(11)")
ctl_0$age_selex_parms[idx,]$INIT <- base_ctl$age_selex_parms[idx,]$LO #Fixing the parameter in teh lower boundary,
                                      # because that's the value that was converging the model, and not very different to FL10
ctl_0$age_selex_parms[idx,]$PHASE <- -base_ctl$age_selex_parms[idx,]$PHASE

# Write SS files:
SS_writedat(dat_0, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_0, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_0, dir = tmp_dir, overwrite = T)
SS_writestarter(start_0, dir = tmp_dir, overwrite = T)

# Run model IN WINDOWS:
r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')

#### end ####





#-------------------------------------------------------------------------------
### 4A_io_lin_v33022_FixedParam2_Fl11: Run with new SS3 in linux, fixing param2_FL11### 
#-------------------------------------------------------------------------------

config_name = '4A_io_lin_v33022_FixedParam2_Fl11'
tmp_dir = file.path(shrpoint_path, SS_win_lin, config_name)
dir.create(tmp_dir)

# Temporary files:
dat_0 = base_dat
ctl_0 = base_ctl
fore_0 = base_fore
start_0 = base_start

# If want to make changes, made them here:
idx <- which(row.names(ctl_0$age_selex_parms)=="AgeSel_P_2_FISHERY11(11)")
ctl_0$age_selex_parms[idx,]$INIT <- base_ctl$age_selex_parms[idx,]$LO #Fixing the parameter in teh lower boundary,
# because that's the value that was converging the model, and not very different to FL10
ctl_0$age_selex_parms[idx,]$PHASE <- -base_ctl$age_selex_parms[idx,]$PHASE
start_0$init_values_src <- 1
# Write SS files:
SS_writedat(dat_0, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_0, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_0, dir = tmp_dir, overwrite = T)
SS_writestarter(start_0, dir = tmp_dir, overwrite = T)

# Run model IN WINDOWS:
r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022'), extras = '-nohess')

#### end ####



#-------------------------------------------------------------------------------
### 4A_io_win_v33022_FixedParam2_Fl11_ss3par:Run with new SS3 in windows, fixing param2_FL11, but reading ss3 par from results in linux (lower LKL)### 
#-------------------------------------------------------------------------------

config_name = '4A_io_win_v33022_FixedParam2_Fl11_ss3par'
tmp_dir = file.path(shrpoint_path, SS_win_lin, config_name)
dir.create(tmp_dir)

# Temporary files:
dat_0 = base_dat
ctl_0 = base_ctl
fore_0 = base_fore
start_0 = base_start

# If want to make changes, made them here:
idx <- which(row.names(ctl_0$age_selex_parms)=="AgeSel_P_2_FISHERY11(11)")
ctl_0$age_selex_parms[idx,]$INIT <- base_ctl$age_selex_parms[idx,]$LO #Fixing the parameter in teh lower boundary,
# because that's the value that was converging the model, and not very different to FL10
ctl_0$age_selex_parms[idx,]$PHASE <- -base_ctl$age_selex_parms[idx,]$PHASE
#ss3par

file.copy(paste0(SS_win_lin,"/4A_io_lin_v33022_FixedParam2_Fl11/ss3.par"), 
          paste0(SS_win_lin,"/4A_io_win_v33022_FixedParam2_Fl11_ss3par/ss3.par"))
# Write SS files:
SS_writedat(dat_0, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_0, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_0, dir = tmp_dir, overwrite = T)
SS_writestarter(start_0, dir = tmp_dir, overwrite = T)

# Run model IN WINDOWS:
r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')

#### end ####