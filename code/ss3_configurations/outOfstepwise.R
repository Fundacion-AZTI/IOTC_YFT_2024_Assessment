

####    CHANGE THE PRIORS TO LL FLEETS  ####
#.......................................................................

# 
config_name = '14A_LLpriors_change'
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

ctl_1$age_selex_parms["AgeSel_P_2_FISHERY11(11)",]$PHASE <-3

idx <- grep(paste0(c("FISHERY3","FISHERY7","FISHERY10","FISHERY11","FISHERY13"),collapse="|"),
            row.names(ctl_1$age_selex_parms), value=FALSE)


summary(ctl_1$age_selex_parms$PHASE[idx])
ctl_1$age_selex_parms$PR_type[idx] <- 6


#PRIORS MEAN LAST VALUE

for(i in idx){
  if(base_ctl$age_selex_parms[i,3]>0){
    ctl_1$age_selex_parms[i,c(1:5)] <-
      c(floor(base_ctl$age_selex_parms[i,3]*1),ceiling(base_ctl$age_selex_parms[i,3]*(1+1)),
        base_ctl$age_selex_parms[i,3],base_ctl$age_selex_parms[i,3],abs(base_ctl$age_selex_parms[i,3]*0.2))
  }else{
    ctl_1$age_selex_parms[i,c(1:5)] <-
      c(floor(base_ctl$age_selex_parms[i,3]*(1+1)),ceiling(base_ctl$age_selex_parms[i,3]*1),
        base_ctl$age_selex_parms[i,3],base_ctl$age_selex_parms[i,3],abs(base_ctl$age_selex_parms[i,3]*0.2))
  }}


# Read base SS inputs (from 2021 assessment)

# Write SS files:

SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
SS_writestarter(start_1, dir = tmp_dir, overwrite = T)

# Run model:
r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')


#....................................................
#### ADDING REC DEV TO 14A2_LLpriors_change- better likelihood A: 8753.39 vs B-8963.92
#.......................................................................


config_name = '15_recDev2021_'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir)

# Temporary files:
# SS base files path (in Sharepoint):
SS_base = 'models/update/14A2_LLpriors_change'
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

ctl_1$MG_parms["RecrDist_GP_1_area_4_month_1",11] <- 300
ctl_1$MainRdevYrLast <- 300
ctl_1$last_yr_fullbias_adj <-303
ctl_1$first_recent_yr_nobias_adj <- 304



# Write SS files:

SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
SS_writestarter(start_1, dir = tmp_dir, overwrite = T)

# Run model:
r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')

#....................................................
#### ADDING REC DEV TO 14A2_LLpriors_change- better likelihood A: 8753.39 vs B-8963.92
# udpate catch and run model until 2023
#.......................................................................


config_name = '15_recDev2021_catch2023'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir)

# Temporary files:
# SS base files path (in Sharepoint):
SS_base = 'models/update/14A2_LLpriors_change'
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

catch_df = read.csv(file.path(shrpoint_path, SS_data, 'catch.csv'))
updated_catch = data.frame(year = catch_df[,'qtr'], seas = 1, fleet = catch_df[,'ModelFleet'], 
                           catch = catch_df[,'Catch'], catch_se = 0.01)
updated_catch = updated_catch %>% add_row(data.frame(year = -999, seas = 1, fleet = 1, catch = 0, catch_se = 0.01), .before = 1) # shouldnt we also add for all fleets?
dat_1$catch = updated_catch
# Last year = 2022:
dat_1$endyr = 308


ctl_1$MG_parms["RecrDist_GP_1_area_4_month_1",11] <- 300
ctl_1$MainRdevYrLast <- 300
ctl_1$last_yr_fullbias_adj <-305
ctl_1$first_recent_yr_nobias_adj <- 308



# Write SS files:

SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
SS_writestarter(start_1, dir = tmp_dir, overwrite = T)

# Run model:
r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')

#### HIhg M 0.6

config_name = 'sensitivities_15/15_catchUpdate2023_MoldHigh_06'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir)

# Temporary files:
# SS base files path (in Sharepoint):
SS_base = 'models/update/15_recDev2021_catch2023'
#SS_base = 'models/update/test'
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

sc_ss3 <- SS_output(dir=SS_base,  repfile = "Report.sso",covar=F)

ctl_1$natM_type  <- 3
ctl_1$N_natMparms <- 0
#ctl_1$natM <- sc_ss3$endgrowth$M
natM <- t(sc_ss3$endgrowth$M)
natM<- as.data.frame(natM)
names(natM) <- paste0("Age_",0:28)
ctl_1$natM <- as.data.frame(natM)
ctl_1$natM[21:29]<- 0.6
ctl_1$Lorenzen_refage <- NULL
ctl_1$MG_parms <- base_ctl$MG_parms[-1,]

# Write SS files:

SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
SS_writestarter(start_1, dir = tmp_dir, overwrite = T)

# Run model:
r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')