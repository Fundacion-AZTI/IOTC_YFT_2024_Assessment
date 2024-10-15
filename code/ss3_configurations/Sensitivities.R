#
#       SENSITIVITIES
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



####    CHANGE THE PRIORS TO LL FLEETS  ####
#.......................................................................


#.......................................................................
### Adding selectivity LL based on length--------------------------------------------
#.......................................................................



config_name = '14B_LL_lengthSelex_type6'
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

# 
ctl_1$size_selex_types["FISHERY3",] <-c(1,0,0,0)
ctl_1$size_selex_types["FISHERY7",] <-c(1,0,0,0)
ctl_1$size_selex_types["FISHERY10",] <-c(1,0,0,0)
ctl_1$size_selex_types["FISHERY11",] <-c(1,0,0,0)
ctl_1$size_selex_types["FISHERY13",] <-c(1,0,0,0)

ctl_1$age_selex_types["FISHERY3",] <-c(0,0,0,0)
ctl_1$age_selex_types["FISHERY7",] <-c(0,0,0,0)
ctl_1$age_selex_types["FISHERY10",] <-c(0,0,0,0)
ctl_1$age_selex_types["FISHERY11",] <-c(0,0,0,0)
ctl_1$age_selex_types["FISHERY13",] <-c(0,0,0,0)


idx <- grep(paste0(c("FISHERY3","FISHERY7","FISHERY10","FISHERY11","FISHERY13"),collapse="|"), 
            row.names(ctl_1$age_selex_parms), value=FALSE)

ctl_1$age_selex_parms <- base_ctl$age_selex_parms[-idx,]



idx6 <- grep("FISHERY6", row.names(base_ctl$size_selex_parms), value=FALSE)
idx8<- grep("FISHERY8", row.names(base_ctl$size_selex_parms), value=FALSE)

size_selex_parmsLL <- base_ctl$size_selex_parms[idx6[1:2],]
size_selex_parmsLL[1,1:7] <- c(50,	158, 100, 100,20,6,3)
size_selex_parmsLL[2,1:7] <- c(10,31,20,20,4,	6,	  3)
size_selex_parmsLL3 <- size_selex_parmsLL
size_selex_parmsLL7 <- size_selex_parmsLL
size_selex_parmsLL10 <- size_selex_parmsLL
size_selex_parmsLL11 <- size_selex_parmsLL
size_selex_parmsLL13 <- size_selex_parmsLL
row.names(size_selex_parmsLL3) <- c("Size_inflection_FISHERY3(3)","Size_95%width_FISHERY3(3)")
row.names(size_selex_parmsLL7) <- c("Size_inflection_FISHERY7(7)","Size_95%width_FISHERY7(7)")
row.names(size_selex_parmsLL10) <- c("Size_inflection_FISHERY10(10)","Size_95%width_FISHERY10(10)")
row.names(size_selex_parmsLL11) <- c("Size_inflection_FISHERY11(11)","Size_95%width_FISHERY11(11)")
row.names(size_selex_parmsLL13) <- c("Size_inflection_FISHERY13(13)","Size_95%width_FISHERY13(13)")



ctl_1$size_selex_parms <- rbind(size_selex_parmsLL3,
                                base_ctl$size_selex_parms[idx6,],
                                size_selex_parmsLL7,
                                base_ctl$size_selex_parms[idx8,],
                                size_selex_parmsLL10,
                                size_selex_parmsLL11,
                                size_selex_parmsLL13)


# Read base SS inputs (from 2021 assessment)

# Write SS files:

SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
SS_writestarter(start_1, dir = tmp_dir, overwrite = T)

# Run model:
r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')


#### based on run with modified priors


#.......................................................................
#### run 2: modifying priors with boundaries
#.......................................................................


config_name = '14A2_LLpriors_change'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir)

# Temporary files:
# SS base files path (in Sharepoint):
SS_base = 'models/update/14A_LLpriors_change'
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

idx <- grep(paste0(c("FISHERY3","FISHERY7","FISHERY10","FISHERY11","FISHERY13","FISHERY21","FISHERY14","FISHERY4","FISHERY5"),collapse="|"), 
            row.names(ctl_1$age_selex_parms), value=FALSE)


#summary(ctl_1$age_selex_parms$PHASE[idx])
#ctl_1$age_selex_parms$PR_type[idx] <- 6


#PRIORS MEAN LAST VALUE

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


# Read base SS inputs (from 2021 assessment)

# Write SS files:

SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
SS_writestarter(start_1, dir = tmp_dir, overwrite = T)

# Run model:
r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')


#...........................................................

#### LENGTH BASED V2 ####
#...........................................................

config_name = '14_LL_lengthSelex_type6'
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



idx <- grep(paste0(c("3_LL_1a","7_LL_1b","10_LL_2","11_LL_3","13_LL_4"),collapse="|"), 
            row.names(ctl_1$age_selex_parms), value=FALSE)


#PRIORS MEAN LAST VALUE

for(i in idx){
  if(base_ctl$size_selex_parms[i,3]>0){
    ctl_1$size_selex_parms[i,c(1:5)] <- 
      c(floor(base_ctl$size_selex_parms[i,3]*0.25),ceiling(base_ctl$size_selex_parms[i,3]*(1+1)),
        base_ctl$size_selex_parms[i,3],base_ctl$size_selex_parms[i,3],abs(base_ctl$size_selex_parms[i,3]*0.2))
  }else{
    ctl_1$size_selex_parms[i,c(1:5)] <- 
      c(floor(base_ctl$size_selex_parms[i,3]*(1+1)),ceiling(base_ctl$size_selex_parms[i,3]*0.25),
        base_ctl$size_selex_parms[i,3],base_ctl$size_selex_parms[i,3],abs(base_ctl$size_selex_parms[i,3]*0.2))
  }}


idx <- grep(paste0(c("FISHERY21","FISHERY14","FISHERY4","FISHERY5"),collapse="|"), 
            row.names(ctl_1$age_selex_parms), value=FALSE)



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


# Read base SS inputs (from 2021 assessment)

# Write SS files:

SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
SS_writestarter(start_1, dir = tmp_dir, overwrite = T)

# Run model:
r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')


#...............................................................

#### HIhg M 0.8  ####

config_name = 'sensitivities_15_M/15_MoldHigh_08'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir)

# Temporary files:
# SS base files path (in Sharepoint):
SS_base = 'models/update/15_BiasCorrectionRamp_hess'
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
ctl_1$natM[21:29]<- 0.8
ctl_1$Lorenzen_refage <- NULL
ctl_1$MG_parms <- base_ctl$MG_parms[-1,]

# Write SS files:

SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
SS_writestarter(start_1, dir = tmp_dir, overwrite = T)

# Run model:
r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')


#### HIhg M 0.6  ####

config_name = 'sensitivities_15_M/15_MoldHigh_06'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir)

# Temporary files:
# SS base files path (in Sharepoint):
SS_base = 'models/update/15_BiasCorrectionRamp_hess'
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
ctl_1$natM[21:29]<- 0.8
ctl_1$Lorenzen_refage <- NULL
ctl_1$MG_parms <- base_ctl$MG_parms[-1,]

# Write SS files:

SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
SS_writestarter(start_1, dir = tmp_dir, overwrite = T)

# Run model:
r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')

#### HIhg M Mirror ####

config_name = 'sensitivities_15_M/15_MoldHigh_Mmirror'
tmp_dir = file.path(shrpoint_path, SS_config, config_name)
dir.create(tmp_dir)

# Temporary files:
# SS base files path (in Sharepoint):
SS_base = 'models/update/15_BiasCorrectionRamp_hess'
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
names(natM) <- paste0("Age_",0:40)
ctl_1$natM <- as.data.frame(natM)
ctl_1$natM[21:29]<- ctl_1$natM[20:(20-8)]
ctl_1$Lorenzen_refage <- NULL
ctl_1$MG_parms <- base_ctl$MG_parms[-1,]

# Write SS files:

SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
SS_writestarter(start_1, dir = tmp_dir, overwrite = T)

# Run model:
r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = ' -nohess')

