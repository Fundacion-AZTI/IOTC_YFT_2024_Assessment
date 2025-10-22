

library(r4ss)

source('sharepoint_path.R')
setwd(shrpoint_path)

flnm <-list.files( "D:/AZTI/IOTC_YFT_2024 - FinalGrid")[c(2:12,14)][c(1,5:12,2,3,4)]
flnm
iter <- sub("_.*", "", flnm)   

it <- 1
dir.create(file.path("D:/AZTI/IOTC_YFT_2024 - FinalGrid","FinalGrid_CPUE2025",paste0(flnm[it]),"_2025"))
 
SS_base = file.path("D:/AZTI/IOTC_YFT_2024 - FinalGrid","FinalGrid_CPUE2025",flnm[it])

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
