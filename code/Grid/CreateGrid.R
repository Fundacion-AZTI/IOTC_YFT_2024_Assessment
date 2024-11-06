#
# Create uncertainty Grid
#
#...........................................................

#remotes::install_github("r4ss/r4ss",force=TRUE)
library(dplyr)
library(r4ss)
library(here)

proj_dir = here::here()
setwd(proj_dir)


source("code/auxiliary_functions.R")
# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)


#### basis ####



# SS configuration path (in Sharepoint):
SS_config = 'models/RefModels'
dir_out <- "models/Grid"


#### UNCERTAINTY GRID ####
h.vec <- c(0.7,0.8,0.9)
tag.vec <- c(1,0.1)
EC.vec <- c(0,1)
model.vec <- c("NoSplit","Split","SplitCPUE")
RefModel.vec <- c("1_NoSplit_tag1_ECO_h08","2_Split_tag1_EC0_h08",
              "3_SplitCPUE_tag1_EC0_h08","4_Split_tag01_EC0_h08",
              "5_Split_tag1_EC1_h08")
SS_for <- file.path("models",'RefModels','Forecast_Starter')
Run <-0
for( h in h.vec){
  for(EC in EC.vec){
    for( tag in tag.vec){
      for(m in 1:3 ){
        Run <- Run+1
        model <- model.vec[m]
        config_name = paste0(Run,"_",model,"_tag",sub("\\.", "", tag),"_EC",EC,"_h",h)
        tmp_dir = file.path(shrpoint_path, dir_out, config_name)
        dir.create(tmp_dir)
        # Temporary files:
        # SS base files path (in Sharepoint):
        RefModel<- RefModel.vec[m]
        SS_base = file.path("models",'RefModels',RefModel)
        # Temporary files:
        
        base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
        base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
        base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_for, 'forecast.ss'))
        base_start = SS_readstarter(file = file.path(shrpoint_path, SS_for, 'starter.ss'))
        
        dat_1 <- base_dat
        ctl_1 <- base_ctl
        fore_1 <- base_fore
        start_1 <- base_start
        ctl_1$SR_parms["SR_BH_steep",3] <- h
        
        if(round(tag)==0){
          SS_base_tag = file.path("models",'RefModels',RefModel.vec[4])
          base_dat_tag = SS_readdat(file = file.path(shrpoint_path, SS_base_tag, 'data.ss'))
          base_ctl_tag = SS_readctl(file = file.path(shrpoint_path, SS_base_tag, 'control.ss'), datlist = base_dat_tag)
          ctl_1$N_lambdas <- base_ctl_tag$N_lambdas
          ctl_1$lambdas  <- base_ctl_tag$lambdas}
        if(EC==1){
          if(m==1){
            SS_base_EC = file.path("models",'RefModels',RefModel.vec[5])
            base_dat_EC = SS_readdat(file = file.path(shrpoint_path, SS_base_EC, 'data.ss'))
            EC_CPUE <- base_dat_EC$CPUE
            EC_CPUE$index[EC_CPUE$index==25] <- 22
            EC_CPUE$index[EC_CPUE$index==26] <- 23
            EC_CPUE$index[EC_CPUE$index==27] <- 24
            EC_CPUE$index[EC_CPUE$index==28] <- 25
            dat_1$CPUE <- EC_CPUE} 
          if(m==2){
            SS_base_EC = file.path("models",'RefModels',RefModel.vec[5])
            base_dat_EC = SS_readdat(file = file.path(shrpoint_path, SS_base_EC, 'data.ss'))
            EC_CPUE <- base_dat_EC$CPUE
            dat_1$CPUE <- EC_CPUE}
            
          if(m==3){
          SS_base_EC = file.path("models",'RefModels',RefModel.vec[5])
          base_dat_EC = SS_readdat(file = file.path(shrpoint_path, SS_base_EC, 'data.ss'))
          EC_CPUE <- base_dat_EC$CPUE
          
          EC_CPUE$index[EC_CPUE$index==25 & EC_CPUE$year>=213] <- 29
          EC_CPUE$index[EC_CPUE$index==26  & EC_CPUE$year>=213] <- 30
          EC_CPUE$index[EC_CPUE$index==28  & EC_CPUE$year>=213] <- 31
          dat_1$CPUE <- EC_CPUE}
        }
        
        # Write SS files:
        
        SS_writedat(dat_1, outfile = file.path(tmp_dir, 'data.ss'), overwrite = T)
        SS_writectl(ctl_1, outfile = file.path(tmp_dir, 'control.ss'), overwrite = T)
        SS_writeforecast(fore_1, dir = tmp_dir, overwrite = T)
        SS_writestarter(start_1, dir = tmp_dir, overwrite = T)
        
      }
    }
  }
}

  

