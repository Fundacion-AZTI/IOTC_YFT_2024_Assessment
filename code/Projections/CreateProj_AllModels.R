

remotes::install_github("r4ss/r4ss",force=TRUE)
library(dplyr)
library(r4ss)
library(here)

proj_dir = here::here()
setwd(proj_dir)


source("code/auxiliary_functions.R")
# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)

nms <- list.dirs(path="projections/FinalGrid",full.names=FALSE)[-c(1,2,3,4,5)]
nms
grid <- nms

dir.origin <- file.path("projections","FinalGrid")
dir.origin.ForeSt <-  file.path("projections","Proj_Forecast")
dir.exists("projections")
if(!dir.exists("projections"))  dir.create("projections")
for (i in 1:length(grid)){
  if(!dir.exists(paste0("projections/",grid[i]))) dir.create(paste0("projections/",grid[i]))}

for (i in 1:length(grid)){

  file.copy(file.path(dir.origin,grid[i],"starter.ss"),
            file.path("projections",grid[i],"starterOld.ss"),overwrite=FALSE)
  
  file.copy(file.path(dir.origin,grid[i],"data.ss"),
            file.path("projections",grid[i],"data.ss"),overwrite=FALSE)
  
  file.copy(file.path(dir.origin,grid[i],"control.ss"),
              file.path("projections",grid[i],"control.ss"),overwrite=FALSE)
  
  file.copy(file.path(dir.origin.ForeSt,"forecast_noScale.ss"),
              file.path("projections",grid[i],"forecast.ss"),overwrite=FALSE)

  file.copy(file.path(dir.origin,grid[i],"ss3.par"),
            file.path("projections",grid[i],"ss3.par"),overwrite=FALSE)
  

  file.copy(file.path("ss3_3022.exe"),
            file.path("projections",grid[i],"ss3_3022.exe"),overwrite=FALSE)
  
  starter <- SS_readstarter(file.path("projections",grid[i],"starterOld.ss"))
  starter$init_values_src <- 1
  SS_writestarter(starter,file.path("projections",grid[i]),"starter.ss",overwrite=TRUE)
  


  forecatch<-seq(0.6,1.2,0.05)
  for(j in 1:length(forecatch)){
    dirFore.j <- file.path("projections",grid[i],paste0("Fore",forecatch[j]))
  dir.create(dirFore.j)
  starter <- SS_readstarter(file.path("projections/",grid[i],"/starterOld.ss"))
  
  file.copy(paste0("projections/forecast_",forecatch[j],".ss"),
            file.path("projections",grid[i],paste0("Fore",forecatch[j]),"forecast.ss"),overwrite=TRUE)
  
  
  file.copy(file.path("projections",grid[i],"starter.ss"),
            file.path("projections",grid[i],paste0("Fore",forecatch[j]),"starter.ss"),overwrite=FALSE)
  
  file.copy(file.path("projections",grid[i],"data.ss"),
            file.path("projections",grid[i],paste0("Fore",forecatch[j]),"data.ss"),overwrite=FALSE)
  
  file.copy(file.path("projections",grid[i],"control.ss"),
            file.path("projections",grid[i],paste0("Fore",forecatch[j]),"control.ss"),overwrite=FALSE)
  
  file.copy(file.path("projections",grid[i],"ss3.par"),
            file.path("projections",grid[i],paste0("Fore",forecatch[j]),"ss3.par"),overwrite=FALSE)
  
  file.copy(file.path("projections",grid[i],"ss3_3022.exe"),
            file.path("projections",grid[i],paste0("Fore",forecatch[j]),"ss3_3022.exe"),overwrite=FALSE)
  

  }

}



####RUN PROJECTIONS   #####

nms <- list.dirs(path="projections/FinalGrid",full.names=FALSE)[-c(1,2,3,4,5)]
nms
grid <- nms

for (i in 1:length(grid)){
  forecatch<-seq(0.6,1.2,0.05)
  for(j in 1:length(forecatch)){
    tmp_dir <- file.path("projections",grid[i],paste0("Fore",forecatch[j]))
   r4ss::run(dir = tmp_dir, exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-maxfn 10 -phase 10 -nohess -ams 1000000000 -cbs 1000000000 -gbs 1000000000 %1 %2 %3 %4')
  tmp_dir_res <- file.path("projections",grid[i])
   file.copy(file.path(tmp_dir,"Report.sso"),
             file.path(tmp_dir_res,paste0("Reportc",forecatch[j],".sso")),overwrite=FALSE)
   file.copy(file.path(tmp_dir,"CompReport.sso"),
             file.path(tmp_dir_res,paste0("ComReportc",forecatch[j],".sso")),overwrite=FALSE)
   file.copy(file.path(tmp_dir,"Forecast-report.sso"),
             file.path(tmp_dir_res,paste0("Forecast-reportc",forecatch[j],".sso")),overwrite=FALSE)
  }
   
}

