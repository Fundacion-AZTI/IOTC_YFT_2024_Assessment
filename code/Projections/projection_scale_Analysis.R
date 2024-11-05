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




i <- 1
mod.sum <- list()
forecatch<-seq(0.6,1.2,0.05)
for(i in 5:length(grid)){
  #  for(i in 1:2){
  mod.sum.grid <- list() 
  for (j in 1:length(forecatch)){
    
    dirFore.j <- file.path("output/figures/PROJECTIONS",grid[i])
    dirFore.j2 <- file.path("output/figures/PROJECTIONS",grid[i],paste0("Fore-scale-",forecatch[j]))
    dirFore.j3 <- paste0("output/figures/PROJECTIONS/",grid[i],"/scaled")
    dir.create(dirFore.j)
    
    dir.create(dirFore.j2)
    dir.create(dirFore.j3)

    tmp_dir= paste0("projections/",nms[i])
    ss3 <- SS_output(dir=tmp_dir,  forecast=TRUE,repfile = paste0("Report-scale-c",forecatch[j],".sso"))
    mod.sum[[j]] <- ss3
    SSplotCatch(mod.sum[[j]],forecastplot = TRUE,
                print=TRUE,plotdir=paste0("output/figures/PROJECTIONS/",grid[i],paste0("/Fore-scale-",forecatch[j])),
                subplots=2)
    rm(ss3)
  }
  mod.sum.grid[[i]] <- mod.sum
  mod.plots <- SSsummarize(mod.sum)
  
  SSplotComparisons(mod.plots, legendlabels=forecatch,print=TRUE,plotdir=paste0("output/figures/PROJECTIONS/",grid[i],"/scaled"),endyrvec=348)
  
  save(mod.sum.grid,file="Proj_Scale_Part2.RData")
}

save(mod.sum.grid,file="output/tables/Proj_noScale_Part2.RData")