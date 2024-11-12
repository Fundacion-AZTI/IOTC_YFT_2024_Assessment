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
for(i in 1:length(grid)){
#  for(i in 1:2){
  mod.sum.grid <- list() 
  for (j in 1:length(forecatch)){
    
    dirFore.j <- file.path("output/figures/PROJECTIONS",grid[i])
    dirFore.j2 <- file.path("output/figures/PROJECTIONS",grid[i],paste0("Fore",forecatch[j]))
    dir.create(dirFore.j)
    dir.create(dirFore.j2)

    tmp_dir= paste0("projections/",nms[i])
    tmp_dir <- "fore-scale20_0.6"
    ss3 <- SS_output(dir=tmp_dir,  forecast=TRUE,repfile = paste0("Reportc",forecatch[j],".sso"))
    ss3 <- SS_output(dir=tmp_dir,  forecast=TRUE)
   SS_plots(ss3,uncertainty=T,png=T,forecastplot=T, fitrange = TRUE, 
            parrows=5, parcols=4, showdev= TRUE)
    mod.sum[[j]] <- ss3
    SSplotCatch(mod.sum[[j]],forecastplot = TRUE,
                print=FALSE,plotdir=paste0("output/figures/PROJECTIONS/",grid[i],paste0("/Fore",forecatch[j])),
                subplots=2)
    rm(ss3)
  }
  mod.sum.grid[[i]] <- mod.sum
  mod.plots <- SSsummarize(mod.sum)
  SSplotComparisons(mod.plots, legendlabels=forecatch,print=TRUE,
                    plotdir=paste0("output/figures/PROJECTIONS/",grid[i]),endyrvec=348)
  dev.off()
  dev.off()
  
  save(mod.sum.grid,file="Proj_noScale_Part1.RData")
}

save(mod.sum.grid,file="output/tables/Proj_noScale_Part1.RData")

saveRDS(mod.sum,'Grid_noScale_Part1.Rdata')
#### PLOTS ######

mod.plots <- SSsummarize(mod.sum)
SSplotComparisons(mod.plots, legendlabels=forecatch,print=TRUE,plotdir="output/figures/PROJECTIONS/",endyrvec=348)
SSplotCatch(mod.sum[[1]],)

#rad

ss3.1 <- SS_output(dir=tmp_dir_res,  repfile = "Reportc0.6.sso",covar=F, ncols=500,
                   compfile="CompReportc1.0",forefile="Forecast-reportc1.0.sso")
ss3.06 <- SS_output(dir=tmp_dir_res,  repfile = "Reportc0.6.sso",covar=F, ncols=500,
                    compfile="CompReportc0.6",forefile="Forecast-reportc0.6.sso")
ss3.12 <- SS_output(dir=ss3.out.wd,  repfile = "Reportc1.2.sso",covar=F, ncols=500,
                    compfile="CompReportc1.2",forefile="Forecast-reportc1.2.sso")

SS_plots(ss3.06,uncertainty=T,png=T,forecastplot=TRUE, fitrange = TRUE, parrows=5, parcols=4, showdev= TRUE) ##png allows you to save them as png files in a dedicated folder or use pdf (,pdf=T)
























#wd <-"C:/use/agur/proyectos/Atun/IOTC/YFT-IOTC/2021/ProjectionsAndAssesment/2021/gridSpatioAgur/io_h70_q1_Gbase_Mbase_tlambda1"
#setwd(wd)

sc.nm <- grid[1]

ss3.out.wd <- file.path("projections",sc.nm[1])
print(ss3.out.wd)

ss3.06 <- SS_output(dir=ss3.out.wd,  repfile = "Reportc0.6.sso",covar=F, ncols=500,
                    compfile="CompReportc0.6",forefile="Forecast-reportc0.6.sso")

ss3.1 <- SS_output(dir=ss3.out.wd,  repfile = "Reportc1.sso",covar=F, ncols=500,
                 compfile="CompReportc1",forefile="Forecast-reportc1.sso")
ss3.12 <- SS_output(dir=ss3.out.wd,  repfile = "Reportc1.2.sso",covar=F, ncols=500,
                   compfile="CompReportc1.2",forefile="Forecast-reportc1.2.sso")

SS_plots(ss3.1,uncertainty=T,png=T,forecastplot=TRUE, fitrange = TRUE, parrows=5, parcols=4, showdev= TRUE) ##png allows you to save them as png files in a dedicated folder or use pdf (,pdf=T)

#
SSB <- ss3.1$derived_quants$Value[3:326]
yr <- 13:336
plot(yr,SSB,type="l")

Bratio <- ss3.1$derived_quants$Value[1302:1624]
Bratio[Bratio>10] <- 10
ss3.1$derived_quants$Label[1302:1624]
yr <- 14:336
plot(yr,Bratio,type="l",xlim=c(296,336),ylim=c(0,4))
abline(h=1)


Forecatch <- ss3.1$derived_quants$Value[1641:(1642+length(297:334))]
#Bratio[Bratio>10] <- 10
ss3.1$derived_quants$Label[1641:(1642+length(297:334))]
yr <- 297:336
plot(yr,Forecatch,type="l",xlim=c(296,336))
abline(h=1)


F <- ss3.1$derived_quants$Value[977:(977+length(13:336)-1)]
#Bratio[Bratio>10] <- 10
ss3.1$derived_quants$Label[977:(977+length(13:336)-1)]
yr <- 13:336
plot(yr,F,type="l",xlim=c(296,336))
abline(h=1)



Forecatch <- ss3.06$derived_quants$Value[1641:(1642+length(297:334))]
#Bratio[Bratio>10] <- 10
ss3.06$derived_quants$Label[1641:(1642+length(297:334))]
yr <- 297:336
plot(yr,Forecatch,type="l",xlim=c(296,336))
abline(h=1)


#..................................

SSB <- ss3.12$derived_quants$Value[3:326]
yr <- 13:336
plot(yr,SSB,type="l")

Bratio <- ss3.12$derived_quants$Value[1302:1624]
Bratio[Bratio>10] <- 10
ss3.1$derived_quants$Label[1302:1624]
yr <- 14:336
plot(yr,Bratio,type="l",xlim=c(296,336),ylim=c(0,4))
abline(h=1)


Forecatch <- ss3.12$derived_quants$Value[1641:(1642+length(297:334))]
#Bratio[Bratio>10] <- 10
ss3.1$derived_quants$Label[1641:(1642+length(297:334))]
yr <- 297:336
plot(yr,Forecatch,type="l",xlim=c(296,336))
abline(h=1)


F <- ss3.12$derived_quants$Value[977:(977+length(13:336)-1)]
#Bratio[Bratio>10] <- 10
ss3.1$derived_quants$Label[977:(977+length(13:336)-1)]
yr <- 13:336
plot(yr,F,type="l",xlim=c(296,336))
abline(h=1)


#.........................................



wd <-"C:/use/agur/proyectos/Atun/IOTC/YFT-IOTC/2021/ProjectionsAndAssesment/2021/gridSpatioAgur/io_h70_q1_Gbase_Mbase_tlambda1"
setwd(wd)

sc.nm <- c("projection")
i<-1
ss3.out.wd <- sc.nm[i]
print(ss3.out.wd)

ss3.1 <- SS_output(dir=ss3.out.wd,  repfile = "Reportc1.0.sso",covar=F, ncols=500,
                   compfile="CompReportc1.0",forefile="Forecast-reportc1.0.sso")
ss3.06 <- SS_output(dir=ss3.out.wd,  repfile = "Reportc0.6.sso",covar=F, ncols=500,
                    compfile="CompReportc0.6",forefile="Forecast-reportc0.6.sso")
ss3.12 <- SS_output(dir=ss3.out.wd,  repfile = "Reportc1.2.sso",covar=F, ncols=500,
                    compfile="CompReportc1.2",forefile="Forecast-reportc1.2.sso")
ss3.09 <- SS_output(dir=ss3.out.wd,  repfile = "Reportc0.9.sso",covar=F, ncols=500,
                    compfile="CompReportc0.9",forefile="Forecast-reportc0.9.sso")

SS_plots(ss3.1,uncertainty=T,png=T,forecastplot=TRUE, fitrange = TRUE, parrows=5, parcols=4, showdev= TRUE) ##png allows you to save them as png files in a dedicated folder or use pdf (,pdf=T)

#
SSB <- ss3.1$derived_quants$Value[3:326]
yr <- 13:336
plot(yr,SSB,type="l")
#
SSB.06 <- ss3.06$derived_quants$Value[3:326]
#
SSB.12 <- ss3.12$derived_quants$Value[3:326]
lines(yr,SSB.06,col=2)
lines(yr,SSB.12,col=3)

Bratio <- ss3.1$derived_quants$Value[1302:1624]
Bratio[Bratio>10] <- 10
ss3.1$derived_quants$Label[1302:1624]
yr <- 14:336
plot(yr,Bratio,type="l",xlim=c(296,336),ylim=c(0,4))
abline(h=1)

Bratio <- ss3.09$derived_quants$Value[1302:1624]
Bratio[Bratio>10] <- 10
ss3.1$derived_quants$Label[1302:1624]
yr <- 14:336
plot(yr,Bratio,type="l",xlim=c(296,336),ylim=c(0,4))
abline(h=1)

Forecatch <- ss3.09$derived_quants$Value[1641:(1642+length(297:334))]
#Bratio[Bratio>10] <- 10
ss3.09$derived_quants$Label[1641:(1642+length(297:334))]
Forecatch
yr <- 297:336
plot(yr,Forecatch,type="l",xlim=c(296,336))
abline(h=1)


Forecatch <- ss3.1$derived_quants$Value[1641:(1642+length(297:334))]
#Bratio[Bratio>10] <- 10
ss3.1$derived_quants$Label[1641:(1642+length(297:334))]
yr <- 297:336
plot(yr,Forecatch,type="l",xlim=c(296,336))
abline(h=1)

F <- ss3.09$derived_quants$Value[977:(977+length(13:336)-1)]
#Bratio[Bratio>10] <- 10
ss3.09$derived_quants$Label[977:(977+length(13:336)-1)]
yr <- 13:336
plot(yr,F,type="l",xlim=c(296,336))
abline(h=1)

F <- ss3.1$derived_quants$Value[977:(977+length(13:336)-1)]
#Bratio[Bratio>10] <- 10
ss3.1$derived_quants$Label[977:(977+length(13:336)-1)]
yr <- 13:336
plot(yr,F,type="l",xlim=c(296,336))
abline(h=1)



Forecatch <- ss3.06$derived_quants$Value[1641:(1642+length(297:334))]
#Bratio[Bratio>10] <- 10
ss3.06$derived_quants$Label[1641:(1642+length(297:334))]
yr <- 297:336
plot(yr,Forecatch,type="l",xlim=c(296,336))
abline(h=1)
