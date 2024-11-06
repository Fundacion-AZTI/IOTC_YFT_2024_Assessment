


#library(r4ss)
#library(xlsx)
# source("F_SS_getoutput.R")
# load("SS_output_summarize.RData"

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

#grid <- c(1:48)[-c(21,4,12,16,31,24,15,35,19,27,13,17,22,23,32,5,20,28)]
#dir.origin <- c("../Grid_2A_4A_revisedCatch_2020/v13")
#dir.exists("projections")
#forecatch <- 120 #c(seq(0.6,1,0.05),c(1,1.1,1.2))*100

nms <- list.dirs(path="projections/FinalGrid",full.names=FALSE)[-c(1,2,3,4,5)]
nms
grid <- nms


#wd <- "C:/use/agur/proyectos/Atun/IOTC/YFT-IOTC/2021/SensitivityTrendsPropRec/Update3_R14_tm30/projections/scenariosFore100"
#setwd(wd)

olddir <- getwd()
outdir <- "output/results/"
outTables <- "output/tables/"
outPlots <- "output/plots/"
newdir <- paste0("")


nm <- nms

#nm <- paste("R",1:48,sep="")


#nm.read<-read.xlsx("../Readme.xlsx",sheetIndex = 1)
#nm.plots <- nm.read$description

df <- NULL

digits <- 3

for(i in 1:length(nm)){

  nmi <- nm[i]
 # load(paste("Results/",nmi,".RData",sep=""))
  ss3 <- SS_output(dir=file.path("models","FinalGrid",nm[i]),  repfile = "Report.sso",covar=F, ncols=500,readwt=FALSE)
  
  yr <- 2023
  helper <- read.csv("D:/OneDrive - AZTI/General - IOTC_YFT_2024/models/forecast/helper.csv")
  ss3.yr <- helper$year[helper$yr %in% yr]
  

  # sb.y_sbMsy <- ss3$Kobe[ss3$Kobe$Yr %in% ss3.yr,2]
  # mean.sb.y_sbMsy <- mean(ss3$Kobe[ss3$Kobe$Yr %in% ss3.yr,2]) 

  #relmsy.f2018 <- ss3$Kobe[ss3$Kobe$Yr %in% ss3.yr,3]
  F.y_FMsy <- round(mean(ss3$derived_quants$Value[ss3$derived_quants$Label %in% paste("F_",ss3.yr,sep="")]),digits) #mean( ss3$Kobe[(281-11):(284-11),3])

  sb0 <- ss3$derived_quants$Value[ss3$derived_quants$Label=="SSB_Virgin"]
  sb.y <- mean(ss3$derived_quants$Value[ss3$derived_quants$Label %in% paste("SSB_",ss3.yr,sep="")])
  sbMsy <- ss3$derived_quants$Value[ss3$derived_quants$Label=="SSB_MSY"]
 # FMsy <- ss3$derived_quants$Value[ss3$derived_quants$Label=="Fstd_MSY"]*4 #v3.30.14
  FMsy <- ss3$derived_quants$Value[ss3$derived_quants$Label=="annF_MSY"]*4 #v3.30.15
  Ftg <- ss3$derived_quants$Value[ss3$derived_quants$Label=="annF_Btgt"]*4
  
  TotYield.MSY <- ss3$derived_quants[ss3$derived_quants$Label=="Dead_Catch_MSY",]
  F.y_FBtg <- F.y_FMsy*FMsy/Ftg
  
  
  Msy <- 4* TotYield.MSY$Value
  sbMsy_sb0 <- sbMsy/sb0
  sb.y_sb0 <- round(sb.y/sb0,digits)
  sb.y_04sb0 <- round(sb.y/(0.4*sb0),digits)
  sb.y_sbMsy <- round(sb.y/sbMsy,digits)

  ss3.sc <- c(sb0,sbMsy,sbMsy_sb0,sb.y,sb.y_sb0,sb.y_04sb0,sb.y_sbMsy,F.y_FBtg,F.y_FMsy,FMsy,Msy)

#  df <- ss3.sc
  df <- rbind(df,ss3.sc)

}

df <- as.data.frame(df)
names(df) <- c("SB0","SBMsy","SBMsy/SB0","SB.2023","SB.2023_SB0","SB.2023_04SB0","SB.2023_SBMsy","F.y_FBtg","F.2023_FMsy","FMsy","Msy")

row.names(df)<- nm
#row.names(df) <- nm
# write.csv(df,file="Tables/test.csv",
#           row.names=TRUE)

write.csv(df,file="output/tables/StockStatus_notScaled.csv",
          row.names=TRUE)


#....................................................

#     global summary

#........................................................


df$Run <- paste0("R",1:36)
df0 <- df
df <- df0[-23,]
#mean values

mean(df$SB.2018_SBMsy,na.rm=TRUE)
mean(df$F.2018_FMsy,na.rm=TRUE)

#min and max

range(df$SB.2018_SBMsy,na.rm=TRUE)
range(df$F.2018_FMsy,na.rm=TRUE)

  u <- which.min(df$SB.2018_SBMsy)
  row.names(df)[u]
  df$Run[u]
  
  u <- which.max(df$SB.2018_SBMsy)
  row.names(df)[u]
  df$Run[u]

  
  u <- which.min(df$F.2018_FMsy)
  row.names(df)[u]
  df$Run[u]
  
  u <- which.max(df$F.2018_FMsy)
  row.names(df)[u]
  df$Run[u]


  #....................................................
  
  #     Based on tagging
  
  #........................................................
  
 df <- df0 
rowslbda0 <- 1:12
rowslbda01 <- seq(13,36,2)[-6]
rowslbda1 <- seq(14,36,2)

  #mean values
  
  mean(df$SB.2018_SBMsy[rowslbda0],na.rm=TRUE)
  mean(df$SB.2018_SBMsy[rowslbda01],na.rm=TRUE)
  mean(df$SB.2018_SBMsy[rowslbda1],na.rm=TRUE)
  
  mean(df$F.2018_FMsy[rowslbda0],na.rm=TRUE)
  mean(df$F.2018_FMsy[rowslbda01],na.rm=TRUE)
  mean(df$F.2018_FMsy[rowslbda1],na.rm=TRUE)
  
  
  #min and max
  
  range(df$SB.2018_SBMsy[rowslbda0],na.rm=TRUE)
  range(df$SB.2018_SBMsy[rowslbda01],na.rm=TRUE)
  range(df$SB.2018_SBMsy[rowslbda1],na.rm=TRUE)
  range(df$F.2018_FMsy[rowslbda0],na.rm=TRUE)
  range(df$F.2018_FMsy[rowslbda01],na.rm=TRUE)
  range(df$F.2018_FMsy[rowslbda1],na.rm=TRUE)
  
  #min
  u <- which.min(df$SB.2018_SBMsy[rowslbda0])
  row.names(df[rowslbda0,])[u]
  df$Run[rowslbda0][u]
  
  u <- which.min(df$SB.2018_SBMsy[rowslbda01])
  row.names(df[rowslbda01,])[u]
  df$Run[rowslbda01][u]
  
  u <- which.min(df$SB.2018_SBMsy[rowslbda1])
  row.names(df[rowslbda1,])[u]
  df$Run[rowslbda1][u]
  #max 
  u <- which.max(df$SB.2018_SBMsy[rowslbda0])
  row.names(df[rowslbda0,])[u]
  df$Run[rowslbda0][u]
  u <- which.max(df$SB.2018_SBMsy[rowslbda01])
  row.names(df[rowslbda01,])[u]
  df$Run[rowslbda01][u]  
  u <- which.max(df$SB.2018_SBMsy[rowslbda1])
  row.names(df[rowslbda1,])[u]
  df$Run[rowslbda1][u]
  
  
  #.......F......................
  

  
  u <- which.min(df$F.2018_FMsy[rowslbda0])
  row.names(df[rowslbda0,])[u]
  df$Run[rowslbda0][u]
  u <- which.min(df$F.2018_FMsy[rowslbda01])
  row.names(df[rowslbda01,])[u]
  df$Run[rowslbda01][u]
  u <- which.min(df$F.2018_FMsy[rowslbda1])
  row.names(df[rowslbda1,])[u]
  df$Run[rowslbda1][u]
  #max 
  u <- which.max(df$F.2018_FMsy[rowslbda0])
  row.names(df[rowslbda0,])[u]
  df$Run[rowslbda0][u]
  u <- which.max(df$F.2018_FMsy[rowslbda01])
  row.names(df[rowslbda01,])[u]
  df$Run[rowslbda01][u]  
  u <- which.max(df$F.2018_FMsy[rowslbda1])
  row.names(df[rowslbda1,])[u]
  df$Run[rowslbda1][u]


  
  #....................................................
  
  #     Based on mortality
  
  #........................................................
  
  df <- df0 
  rowsMB <- c(seq(1,12,2),seq(13,36,4),seq(14,36,4))
  rowsML <- c(seq(2,12,2),seq(15,36,4),seq(16,36,4))[-9]

  
  #mean values
  
  mean(df$SB.2018_SBMsy[rowsMB],na.rm=TRUE)
  mean(df$SB.2018_SBMsy[rowsML],na.rm=TRUE)

  mean(df$F.2018_FMsy[rowsMB],na.rm=TRUE)
  mean(df$F.2018_FMsy[rowsML],na.rm=TRUE)

  
  #min and max
  
  range(df$SB.2018_SBMsy[rowsMB],na.rm=TRUE)
  range(df$SB.2018_SBMsy[rowsML],na.rm=TRUE)
  range(df$F.2018_FMsy[rowsMB],na.rm=TRUE)
  range(df$F.2018_FMsy[rowsML],na.rm=TRUE)

  #min
  u <- which.min(df$SB.2018_SBMsy[rowsMB])
  row.names(df[rowsMB,])[u]
  df$Run[rowsMB][u]
  
  u <- which.min(df$SB.2018_SBMsy[rowsML])
  row.names(df[rowsML,])[u]
  df$Run[rowsML][u]

    #max 
  u <- which.max(df$SB.2018_SBMsy[rowsMB])
  row.names(df[rowsMB,])[u]
  df$Run[rowsMB][u]
  
  u <- which.max(df$SB.2018_SBMsy[rowsML])
  row.names(df[rowsML,])[u]
  df$Run[rowsML][u]  

  
  #.......F......................
  
  
  
  u <- which.min(df$F.2018_FMsy[rowsMB])
  row.names(df[rowsMB,])[u]
  df$Run[rowsMB][u]
  
  u <- which.min(df$F.2018_FMsy[rowsML])
  row.names(df[rowsML,])[u]
  df$Run[rowsML][u]

    #max 
  u <- which.max(df$F.2018_FMsy[rowsMB])
  row.names(df[rowsMB,])[u]
  df$Run[rowsMB][u]
  
  u <- which.max(df$F.2018_FMsy[rowsML])
  row.names(df[rowsML,])[u]
  df$Run[rowsML][u]  

  
  
  #....................................................
  
  #     Based on growth
  
  #........................................................
  
  df <- df0 
  rowsGF <- c(seq(1,12,4),seq(2,12,4),seq(13,36,8),seq(14,36,8),seq(15,36,8),seq(16,36,8))[-14]
  rowsGD <- c(seq(3,12,4),seq(4,12,4),seq(17,36,8),seq(18,36,8),seq(19,36,8),seq(20,36,8))

  
  
  #mean values
  
  mean(df$SB.2018_SBMsy[rowsGF],na.rm=TRUE)
  mean(df$SB.2018_SBMsy[rowsGD],na.rm=TRUE)
  
  mean(df$F.2018_FMsy[rowsGF],na.rm=TRUE)
  mean(df$F.2018_FMsy[rowsGD],na.rm=TRUE)
  
  
  #min and max
  
  range(df$SB.2018_SBMsy[rowsGF],na.rm=TRUE)
  range(df$SB.2018_SBMsy[rowsGD],na.rm=TRUE)
  range(df$F.2018_FMsy[rowsGF],na.rm=TRUE)
  range(df$F.2018_FMsy[rowsGD],na.rm=TRUE)
  
  #min
  u <- which.min(df$SB.2018_SBMsy[rowsGF])
  row.names(df[rowsGF,])[u]
  df$Run[rowsGF][u]
  
  u <- which.min(df$SB.2018_SBMsy[rowsGD])
  row.names(df[rowsGD,])[u]
  df$Run[rowsGD][u]
  
  #max 
  u <- which.max(df$SB.2018_SBMsy[rowsGF])
  row.names(df[rowsGF,])[u]
  df$Run[rowsGF][u]
  
  u <- which.max(df$SB.2018_SBMsy[rowsGD])
  row.names(df[rowsGD,])[u]
  df$Run[rowsGD][u]  
  
  
  #.......F......................
  
  
  
  u <- which.min(df$F.2018_FMsy[rowsGF])
  row.names(df[rowsGF,])[u]
  df$Run[rowsGF][u]
  
  u <- which.min(df$F.2018_FMsy[rowsGD])
  row.names(df[rowsGD,])[u]
  df$Run[rowsGD][u]
  
  #max 
  u <- which.max(df$F.2018_FMsy[rowsGF])
  row.names(df[rowsGF,])[u]
  df$Run[rowsGF][u]
  
  u <- which.max(df$F.2018_FMsy[rowsGD])
  row.names(df[rowsGD,])[u]
  df$Run[rowsGD][u]  
  
  
  #....................................................
  
  #     Based on steepness
  
  #........................................................
  
  df <- df0 
  rowsh08 <- c(1:4,13:20)
  rowsh07 <- c(5:8,21:28)[-7]
  rowsh09 <- c(9:12,29:36)
  
  #mean values
  
  mean(df$SB.2018_SBMsy[rowsh07],na.rm=TRUE)
  mean(df$SB.2018_SBMsy[rowsh08],na.rm=TRUE)
  mean(df$SB.2018_SBMsy[rowsh09],na.rm=TRUE)
  
  mean(df$F.2018_FMsy[rowsh07],na.rm=TRUE)
  mean(df$F.2018_FMsy[rowsh08],na.rm=TRUE)
  mean(df$F.2018_FMsy[rowsh09],na.rm=TRUE)
  
  
  #min and max
  
  range(df$SB.2018_SBMsy[rowsh07],na.rm=TRUE)
  range(df$SB.2018_SBMsy[rowsh08],na.rm=TRUE)
  range(df$SB.2018_SBMsy[rowsh09],na.rm=TRUE)
  range(df$F.2018_FMsy[rowsh07],na.rm=TRUE)
  range(df$F.2018_FMsy[rowsh08],na.rm=TRUE)
  range(df$F.2018_FMsy[rowsh09],na.rm=TRUE)
  
  #min
  u <- which.min(df$SB.2018_SBMsy[rowsh07])
  row.names(df[rowsh07,])[u]
  df$Run[rowsh07][u]
  
  u <- which.min(df$SB.2018_SBMsy[rowsh08])
  row.names(df[rowsh08,])[u]
  df$Run[rowsh08][u]
  
  u <- which.min(df$SB.2018_SBMsy[rowsh09])
  row.names(df[rowsh09,])[u]
  df$Run[rowsh09][u]
  #max 
  u <- which.max(df$SB.2018_SBMsy[rowsh07])
  row.names(df[rowsh07,])[u]
  df$Run[rowsh07][u]
  u <- which.max(df$SB.2018_SBMsy[rowsh08])
  row.names(df[rowsh08,])[u]
  df$Run[rowsh08][u]  
  u <- which.max(df$SB.2018_SBMsy[rowsh09])
  row.names(df[rowsh09,])[u]
  df$Run[rowsh09][u]
  
  #.......F......................
  
  
  
  u <- which.min(df$F.2018_FMsy[rowsh07])
  row.names(df[rowsh07,])[u]
  df$Run[rowsh07][u]
  u <- which.min(df$F.2018_FMsy[rowsh08])
  row.names(df[rowsh08,])[u]
  df$Run[rowsh08][u]
  u <- which.min(df$F.2018_FMsy[rowsh09])
  row.names(df[rowsh09,])[u]
  df$Run[rowsh09][u]
  #max 
  u <- which.max(df$F.2018_FMsy[rowsh07])
  row.names(df[rowsh07,])[u]
  df$Run[rowsh07][u]
  u <- which.max(df$F.2018_FMsy[rowsh08])
  row.names(df[rowsh08,])[u]
  df$Run[rowsh08][u]  
  u <- which.max(df$F.2018_FMsy[rowsh09])
  row.names(df[rowsh09,])[u]
  df$Run[rowsh09][u]
  