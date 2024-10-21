
#*************
# INSTALL
#*************
#devtools::install_github("JABBAmodel/ss3diags")
library(r4ss)
library(ss3diags)

wd <- "D:/OneDrive - AZTI/General - IOTC_YFT_2024/models/retrospectives_ref_models/models"
setwd(wd)

#setwd("C:/Users/henni/Dropbox/YFT_IO")

#---------------------------------------
# load ss outputs
#--------------------------------------

Model = "4_Dwtag01"
nm <- Model
output.dir <- paste0(nm,"/plotsDiag")

readss3 = TRUE
covar =FALSE # Not available for Annual model
if(readss3==TRUE){
mod =  ss3rep=SS_output(dir=Model,covar=covar,ncol=1000,forecast=FALSE)
# Adjust retro steps here
subdirs = paste0("retrospectives/retro",c(0,-seq(4,20,4))) # Difine SS3 model folders
# Compile models
retroModels <- SSgetoutput(dirvec=file.path(getwd(),Model,subdirs),getcovar=FALSE,forecast=FALSE)
# Summarize outputs
retros <- SSsummarize(retroModels)
# Adjust model names here
#dir.create(file.path(getwd(),"Results"),showWarnings = F)
save(mod,retros,file = paste0("Results/",Model,".rdata"))
} else {
  load(paste0("Results/",Model,".rdata"))
}

#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
# The 2018/2019 IOTC yft setup is based on 284 time steps
# Each time step represents a quarter
# ss3diags now includes a function to convert non-annual to annual time steps 
# to allow running the suit of inbuild routine diags
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>

# define last time step of observations
end.time= 2033.75
time.steps = 0.25
# For rep file (Runs Test (Len/Cpue) and JointResiduals)
ss3rep = SSdiagsTime2Year(mod,time.step=0.25,end.time=2033.75)
# For SSsummarize() output (Retro + HCxval) 
retroSummary = SSdiagsTime2Year(retros,time.step=0.25,end.time=2033.75)

# NOW RUN DIAGS

# Check Runs tests with seasonal time intervals
# sspar(mfrow=c(2,2),plot.cex =0.7)
# SSplotRunstest(ss3rep,add=T)
# SSplotRunstest(ss3rep,add=T, legend=TRUE, legendcex = 0.5)
out <- ss3rep
out$cpue <- out$cpue[out$cpue$Yr<=2023,]
out$lendbase <- out$lendbase[out$lendbase$Yr<=308,]
plname = "YFT_runstests"
pwidth = 8
pheight = 6
res=300
windows(width=pwidth,height=pheight)
sspar(mfrow=c(2,2),labs=T,plot.cex=1)
SSplotRunstest(out,subplots = "cpue",add=T)
mtext("Year",side=1,outer=T,cex=1,line=-0.8)
dev.print(jpeg,paste0(output.dir,"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")

SScompsTA1.8


plname = "YFT_runstests_length"
pwidth = 8
pheight = 6
res=300
windows(width=pwidth,height=pheight)
sspar(mfrow=c(5,5),labs=F,plot.cex=1)
SSplotRunstest(out,subplots = "len",add=T, legend=TRUE, legendcex = 0.5)
mtext("Year",side=1,outer=T,cex=1,line=-0.8)
mtext("Mean length residuals",side=2,outer=T,cex=1,line=0.6)
dev.print(jpeg,paste0(output.dir,"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")


# Check Joint Residual Plot
#sspar(mfrow=c(1,1))
#SSplotJABBAres(ss3rep,add=T)
# Plot Model RMSE 
plname = "YFT_RMSE"
pwidth = 8
pheight = 6
res=300
windows(width=pwidth,height=pheight)
sspar(mfrow=c(1,2),labs=T,plot.cex=1)
SSplotJABBAres(out,subplots = "cpue",add=T)
SSplotJABBAres(out,subplots = "len",add=T)
mtext("Year",side=1,outer=T,cex=1,line=-0.8)
dev.print(jpeg,paste0(output.dir,"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")

# Check for Joint Residual by Region
#sspar(mfrow=c(2,2),plot.cex = 0.7)
#SSplotJABBAres(ss3rep,indexselect = c(1,2),add=T,cex.main = 0.8)
#mtext("Northern IO")
#SSplotJABBAres(ss3rep,indexselect = c(3:4),add=T,cex.main = 0.8)
#mtext("Southern IO")
#SSplotJABBAres(ss3rep,indexselect = c(1,3),add=T,cex.main = 0.8)
#mtext("Western IO")
#SSplotJABBAres(ss3rep,indexselect = c(2,4),add=T,cex.main = 0.8)
#mtext("Eastern IO")


plname = "YFT_RMSE by region"
pwidth = 8
pheight = 6
res=300
windows(width=pwidth,height=pheight)
sspar(mfrow=c(2,2),plot.cex = 0.7)
SSplotJABBAres(out,indexselect = c(1,4),add=T,cex.main = 0.8)
mtext("Northern IO")
SSplotJABBAres(out,indexselect = c(2,3),add=T,cex.main = 0.8)
mtext("Southern IO")
SSplotJABBAres(out,indexselect = c(1,2),add=T,cex.main = 0.8)
mtext("Western IO")
SSplotJABBAres(out,indexselect = c(3,4),add=T,cex.main = 0.8)
mtext("Eastern IO")
mtext("Year",side=1,outer=T,cex=1,line=-0.8)

dev.print(jpeg,paste0(output.dir,"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")

# if(covar==TRUE){
# # Check Status 
# sspar(labs = T)
# status = SSdeltaMVLN(ss3rep,run="R412")
# 
# # Plot trajectories with deltaMVLN derived CIs
# sspar(mfrow=c(2,2),plot.cex = 0.8)
# SSplotEnsemble(status$kb,add=T,legend = F)
# }
# 
# # Do Retro plus Forecast
# sspar(mfrow=c(2,1),plot.cex = 0.7)
# SSplotRetro(retroSummary,add=T,forecast=T,forcastrho=T)
# SSplotRetro(retroSummary,add=T,forecast=T,forcastrho=T,xmin = 2000)


# Hindcasting Season 1 for survey
retroSummary$indices<- retroSummary$indices[retroSummary$indices$Yr<=2023,]
retroSummary$endyrs <- rep(2023,6)
plname = "YFT_hindcasting season 1"
pwidth = 8
pheight = 6
res=300
windows(width=pwidth,height=pheight)
sspar(mfrow=c(2,2),plot.cex = 0.7,labs=F)
SSplotHCxval(retroSummary,add=T,xmin=2000, Season=1)
mtext("Year",side=1,outer=T,cex=1,line=0.5)
mtext("Index",side=2,outer=T,line=0.5,cex=1)
dev.print(jpeg,paste0(output.dir,"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")

# Season 2
plname = "YFT_hindcasting season 2"
pwidth = 8
pheight = 6
res=300
windows(width=pwidth,height=pheight)
sspar(mfrow=c(2,2),plot.cex = 0.7,labs=F)
SSplotHCxval(retroSummary,add=T,xmin=2000,Season=2)
mtext("Year",side=1,outer=T,cex=1,line=0.5)
mtext("Index",side=2,outer=T,line=0.5,cex=1)
dev.print(jpeg,paste0(output.dir,"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")

# Season 3
plname = "YFT_hindcasting season 3"
pwidth = 8
pheight = 6
res=300
windows(width=pwidth,height=pheight)
sspar(mfrow=c(2,2),plot.cex = 0.7,labs=F)
SSplotHCxval(retroSummary,add=T,xmin=2000,Season=3)
mtext("Year",side=1,outer=T,cex=1,line=0.5)
mtext("Index",side=2,outer=T,line=0.5,cex=1)
dev.print(jpeg,paste0(output.dir,"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")

# Season 4
plname = "YFT_hindcasting season 4"
pwidth = 8
pheight = 6
res=300
windows(width=pwidth,height=pheight)
sspar(mfrow=c(2,2),plot.cex = 0.7,labs=F)
#Specify Season
SSplotHCxval(retroSummary,add=T,xmin=2000,Season=4)
mtext("Year",side=1,outer=T,cex=1,line=0.5)
mtext("Index",side=2,outer=T,line=0.5,cex=1)
dev.print(jpeg,paste0(output.dir,"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")

retroSummary$endyrs <- rep(2024,6)
retroSummary$SpawnBio <- retroSummary$SpawnBio[retroSummary$SpawnBio$Yr<=2024,]
retroSummary$SpawnBioLower <- retroSummary$SpawnBioLower[retroSummary$SpawnBioLower$Yr<=2024,]
retroSummary$SpawnBioUpper <- retroSummary$SpawnBioUpper[retroSummary$SpawnBioUpper$Yr<=2024,]
retroSummary$SpawnBioSD <- retroSummary$SpawnBioSD[retroSummary$SpawnBioSD$Yr<=308,]

# Retrospective
plname = "YFT SSB retro and hindcasting"
pwidth = 8
pheight = 6
res=300
windows(width=pwidth,height=pheight)
sspar(mfrow=c(2,1),plot.cex = 0.7)
SSplotRetro(retroSummary,add=T,forecast=T)
SSplotRetro(retroSummary,add=T,forecast=T,xmin = 2000)
#SSplotComparisons(retroSummary, endyrvec=2024:2013, legendlabels=paste("Data",0:-5,"years"), uncertainty = T,subplots = 2,add=F,new=F)
#SSplotComparisons(retroSummary, endyrvec=2024:2013, legendlabels=paste("Data",0:-5,"years"), uncertainty = T,subplots = 4,add=F,new=F)
dev.print(jpeg,paste0(output.dir,"/",plname,".jpg"), width = pwidth, height = pheight, res = res, units = "in")


# 
# # Do HCxval
# # Season 1
# sspar(mfrow=c(2,2),plot.cex = 0.7,labs=F)
# SSplotHCxval(retroSummary,add=T,xmin=2000,legendcex = 0.7)
# mtext("Year",side=1,outer=T,cex=1,line=0.5)
# mtext("Index",side=2,outer=T,line=0.5,cex=1)
# # Season 2
# sspar(mfrow=c(2,2),plot.cex = 0.7,labs=F)
# SSplotHCxval(retroSummary,add=T,xmin=2000,Season=2,legendcex = 0.7)
# mtext("Year",side=1,outer=T,cex=1,line=0.5)
# mtext("Index",side=2,outer=T,line=0.5,cex=1)
# 
# # Season 3
# sspar(mfrow=c(2,2),plot.cex = 0.7,labs=F)
# #Specify Season
# SSplotHCxval(retroSummary,add=T,xmin=2000,Season=3,legendcex = 0.7)
# mtext("Year",side=1,outer=T,cex=1,line=0.5)
# mtext("Index",side=2,outer=T,line=0.5,cex=1)
# 
# # Season 4
# sspar(mfrow=c(2,2),plot.cex = 0.7,labs=F)
# #Specify Season
# SSplotHCxval(retroSummary,add=T,xmin=2000,Season=4,legendcex = 0.7)
# mtext("Year",side=1,outer=T,cex=1,line=0.5)
# mtext("Index",side=2,outer=T,line=0.5,cex=1)
# 
# # NOT WORKING WITH YEAR (Better you original)
# # Check LENGTH COMPS
# sspar(mfrow=c(3,3),plot.cex = 0.6,labs=F)
# SSplotRunstest(ss3rep,subplots = "len",indexselect = 1:10,add=T)
# mtext("Residuals",side=2,outer=T,line=0.5,cex=1)
# mtext("Year",side=1,outer=T,cex=1,line=0.5)
# 
# sspar(mfrow=c(3,3),plot.cex = 0.6,labs=F)
# SSplotRunstest(ss3rep,subplots = "len",indexselect = 11:19,add=T)
# mtext("Residuals",side=2,outer=T,line=0.5,cex=1)
# mtext("Year",side=1,outer=T,cex=1,line=0.5)
# 
# # Joint Residual plot
# sspar(mfrow=c(1,1),plot.cex = 0.9,labs=T)
# SSplotJABBAres(ss3rep,subplots = "len",indexselect = 11:19,add=T)
# 
