
library(r4ss)
library(mvtnorm)
library(plyr)
library(kobe)
library(png)


proj_dir = here::here()
setwd(proj_dir)

source("code/auxiliary_functions.R")
# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)


nms <- list.dirs(path="projections/FinalGrid",full.names=FALSE)[-c(1,2,3,4,5)]
nms
grid <- nms



GridList <- nms
gridDir <- file.path("projections")


results = list()

	GridName <- "NoScale"
	Grid = NULL
	tacs = seq(0.6,1.2,0.05)
	pars =c('Bratio','F')
	Bfloor = 0.01
	Fceiling = 10
	for (i in 1:length(GridList)) { 
        for(j in 1:length(tacs)){
          repf = paste0("Reportc",tacs[j],".sso") 
          compf = paste0("CompReportc",tacs[j],".sso") 		  
          out=SS_output(dir=paste0(gridDir,"/",GridList[i]),repfile = repf,compfile = compf,covar=F,readwt = FALSE)
          y <- out$derived_quants
          yprj <- subset(y, substring(y$Label, 1, 9) == "ForeCatch")
          fyrs= (substr(y[y$Label %in% paste0(paste0(pars[1],"_"),1:100000),1],8,20))
          yprj <- subset(y, substring(y$Label, 1, 9) == "ForeCatch")
          prjyr = as.numeric(substring(yprj$Label, 11,100))
          maxYear <- max(as.numeric(fyrs))
          minYear <- min(as.numeric(fyrs))
          yrs = minYear:maxYear
          
          if (length(which(substring(y$Label, 1, 9) == "ForeCatch" &  (y$Value < 0 | y$Value>1e6))) >= 1) {
            crashElement <- min(which(substring(y$Label, 1, 9) == "ForeCatch" & (y$Value < 0 | y$Value>1e6)))
            firstCrashYear <- substring(y[crashElement, ]$Label, 11, 14)
            yrs_crashed = (as.numeric(firstCrashYear) - 1):as.numeric(maxYear)
            y[y$Label %in% paste0(paste0(pars[1],"_"),yrs_crashed),2]= Bfloor
            y[y$Label %in% paste0(paste0(pars[2],"_"),yrs_crashed),2]= Fceiling
          }
          if (length(which(substring(y$Label, 1, 2) == "F_" &  (y$Value < 0 | y$Value>10))) >= 1) {
            crashElement <- min(which(substring(y$Label, 1,2) == "F_" & (y$Value < 0 | y$Value>10)))
            firstCrashYear <- substring(y[crashElement, ]$Label, 3, 6)
            yrs_crashed = (as.numeric(firstCrashYear) - 1):as.numeric(maxYear)
            y[y$Label %in% paste0(paste0(pars[1],"_"),yrs_crashed),2]= Bfloor
            y[y$Label %in% paste0(paste0(pars[2],"_"),yrs_crashed),2]= Fceiling
          }          
          Grid = rbind(Grid,data.frame(model=GridList[i],yr=yrs,stock=(y[y$Label %in% paste0(paste0(pars[1],"_"),fyrs),2]),harvest=(y[y$Label %in% paste0(paste0(pars[2],"_"),fyrs),2]),tac=yprj$Value[1],tac.perc=tacs[j],type=ifelse(yrs %in% prjyr,"prj","fit"))) 
        }
	}
	 results[[GridName]]= Grid
 #}


save(results,file="Grid.MVLN.RData")


#-------------------------------------------
# Create MVLN Posteriors for IO BET Grid
# written by Henning Winker (2019)
# henning.winker@gmail.com
#-------------------------------------------
load(file="Grid.MVLN.RData")
my.path= file.path("projections",'projectionMVN')
dir.create(my.path)
assessment = "yft_noScale"
# Grid = data.frame(model = c(results[['gridIO']]$model,results[['gridSpatio']]$model),
# 					 yr = c(results[['gridIO']]$yr,results[['gridSpatio']]$yr),
# 			      stock = c(results[['gridIO']]$stock,results[['gridSpatio']]$stock),
# 			     harvest = c(results[['gridIO']]$harvest,results[['gridSpatio']]$harvest),
# 			         tac = c(results[['gridIO']]$tac,results[['gridSpatio']]$tac),
# 			    tac.perc = c(results[['gridIO']]$tac.perc,results[['gridSpatio']]$tac.perc),
# 			         type = c(results[['gridIO']]$type,results[['gridSpatio']]$type))

 Grid = data.frame(model = c(results[['NoScale']]$model),
 					 yr = c(results[['NoScale']]$yr),
 			      stock = c(results[['NoScale']]$stock),
 			     harvest = c(results[['NoScale']]$harvest),
 			         tac = c(results[['NoScale']]$tac),
 			    tac.perc = c(results[['NoScale']]$tac.perc),
 			         type = c(results[['NoScale']]$type))


	
Grid$yr =  floor((Grid$yr-13)/4+1950+1/8) 			
Grid= aggregate(cbind(stock,harvest)~model+tac.perc+yr,Grid,mean)


# "More" Generic section
mods = unique(Grid$model)
mc = 10000 # number mvn Monte-Carlo iteration per year/tac
run = "NoScale"
tacs = seq(0.6,1.2,0.05)#seq(60,120,10) # TAC is here % of current catch
yrs = 2024:2033 # Projection horizon
kbprjs = NULL # object to store projected kobe (stock/harvest) 
#------------------------------------------------------
# Penalties to prevent implausible MVLN for high F/TAC
#------------------------------------------------------
Bfloor = 0.01 # minimum biomass (here prevents negatives)
Fceiling = 10 # F cap
# Penalty on variances in cov()
maxmult.sigma2 = 5 # upper variance limit as maxmult.sigma2*var for the last fitted year (here covref in 2017)
maxmult.cor = 5 # lower (neg.) correlation limit as maxmult.cor*cor for the last fitted year (here covref in 2017)
weighting = c(FALSE,TRUE)[1] # If TRUE - this will require case-specific manimulation 
#--------------------------------------------------------
j = 1 # index tacs
y = 1 # index years
for(j in 1:length(tacs)){
for(y in 1:length(yrs)){
gr1 = Grid[Grid$yr==yrs[y] & Grid$tac.perc==tacs[j],] #><> use tac and yr!
gr2 = gr1 #><> now aggregate to yr on top
gr2$stock[gr2$harvest<0] = Bfloor
gr2$harvest[gr2$harvest<0 | gr2$harvest>Fceiling |gr2$stock==Bfloor] = Fceiling
gr2$harvest[gr2$harvest==0] = 0.001


if(weighting==TRUE){
#### IGNORE FOR EQUAL WEIGHTING
weight = ifelse(substring(gr2$model,9,9)==paste(1),4,1)
gr3 = gr2[rep(1:nrow(gr2),weight),] # apply weighting q1 : q2 = 3:1 for yft Grid
} else {
gr3 = gr2  
}
mvnmu = apply(log(gr3[,c("stock","harvest")]),2,median)  
mvncov = cov(log(gr3[,c("stock","harvest")]))
if(y==1 & j==1){
covref = mvncov  
}
# Penalties
if(mvncov[1,2]<covref[1,2]*maxmult.cor){
mvncov[1,2] = mvncov[2,1] = covref[1,2]*maxmult.cor
}
mvncov[1,1]=max(0.01,min(covref[1,1]*maxmult.sigma2,mvncov[1,1]))
mvncov[2,2]=max(0.01,min(covref[2,2]*maxmult.sigma2,mvncov[2,2]))

# Create MVLN Kobe posteriors 
kb.temp = data.frame(iter=1:mc,year=yrs[y],tac=tacs[j],exp(rmvnorm(mc ,mean = mvnmu,sigma = mvncov,method="svd")),run=run) # random  MVN generator
kbprjs = rbind(kbprjs,kb.temp)
}}


#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
# End of KOBE projection posterior generation - now start plotting
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>

kbPrj=transform(kbprjs,tac)
k2sm =  kbPrj 
nTAC = length(tacs)

my.path <- "projectionMVN"
# Prepare matrices using library(kob)
t.=ddply(k2sm,.(year,tac), with, kobe:::smry(stock,harvest))
t.[t.$tac==0.9,]
Par = list(mfrow=c(2,1),mar = c(2, 3, 1., 0.1),oma=c(1.5, 0, 0, 6), mgp =c(2.,0.5,0), tck = -0.02,cex=0.8)
png(file = paste0("projectionMVN","/ProjFLR_",assessment,".png"), width = 5, height = 5, 
    res = 200, units = "in")
par(Par)
# Stock
cols = rainbow(length(tacs))
plot(t.$year,t.$stock,ylim=c(0,max(c(t.$stock,2))),type="n",xlab="Year",ylab=expression(SSB/SSB[MSY]))
for(t in 1:nTAC){
  temp = t.[t.$tac==tacs[t],] 
  lines(temp$year,temp$stock,col=cols[t],lwd=1.5)  
}
abline(h=1,lty=2)
legend(par('usr')[2]+1, par('usr')[3]+0.5, bty='n', xpd=NA,
       paste0(tacs,"%"),pch=15,col=cols,pt.cex=2,cex=0.7)

# Harvest
plot(t.$year,t.$harvest,ylim=c(0,max(c(t.$harvest,2))),type="n",xlab="Year",ylab=expression(F/F[MSY]))
for(t in 1:nTAC){
  temp = t.[t.$tac==tacs[t],] 
  lines(temp$year,temp$harvest,col=cols[t],lwd=1.5)  
}
abline(h=1,lty=2)
mtext(paste("Years"), side=1, outer=TRUE, at=0.5,line=-0.3,cex=.8)
dev.off()


#---------------------------------
# Select years to be shown in K2SM
#---------------------------------
sel.yr = yrs

# Compile projection matrices in tables 
k2smTab=list()
k2smTab[[1]]=cast(t.,tac~year,value="overFishing")
k2smTab[[2]]=cast(t.,tac~year,value="overFished")
k2smTab[[3]]=cast(t.,tac~year,value="green")

# text size
tx = 0.75
TACs = tacs
#Loop through B > Bmsy, F < Fmsy and Kobe K2SM tables (ICCAT style)
for(k in 1:3){
  # Define Projection matrix
  pjm = as.matrix(k2smTab[[k]])
  pjm = round(pjm[,which(colnames(pjm)%in%paste(sel.yr))]*100,0)
  ypr = as.numeric(colnames(pjm))
  npr = length(ypr)
  mat.names = c("/pjmF_","/pjmB_","/pjmKobe_")
  
  
  # Write table
  pjm.save = k2smTab[[k]]
  pjm.save[,2:ncol(pjm.save)] = round(pjm.save[,2:ncol(pjm.save)]*100,1)
  write.csv(pjm.save,file.path(my.path,paste0(mat.names[k],assessment,".csv")),row.names = FALSE)
  
  op <- par(mfrow = c(1,1),mar = rep(0, 4),mai = rep(0, 4),omi = rep(0, 4))
  png(file = paste0(my.path,"/",paste(mat.names[k]),assessment,".png"), width = 6.5, height = 6.5*nTAC/npr*0.4 , 
      res = 200, units = "in")
  par(op)
  
  plot(1,1,axes=FALSE,frame.plot=FALSE,xlab="",ylab="",type="n",ylim=c(0,nTAC+1),xlim=c(-1,npr+1))
  # first line
  rect(-1,1:nTAC+1,1,0:nTAC);  
  text(rep(0,nTAC+1),seq(0.5,nTAC+0.5,1),c(rev(paste(TACs)),"TAC | Year"),cex=tx)
  
  # Set grey shading
  # Set grey shading
  mat = pjm/100
  mat[mat<0.5]=-1
  mat=(1-mat)*2
  mat[mat>1]=1
  
  rect(1:npr,rep(nTAC+1,npr),1:(npr+1),rep(nTAC,npr))
  text(c(seq(1.5,npr+0.5,1)),rep(nTAC+0.5,npr),c(paste(ypr)),cex=tx)
  for(t in 1:nTAC) rect(1:npr,rep(nTAC+1-t,npr),2:(npr+1),rep(nTAC-t,npr),col=grey(mat[t,],0.5))
  for(t in 1:nTAC) text(c(seq(1.5,npr+0.5,1)),rep(nTAC+0.5-t,npr),paste(pjm[t,]),cex=tx)
  
  
  dev.off()
}

#--------------------------------------------------------
# Do posterior check for 2020 of combined KOBE
#--------------------------------------------------------

plot.yr = 2027
get_plot =paste0(my.path,"/KobePRJ_",plot.yr,".png")
yr.prj = kbprjs[kbprjs$year==plot.yr,]
kpd = data.frame(yr.prj[4:5],run=ifelse(yr.prj$tac>=100,paste0("z",yr.prj$tac),paste0("a",yr.prj$tac)))

row.names(kpd) = 1:nrow(kpd)

# Make plot
Par = list(mfrow=c(1,1),mar = c(5, 5, 1, 1), mgp =c(3,1,0), tck = -0.02,cex=0.8)
png(file = get_plot, width = 6.5, height = 5.5, 
    res = 200, units = "in")
par(Par)
kobe:::kobePhaseMar2(transform(kpd,run=paste(run))[,c("stock","harvest","run")]
                     ,col =alpha(rainbow(length(unique(kpd$run))),0.5)
                     ,xlab = expression(SSB/SSB[TGT]),ylab = expression(F/F[TGT]),ylim =c(10),xlim=c(3))
dev.off()



#--------------------------------------------------------
# Do posterior check for 2027 (10 yrs projections)
#--------------------------------------------------------
plot.yr = 2033
get_plot =paste0(my.path,"/KobePRJ_",plot.yr,".png")
yr.prj = kbprjs[kbprjs$year==plot.yr,]
kpd = data.frame(yr.prj[4:5],run=ifelse(yr.prj$tac>=100,paste0("z",yr.prj$tac),paste0("a",yr.prj$tac)))

row.names(kpd) = 1:nrow(kpd)

# Make plot
Par = list(mfrow=c(1,1),mar = c(5, 5, 1, 1), mgp =c(3,1,0), tck = -0.02,cex=0.8)
png(file = get_plot, width = 6.5, height = 5.5, 
    res = 200, units = "in")
par(Par)
kobe:::kobePhaseMar2(transform(kpd,run=paste(run))[,c("stock","harvest","run")]
                     ,col =alpha(rainbow(length(unique(kpd$run))),0.5)
                     ,xlab = expression(SSB/SSB[TGT]),ylab = expression(F/F[TGT]),ylim =c(10),xlim=c(3))
dev.off()

# Beautify
DIMs=c(6*2,5.5)
# setup plot
par(mar=rep(0,4),omi= c(0, 0, 0, 0)) # no margins

# layout the plots into a matrix w/ 12 columns, by row
layout(matrix(1:2, ncol=2, byrow=TRUE))

plot.yr = 2024
get_plot =paste0(my.path,"/KobePRJ_",plot.yr,".png")

# example image
img <- readPNG(paste0(get_plot))

# do the plotting
plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
rasterImage(img,0,0,1,1)
legend(0.84,0.98, 
       paste0(tacs,"%"), 
       lty=c(rep(-1,7)),pt.bg=alpha(rainbow(length(unique(kpd$run))),0.5),pch=22, 
       col=1,lwd=1.1,cex=0.8,pt.cex=c(rep(1.7,3)),bty="n",x.intersp = .1,y.intersp = 1.3)

legend(0.3,1.05,paste(plot.yr),bty="n",cex=1.3)

plot.yr = 2033
get_plot =paste0(my.path,"/KobePRJ_",plot.yr,".png")

# example image
img <- readPNG(paste0(get_plot))

# do the plotting
plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
rasterImage(img,0,0,1,1)
legend(0.84,0.98, 
       paste0(tacs,"%"), 
       lty=c(rep(-1,7)),pt.bg=alpha(rainbow(length(unique(kpd$run))),0.5),pch=22, 
       col=1,lwd=1.1,cex=0.8,pt.cex=c(rep(1.7,3)),bty="n",x.intersp = .1,y.intersp = 1.3)
legend(0.3,1.05,paste(plot.yr),bty="n",cex=1.3)

dev.print(png,paste0(my.path,"/KobeProj_","2024_2033","_",assessment,".png"), width = DIMs[1], height = DIMs[2], res = 200, units = "in")

