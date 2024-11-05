

library(r4ss)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

proj_dir = here::here()
setwd(proj_dir)
source(here('code', 'auxiliary_functions.R'))
# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)

dir_plot <- "output/figures/SplitCPUE/Profile"
dir_table <- "output/tables/"

scs <- "R2M_H07/natM"  
scs_wd <-paste0("models/update/ExploratoryRuns/",scs)


df <- NULL
for(i in 1:22){
load(file.path(scs_wd,paste0("h_steep_profile",i,".RData")))
 df <- rbind(df,profile)}


df_long <- df %>% pivot_longer(cols=names(df)[c(3:4,6:10)],names_to="LKL_Comp",values_to= "LKL")
head(df_long)

ggplot(df_long[df_long$LKL_Comp=="Survey",],aes(x=Value, y=LKL, group=LKL_Comp, colour=LKL_Comp))+
  geom_point(aes(colour=LKL_Comp))+ geom_line(aes(colour=LKL_Comp))+
  xlab("Steepness")+ ylab("LKL")+ggtitle("Steepness LKL profile")+
  theme_fun()

xx <- aggregate(LKL~LKL_Comp, df_long,min,drop=FALSE)
names(xx)[2] <- "mean_LKL"
library(dplyr)
yy <- df_long %>% 
  left_join(xx %>% select(LKL_Comp,mean_LKL))

head(yy[,c(1,12:14)])
yy$Dif <- yy$LKL-yy$mean_LKL

ggplot(yy, aes(x=Value, y=Dif, group=LKL_Comp, colour=LKL_Comp))+
  geom_point(aes(colour=LKL_Comp),size=2)+ geom_line(aes(colour=LKL_Comp),size=1)+
xlab("Steepness")+ ylab("LKL-min(LKL)")+ggtitle("Steepness LKL profile")+
  theme_fun()

SavePlot('PROFILE_STEEPNESS_min',15,10)

ggplot(yy, aes(x=Value, y=Dif, group=LKL_Comp, colour=LKL_Comp))+
  geom_point(aes(colour=LKL_Comp),size=2)+ geom_line(aes(colour=LKL_Comp),size=1)+
  xlab("Steepness")+ ylab("LKL-min(LKL)")+ggtitle("Steepness LKL profile")+
  theme_fun()+coord_cartesian(ylim = c(0, 50))

SavePlot('PROFILE_STEEPNESS_min_zoom',15,10)

#### RO ####
scs <- "R2_min/LNR0"  
scs_wd <-paste0("models/retrospectives_ref_models/models/PROFILES/",scs)
#dir_prof <- scs_wd
df <- NULL
for(i in c(1:9)){
  load(file.path(scs_wd,paste0("LN_R0_profile",i,".RData")))
  df <- rbind(df,profile)}


df_long <- df %>% pivot_longer(cols=names(df)[c(3:4,6:10)],names_to="LKL_Comp",values_to= "LKL")
head(df_long)

ggplot(df_long[df_long$LKL_Comp=="Survey",],aes(x=Value, y=LKL, group=LKL_Comp, colour=LKL_Comp))+
  geom_point(aes(colour=LKL_Comp))+ geom_line(aes(colour=LKL_Comp))+
  xlab("LNR0")+ ylab("LKL")+ggtitle("Steepness LKL profile")+
  theme_fun()

xx <- aggregate(LKL~LKL_Comp, df_long,min,drop=FALSE)
names(xx)[2] <- "mean_LKL"
library(dplyr)
yy <- df_long %>% 
  left_join(xx %>% select(LKL_Comp,mean_LKL))

head(yy[,c(1,12:14)])
yy$Dif <- yy$LKL-yy$mean_LKL

ggplot(yy, aes(x=Value, y=Dif, group=LKL_Comp, colour=LKL_Comp))+
  geom_point(aes(colour=LKL_Comp),size=2)+ geom_line(aes(colour=LKL_Comp),size=1)+
  xlab("LN_R0")+ ylab("LKL-min(LKL)")+ggtitle("LN(R0) LKL profile")+
  theme_fun()

SavePlot('PROFILE_LNR0_2',15,10)

ggplot(yy, aes(x=Value, y=Dif, group=LKL_Comp, colour=LKL_Comp))+
  geom_point(aes(colour=LKL_Comp),size=2)+ geom_line(aes(colour=LKL_Comp),size=1)+
  xlab("LN_R0")+ ylab("LKL-min(LKL)")+ggtitle("LN(R0) LKL profile")+
  theme_fun()+coord_cartesian(ylim = c(0, 50))

SavePlot('PROFILE_LNR0_zoom_2',15,10)


#### M ####
scs <- "R2_min/natM"  
scs_wd <-paste0("models/retrospectives_ref_models/models/PROFILES/",scs)

scs <- "R2M_H07/natM"  
scs_wd <-paste0("models/update/ExploratoryRuns/",scs)
scs_wd <-"models/update/ExploratoryRuns/RM2_H09/natM"
#dir_prof <- scs_wd
df <- NULL
for(i in c(1:8)){
  load(file.path(scs_wd,paste0("NatM_profile",i,".RData")))
  df <- rbind(df,profile)
  }



df_long <- df[-1,] %>% pivot_longer(cols=names(df)[c(3:4,6:10)],names_to="LKL_Comp",values_to= "LKL")
head(df_long)

ggplot(df_long[df_long$LKL_Comp=="Survey",],aes(x=Value, y=LKL, group=LKL_Comp, colour=LKL_Comp))+
  geom_point(aes(colour=LKL_Comp))+ geom_line(aes(colour=LKL_Comp))+
  xlab("LNR0")+ ylab("LKL")+ggtitle("NatM LKL profile H07")+
  theme_fun()

xx <- aggregate(LKL~LKL_Comp, df_long,min,drop=FALSE)
names(xx)[2] <- "mean_LKL"
library(dplyr)
yy <- df_long %>% 
  left_join(xx %>% select(LKL_Comp,mean_LKL))

head(yy[,c(1,12:14)])
yy$Dif <- yy$LKL-yy$mean_LKL

ggplot(yy, aes(x=Value, y=Dif, group=LKL_Comp, colour=LKL_Comp))+
  geom_point(aes(colour=LKL_Comp),size=2)+ geom_line(aes(colour=LKL_Comp),size=1)+
  xlab("Natural Mortality")+ ylab("LKL-min(LKL)")+ggtitle("H09-Natural mortality at age 4 LKL profile")+
  theme_fun()

SavePlot('PROFILE_NatM_H09',10,5)

ggplot(yy, aes(x=Value, y=Dif, group=LKL_Comp, colour=LKL_Comp))+
  geom_point(aes(colour=LKL_Comp),size=2)+ geom_line(aes(colour=LKL_Comp),size=1)+
  xlab("Natural Mortality")+ ylab("LKL-min(LKL)")+ggtitle("h09-Natural mortality at age 4 LKL profile")+
  theme_fun()+coord_cartesian(ylim = c(0, 50))

SavePlot('PROFILE_NatM_H09_zoom',15,10)

#### LINF ####
scs <- "R2_min/Linf"  
scs_wd <-paste0("models/retrospectives_ref_models/models/PROFILES/",scs)
#dir_prof <- scs_wd
df <- NULL
for(i in c(1:16)){
  load(file.path(scs_wd,paste0("Linf_profile",i,".RData")))
  df <- rbind(df,profile)
}


df_long <- df[-1,] %>% pivot_longer(cols=names(df)[c(3:4,6:10)],names_to="LKL_Comp",values_to= "LKL")
head(df_long)

ggplot(df_long[df_long$LKL_Comp=="Survey",],aes(x=Value, y=LKL, group=LKL_Comp, colour=LKL_Comp))+
  geom_point(aes(colour=LKL_Comp))+ geom_line(aes(colour=LKL_Comp))+
  xlab("LINF")+ ylab("LKL")+ggtitle("Linf LKL profile")+
  theme_fun()

xx <- aggregate(LKL~LKL_Comp, df_long,min,drop=FALSE)
names(xx)[2] <- "mean_LKL"
library(dplyr)
yy <- df_long %>% 
  left_join(xx %>% select(LKL_Comp,mean_LKL))

head(yy[,c(1,12:14)])
yy$Dif <- yy$LKL-yy$mean_LKL
ggplot(yy, aes(x=Value, y=Dif, group=LKL_Comp, colour=LKL_Comp))+
  geom_line(aes(colour=LKL_Comp),size=1)+
  geom_point(aes(colour=LKL_Comp),size=2)+ 
  xlab("Linf")+ ylab("LKL-min(LKL)")+ggtitle("Linf LKL profile")+
  theme_fun()

SavePlot('PROFILE_Linf_MIN',15,10)

ggplot(yy, aes(x=Value, y=Dif, group=LKL_Comp, colour=LKL_Comp))+
  geom_line(aes(colour=LKL_Comp),size=1)+
  geom_point(aes(colour=LKL_Comp),size=2)+ 
  xlab("Linf")+ ylab("LKL-min(LKL)")+ggtitle("Linf LKL profile")+
  theme_fun()+coord_cartesian(ylim = c(0, 50))

SavePlot('PROFILE_Linf_zoom_min',15,10)


#### SIGMAR ####
scs <- "R2_min/sigmaR"  
scs_wd <-paste0("models/retrospectives_ref_models/models/PROFILES/",scs)
#dir_prof <- scs_wd
df <- NULL
for(i in c(1:13)){
  load(file.path(scs_wd,paste0("sigmaR_profile",i,".RData")))
  df <- rbind(df,profile)
}


df_long <- df[-1,] %>% pivot_longer(cols=names(df)[c(3:4,6:10)],names_to="LKL_Comp",values_to= "LKL")
head(df_long)

ggplot(df_long[df_long$LKL_Comp=="Survey",],aes(x=Value, y=LKL, group=LKL_Comp, colour=LKL_Comp))+
  geom_point(aes(colour=LKL_Comp))+ geom_line(aes(colour=LKL_Comp))+
  xlab("sigmaR")+ ylab("LKL")+ggtitle("sigmaR LKL profile")+
  theme_fun()

xx <- aggregate(LKL~LKL_Comp, df_long,min,drop=FALSE)
names(xx)[2] <- "min_LKL"
library(dplyr)
yy <- df_long %>% 
  left_join(xx %>% select(LKL_Comp,min_LKL))

head(yy[,c(1,12:14)])
yy$Dif <- yy$LKL-yy$min_LKL
ggplot(yy, aes(x=Value, y=Dif, group=LKL_Comp, colour=LKL_Comp))+
  geom_line(aes(colour=LKL_Comp),size=1)+
  geom_point(aes(colour=LKL_Comp),size=2)+ 
  xlab("sigmaR")+ ylab("LKL-min(LKL)")+ggtitle("Linf LKL profile")+
  theme_fun()

SavePlot('PROFILE_sigmaR_min',15,10)

ggplot(yy, aes(x=Value, y=Dif, group=LKL_Comp, colour=LKL_Comp))+
  geom_line(aes(colour=LKL_Comp),size=1)+
  geom_point(aes(colour=LKL_Comp),size=2)+ 
  xlab("Linf")+ ylab("LKL-min(LKL)")+ggtitle("Linf LKL profile")+
  theme_fun()+coord_cartesian(ylim = c(0, 50))

SavePlot('PROFILE_Linf_zoom_2',15,10)
#.............................................................
#### OTHER WAY  ####

dirname <- "h_steep"
repfiles <- paste0(file.path(scs_wd,dirname),"/prof-",c(0:9))
prof.R0.models <- SSgetoutput(dirvec=repfiles[-1], keyvec=1:length(repfiles[-1]), getcovar = FALSE) # 

#check convergence
a <- unlist(lapply(prof.R0.models,function(x) x$maximum_gradient_component)) #an error might indicate some of the runs didnt complete
barplot(a,ylab='gradient');abline(h=0.0001,col=2) 
png(file.path(plotdir,'hsteep_profile_convergence.png'),6,4,'in',res=300)
barplot(a,ylab='gradient');abline(h=0.0001,col=2) 
dev.off()

#i <- which(a<0.00015)
i <- which(a<0.001)
#i <- i[-c(1,2,3)]
# summarize output
prof.R0.summary <- SSsummarize(prof.R0.models)#SSsummarize(prof.R0.models[i])



# Likelihood components 
mainlike_components         <- c('TOTAL',"Survey", "Age_comp", 'Length_comp','Recruitment') 
mainlike_components_labels  <- c('Total likelihood','Index likelihood',"Age_comp",'Length likelihood','Recruitment likelihood') 


# plot profile using summary created above
#png(file.path(plotdir,"h_steep_profile_plot.png"),width=7,height=4.5,res=300,units='in')
par(mar=c(5,4,1,1))
SSplotProfile(prof.R0.summary,           # summary object
              profile.string = "SR_BH",     # substring of profile parameter
              profile.label=expression(h_steep),minfraction = 0.001,
              pheight=4.5, 
              print=FALSE, 
              plotdir=plotdir, 
              components = mainlike_components, 
              component.labels = mainlike_components_labels,
              add_cutoff = TRUE,
              cutoff_prob = 0.95)
Baseval <- round(Base$parameters$Value[grep("SR_BH",Base$parameters$Label)],2)
abline(v = Baseval, lty=2)
#dev.off()

# make timeseries plots comparing models in profile

R0.vec <- subset(prof.R0.summary$pars,Label=="SR_BH_steep")
#i <- which(a<0.0001)
nR0 <- length(i)
labs <- paste("h_steep = ",R0.vec)[1:nR0]
#labs[which(round(R0.vec,2)==Baseval)] <- paste("SR_Ln(R0) = ",Baseval,"(Base model)")

windows()
SSplotComparisons(prof.R0.summary,legendlabels=labs,
                  pheight=4.5,png=TRUE,plotdir=plotdir,legendloc='bottomleft')
dev.off()

###Piner plot
#png(file.path(plotdir,"h_steep_profile_plot_Length_like.png"),width=7,height=4.5,res=300,units='in')
par(mar=c(5,4,1,1))
PinerPlot(prof.R0.summary, 
          profile.string = "SR_BH_steep", 
          component = "Length_like",
          main = "Changes in length-composition likelihoods by fleet",
          add_cutoff = TRUE,
          cutoff_prob = 0.95,profile.label="SR_BH_steep")
Baseval <- round(Base$parameters$Value[grep("SR_BH_steep",Base$parameters$Label)],2)
abline(v = Baseval, lty=2)
#dev.off()
###Piner plot
#png(file.path(plotdir,"h_steep_profile_plot_Length_like_zoom.png"),width=7,height=4.5,res=300,units='in')
par(mar=c(5,4,1,1))
PinerPlot(prof.R0.summary, 
          profile.string = "SR_BH_steep", 
          component = "Length_like",
          main = "Changes in length-composition likelihoods by fleet",
          add_cutoff = TRUE,
          cutoff_prob = 0.95,ymax=5,profile.label="SR_BH_steep")
Baseval <- round(Base$parameters$Value[grep("SR_BH_steep",Base$parameters$Label)],2)
abline(v = Baseval, lty=2)
#dev.off()

#png(file.path(plotdir,"h_steep_profile_plot_Survey_like.png"),width=7,height=4.5,res=300,units='in')
par(mar=c(5,4,1,1))
PinerPlot(prof.R0.summary, profile.string = "SR_BH_steep", component = "Surv_like",main = "Changes in Index likelihoods by fleet",
          add_cutoff = TRUE,
          cutoff_prob = 0.95, legendloc="topleft",profile.label=expression(h_steep))
Baseval <- round(Base$parameters$Value[grep("SR_BH_steep",Base$parameters$Label)],2)
abline(v = Baseval, lty=2)
#dev.off()

#png(file.path(plotdir,"h_steep_profile_plot_Catch_like.png"),width=7,height=4.5,res=300,units='in')
par(mar=c(5,4,1,1))
PinerPlot(prof.R0.summary, profile.string = "SR_BH_steep", component = "Catch_like",main = "Changes in catch likelihoods by fleet",minfraction=0.0000001)
Baseval <- round(Base$parameters$Value[grep("SR_BH_steep",Base$parameters$Label)],2)
abline(v = Baseval, lty=2)
#dev.off()

# png(file.path(plotdir,"h_steep_profile_plot_Catch_like_zoom.png"),width=7,height=4.5,res=300,units='in')
# par(mar=c(5,4,1,1))
# PinerPlot(prof.R0.summary, profile.string = "SR_BH_steep", component = "Catch_like",
#           main = "Changes in catch likelihoods by fleet",minfraction=0.0000001,ymax=0.02)
# Baseval <- round(Base$parameters$Value[grep("SR_BH_steep",Base$parameters$Label)],2)
# abline(v = Baseval, lty=2)
# dev.off()


#Estimated parameters across runs
pars=prof.R0.summary$pars
n=prof.R0.summary$n
library(tidyr)
a <- pars %>% pivot_longer(1:n,names_to='run')
a <- subset(a,is.na(Yr))
g <- ggplot(subset(a,is.na(Yr)),aes(run,value)) + geom_point() + facet_wrap(~Label,scales='free_y') +
  theme(strip.text=element_text(size=5)) + expand_limits(y=0)
g
#ggsave(file.path(plotdir,'h_steepProfilePar.png'),g,width=6,height=6,units='in',scale=2)

