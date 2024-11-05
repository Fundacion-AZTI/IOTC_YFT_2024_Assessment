

library(r4ss)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggplot2)
library(here)

proj_dir = here::here()
setwd(proj_dir)
source(here('code', 'auxiliary_functions.R'))
# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)

dir_plot <- "output/figures/LLdividedModel"
dir_table <- "output/tables/"

spat_config = '4A_io'
fish_names = get_fisheries(spat_config)$fleet_name
fish_names = gsub(pattern = ' ', replacement = '_', x = fish_names)
fleetnames <- paste0(1:25,"_",c(fish_names,c(fish_names[c(7,10,11,13)])))


scs <- "16_LLsplit_LL1b_LL4_DN_min"
scs_wd <-paste0("models/update/",scs)

  sc_ss3 <- SS_output(dir=scs_wd,  repfile = "Report.sso",covar=T)

  
  #................................................................
  #### PLOT RECRUITMENT DEVIATES  ####
  

  df <- sc_ss3$recruit
  df$RecDev <- "MainRecDev"
  df$RecDev[df$Yr>300] <- "LateRecDev"

  sub_df_long  <- df %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))%>% subset(Yr<309)
  
  scBlack <- "MainRecDev"
  scBlue <- "LateRecDev"
  ggplot(sub_df_long,aes(x=yrqtr,y=dev,group =RecDev)) +
    geom_line(aes(color=RecDev), data = . %>% subset(., RecDev %in% scBlack),lty=3,size=0.6) +
    geom_point(aes(color=RecDev), data = . %>% subset(., RecDev %in% scBlack),size=1.3) +
    geom_line(aes(color=RecDev), data = . %>% subset(., RecDev %in% scBlue),lty=3) +
    geom_point(aes(color=RecDev), data = . %>% subset(., RecDev %in% scBlue),size=1.3) +
    geom_hline(yintercept=0,lty=2)+
    scale_color_manual(values = c('red', 'black')) +xlim(1972,2024)+
    xlab("Year")+ ylab("Log(RecDev)")+
    theme_fun()
  SavePlot("RecDev_RefModel_FINAL",10,5)
  
  #### ABSOLUTE VALUES OF RECRUITMENT ####
  
  
  df <- sc_ss3$recruit
  df$RecDev <- "MainRecDev"
  df$RecDev[df$Yr>300] <- "LateRecDev"
  sub_df_long  <- df %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))%>% subset(Yr<309)
  
  scBlack <- "MainRecDev"
  scBlue <- "LateRecDev"

  ggplot(sub_df_long,aes(x=yrqtr,y=pred_recr)) +
    geom_line(lty=3,size=0.6,color="blue") +
    geom_point(size=1.3,color="blue") +
    xlim(1950,2024)+
    xlab("Year")+ ylab("Age-0 recruits (1.000s)")+
    theme_fun()
  SavePlot("Recruitment_FINAL",10,5)
  
  
 #  #### PLOT RECRUITMENT DEVIATES between areas ####
 #  
 #  sub_scs <- scs[1]
 #  sub_scs_wd <-paste0("models/update/",sub_scs)
 # 
 #  debug(SSplotTimeseries)
 # 
 #  SSplotTimeseries(sc_ss3,subplot=13)
 #  debug(biofunc)
 #  ### Comparison f
 #  df <- sc_ss3$RecrDistpars
 #  df$RecDev <- "MainRecDev"
 #  df$RecDev[df$Yr>300] <- "LateRecDev"
 #  
 #  sub_df_long  <- df %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
 #    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))%>% subset(Yr<309)
 #  
 #  ggplot(sub_df_long,aes(x=yrqtr,y=dev,group =RecDev,colour = RecDev))+geom_point( size = 2)+
 #    geom_line( )+xlim(1972,2025)+
 #    geom_hline(yintercept=0,lty=2)+
 #    scale_color_discrete(labels=levels(sub_df_long$RecDev))+
 #    xlab("Year")+ ylab("Log(RecDev)")+theme_fun()
 #  
 #  
 #  #....................................................
 #  
 #  #### PLOT RECRUITMENT DEVIATES between areas ####
 #  
 # # load(list=ls(),file="RecDev_byArea.RData")
 #  
 #  
 #  load("output/figures/LLdividedModel/RecDev_byArea.RData")
 #  
 #  yrvalsOriginal <- yrvals
 #  yrvals <- qtr2yearqtr(yrvals,1950,13)
 #  ts0 <- ts
 #  ts[["YrSeas"]]<- qtr2yearqtr(ts0[["YrSeas"]],1950,13)
 #  plot(yrvals, yvals[plot1 | plot2 | plot3], type = "n", 
 #       xlab = xlab, ylim = c(0, 1.05 * ymax), yaxs = "i", 
 #       ylab = ylab, main = ifelse(mainTitle, main, ""), 
 #       cex.main = cex.main)
 #  
 #  
 #  iseas<-1
 #  s <- birthseas[iseas]
 #  mycol <- seascols[iseas]
 #  mytype <- "o"
 #  points(ts[["YrSeas"]][plot1], yvals[plot1], 
 #         pch = 19, col = mycol)
 #  lines(ts[["YrSeas"]][plot2], yvals[plot2], type = mytype, 
 #        col = mycol)
 #  points(ts[["YrSeas"]][plot3], yvals[plot3], 
 #         pch = 19, col = mycol)
 #  
 #  load(file="output/figures/LLdividedModel/RecDev_byArea_2.RData")
 #  ts1 <- ts
 #  ts[["YrSeas"]]<- qtr2yearqtr(ts1[["YrSeas"]],1950,13)
 #  
 #  points(ts[["YrSeas"]][plot1], yvals[plot1], 
 #         pch = 19, col = mycol)
 #  lines(ts[["YrSeas"]][plot2], yvals[plot2], 
 #        type = mytype, col = mycol)
 #  points(ts[["YrSeas"]][plot3], yvals[plot3], 
 #         pch = 19, col = mycol)
 #  
 #  
 #  if (nareas > 1 & subplot %in% c(2, 3, 5, 6, 8, 10, 12, 
 #                                  13)) {
 #    legend("topright", legend = areanames[areas[c(1,4)]], lty = 1, 
 #           pch = 1, col = areacols[areas[c(1,4)]], bty = "n")
 #  }
 #  
 #  SavePlot("RecDev_by_Area",10,5)
  
  
  #### PFISHING MORTALITY BY REGION ####
  #...............................................

  data<- sc_ss3$timeseries
  
  ### Comparison f
  df <- data[,c(1,2,21)]
  df$Fleet <- sc_ss3$FleetNames[1]
  names(df)[3] <- "F"
  for(i in 2: 24){
    newFleet <- data[,c(1,2,21+8*(i-1))]
    newFleet$Fleet <- sc_ss3$FleetNames[i]
    names(newFleet)[3] <- "F"
    df <- rbind(df,newFleet )
    }
df$Yrss <- qtr2yearqtr(df$Yr,1950,13) # sub_df_long  <- df %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
 Fmsy <- sc_ss3$derived_quants$Value[sc_ss3$derived_quants$Label=="annF_MSY"]
 #  mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))%>% subset(Yr<309)
 sc_ss3$derived_quants$Label[c(1000:1200)]
 df$yrssInt <- floor(df$Yrss)
 dfnew <- aggregate(F~ yrssInt+Fleet+Area,data=df,sum)
 sub_df1 <- dfnew[dfnew$Area==1 & dfnew$Fleet %in% sc_ss3$FleetNames[c(1:9,22)],]
 sub_df2 <- dfnew[dfnew$Area==2 & dfnew$Fleet %in% sc_ss3$FleetNames[c(10,16,18,23)],]
 sub_df3<- dfnew[dfnew$Area==3 & dfnew$Fleet %in% sc_ss3$FleetNames[11],]
 sub_df4 <- dfnew[dfnew$Area==4 & dfnew$Fleet %in% sc_ss3$FleetNames[c(12,13,14,15,19,20,21,24)],]
 
  p1 <- ggplot(sub_df1,aes(yrssInt,y=F*0.04,group =Fleet,colour = Fleet))+
    geom_line(size=1.2 )+xlim(1950,2025)+facet_wrap(~ Area)+
    xlab("Year")+ ylab("F")+theme_fun()  +ggtitle("Region 1") +
    theme(legend.position = c(0.25, 0.99),
          legend.justification = c("right", "top"), 
      legend.text = element_text(size=8),
      legend.title = element_text(size=8),
      legend.key.width= unit(0.5, 'cm'),
      legend.key.size = unit(0.2, 'cm'))


  p2 <- ggplot(sub_df2,aes(yrssInt,y=F*0.04,group =Fleet,colour = Fleet))+
    geom_line(size=1.2 )+xlim(1950,2025)+facet_wrap(~ Area)+
    xlab("Year")+ ylab("F")+theme_fun()  +ggtitle("Region 2") +
    theme(legend.position = c(0.2, 0.99),
          legend.justification = c("right", "top"), 
          legend.text = element_text(size=8),
          legend.title = element_text(size=8),
          legend.key.width= unit(0.5, 'cm'),
          legend.key.size = unit(0.2, 'cm'))
  p3 <- ggplot(sub_df3,aes(yrssInt,y=F*0.04,group =Fleet,colour = Fleet))+
    geom_line(size=1.2 )+xlim(1950,2025)+facet_wrap(~ Area)+
    xlab("Year")+ ylab("F")+theme_fun()  +ggtitle("Region 3") +
    theme(legend.position = c(0.2, 0.99),
          legend.justification = c("right", "top"), 
          legend.text = element_text(size=8),
          legend.title = element_text(size=8),
          legend.key.width= unit(0.5, 'cm'),
          legend.key.size = unit(0.2, 'cm'))
  p4 <- ggplot(sub_df4,aes(yrssInt,y=F*0.04,group =Fleet,colour = Fleet))+
    geom_line(size=1.2 )+xlim(1950,2025)+facet_wrap(~ Area)+
    xlab("Year")+ ylab("F")+theme_fun()  +ggtitle("Region 4") +
    theme(legend.position = c(0.2, 0.99),
          legend.justification = c("right", "top"), 
          legend.text = element_text(size=8),
          legend.title = element_text(size=8),
          legend.key.width= unit(0.5, 'cm'),
          legend.key.size = unit(0.2, 'cm'))

  grid.arrange(p1,p4,p2,p3,ncol=2)
  SavePlot("F_Area",15,10)
  
  
  
  #### fleet selectivtiy ####
  #...............................................
  
  head(sc_ss3$ageselex)
  data <- sc_ss3$ageselex[sc_ss3$ageselex$Yr==308 & sc_ss3$ageselex$Factor=="Asel" & sc_ss3$ageselex$Fleet<=24,]
  df <- NULL
  ageFleet <- c(1:24)[-c(6,8,16,17,19,20)]
  sizeFleet <- c(6,8,16,17,19,20)
  for(i in ageFleet){
    sel.f <- data[data$Fleet==i,-c(1:7)]
    dfnew <- data.frame(Age=0:40,Selectivity=as.vector(t(sel.f)),Fleet=sc_ss3$FleetNames[i])
    df <-rbind(df,dfnew)
  }
  
  p1 <- ggplot(df,aes(x=Age,y=Selectivity,group =Fleet))+geom_point( size = 2)+
    geom_line( )+ facet_wrap(~Fleet)+
    #   scale_color_discrete(labels=levels(sub_df_long$RecDev))+
    xlab("Year")+ ylab("Selectivity")+theme_fun()
  
  SavePlot("Selectivity_by_Age",15,10)
  
  
  
  data <- sc_ss3$sizeselex[sc_ss3$sizeselex$Yr==308 & sc_ss3$sizeselex$Factor=="Dead" & sc_ss3$sizeselex$Fleet<=24,]
  df2 <- NULL
  sizeFleet <- c(6,8,16,17,19,20)
  for(i in sizeFleet){
    sel.f <- data[data$Fleet==i,-c(1:5)]
    dfnew <- data.frame(Size=seq(12,200,4),Selectivity=as.vector(t(sel.f)),Fleet=sc_ss3$FleetNames[i])
    df2 <-rbind(df2,dfnew)
  }
  
  p2 <- ggplot(df2,aes(x=Size,y=Selectivity,group =Fleet))+geom_point( size = 2)+
    geom_line( )+ facet_wrap(~Fleet)+
    #   scale_color_discrete(labels=levels(sub_df_long$RecDev))+
    xlab("Year")+ ylab("Selectivity")+theme_fun()
  p2
  SavePlot("Selectivity_by_Size",15,10)
  
  ggplot(df2,aes(x=Size,y=Selectivity,group =Fleet))+geom_point( size = 2)+
    geom_line( )+ facet_wrap(~Fleet)+ facet_wrap(~Fleet)
    #   scale_color_discrete(labels=levels(sub_df_long$RecDev))+
    xlab("Year")+ ylab("Selectivity")+theme_fun()
  arrangeGrob(p1,p2)
  library(ggpubr)
  ggarrange(p1,p2,
            labels = c("A", "B"),
            ncol = 2, nrow = 2)
  
  
  
  #### F at age ####
  #...............................................
  
  df <- sc_ss3$fatage[sc_ss3$fatage$Yr>300 & sc_ss3$fatage$Yr<309,]
  Fmsy <- sc_ss3$derived_quants$Value[sc_ss3$derived_quants$Label=="annF_MSY"]
  aux <- NULL
  for(i in 1:41){
    subdf <- df[,c(1,2,5,8+(i-1))]
    subdf$Age <- i-1
    names(subdf)[4] <- "F_at_age"
    aux <- rbind(aux, subdf)
  }
  data <- aux
  
  dfnew <- aggregate(F_at_age*Fmsy~ Yr+Area+Age,data=data,sum)#sum fleets
  dfnew$Yrss <- qtr2yearqtr(dfnew$Yr,1950,13) # sub_df_long  <- df %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
  dfnew$yrssInt <- floor(dfnew$Yrss)
  names(dfnew)[4] <- "F_at_age"
  dfnewArea <- aggregate(F_at_age~ Area+Age,data=dfnew,mean) #mean years
  ggplot(dfnewArea,aes(x=Age,y=F_at_age,group =Area))+geom_point( size = 2)+
    geom_line( )+ facet_wrap(~Area)+
  #   scale_color_discrete(labels=levels(sub_df_long$RecDev))+
  xlab("Age")+ ylab("F_at_age")+theme_fun()
  
  SavePlot("F_at_Age_BYarea",10,5)
  
  #NOT CORRECT F by area can not be aggregated
  # dfnewAll<- aggregate(F_at_age~ Age,data= dfnewArea,sum)
  # ggplot( dfnewAll,aes(x=Age,y=F_at_age))+geom_point( size = 2)+
  #   geom_line( )+
  #   #   scale_color_discrete(labels=levels(sub_df_long$RecDev))+
  #   xlab("Age")+ ylab("F_at_age")+theme_fun()
  # SavePlot("F_at_Age",15,10)
  
  
  #### CPUE OBSERVED VS PREDICTED ####
  
  
  df <- sc_ss3$cpue
head(df)  
df$Yrss <- qtr2yearqtr(df$Yr,1950,13) 
unique(df$Yr)
ggplot(df,aes(Yrss,y=Obs,group =Fleet))+geom_point()+facet_wrap(~ factor(Fleet_name,
              c("25_CPUE_LL_1b", "28_CPUE_LL_4", "26_CPUE_LL_2","27_CPUE_LL_3")),scales="free")+
  geom_line(aes(y=Obs, colour="Obs") )+
  geom_line(aes(y=Exp, colour="Exp") ,size=1)+
  xlab("Year")+ ylab("Index")+theme_fun()  +
  scale_color_manual(values = c( 'red','black')) 
SavePlot("CPUE_OBS_EXP",15,10)


#### residuals OBSERVED VS PREDICTED ####


df <- sc_ss3$cpue
head(df)  
df$Yrss <- qtr2yearqtr(df$Yr,1950,13) 
unique(df$Yr)
ggplot(df,aes(Yrss,y=Obs-Exp,group =Fleet))+geom_point()+facet_wrap(~ factor(Fleet_name,
    c("25_CPUE_LL_1b", "28_CPUE_LL_4", "26_CPUE_LL_2","27_CPUE_LL_3")),scales="free")+
  geom_hline(yintercept=0,lty=2)+
  xlab("Year")+ ylab("Residuals: Obs-Exp")+theme_fun()  +
  scale_color_manual(values = c( 'red','black')) 
SavePlot("CPUE_Residuals",15,10)

##### LENGTH COMPOSITION DATA ####


df <- sc_ss3$lendbase
head(df)  
df$Yrss <- qtr2yearqtr(df$Yr,1950,13) 
unique(df$Yr)
ggplot(df,aes(Yrss,y=Obs,group =Fleet))+geom_point()+facet_wrap(~ Fleet,scales="free")+
    geom_point(aes(y=Exp, color="red",pch=3) ,size=1)+
  xlab("Year")+ ylab("Average length(cm")+theme_fun() 
SavePlot("CPUE_Residuals",15,10)


##### Mean length DATA ####
out <- sc_ss3
out$lendbase$Yr <-  qtr2yearqtr(out$lendbase$Yr,1950,13) 
SSMethod.TA1.8(out, "len", fleet = c(1:24),maxpanel = 24,plotadj = FALSE)
SavePlot("MeanLength_And_predictions",15,10)


##### Mean length DATA one blcok####

scs <- "15_BiasCorrectionRamp_hess"
scs_wd <-paste0("models/update/",scs)

sc_ss3_one <- SS_output(dir=scs_wd,  repfile = "Report.sso",covar=T)


out <- sc_ss3_one
out$lendbase$Yr <-  qtr2yearqtr(out$lendbase$Yr,1950,13) 
SSMethod.TA1.8(out, "len", fleet = c(7,10,11,13),maxpanel = 4,plotadj = FALSE)
SavePlot("MeanLength_And_predictions_OneBlock",15,10)


##### bubble plot #### not manage

out <- sc_ss3
out$lendbase$Yr <-  qtr2yearqtr(out$lendbase$Yr,1950,13) 
p1 <- SSplotComps(out,subplots=24, fleets=c(7,10,11,13,22,23,24),cols=2)
SavePlot("MeanLength_And_predictions",15,10)
grid.arrange(p1)



##### tag #### not manage

out <- sc_ss3
out$tagdbase2$Yr<-  qtr2yearqtr(out$tagdbase2$Yr,1950,13) 
out$tagdbase2$Yr.S<-  qtr2yearqtr(out$tagdbase2$Yr.S,1950,13) 
out$tagdbase1$Yr<-  qtr2yearqtr(out$tagdbase1$Yr,1950,13) 
out$tagdbase1$Yr.S<-  qtr2yearqtr(out$tagdbase1$Yr.S,1950,13) 
debug(SSplotTags) #debug tagfun2 function when making the plot
SSplotTags(out,subplots=2)

plot(0, xlim = xlim + c(-0.5, 0.5), ylim = c(0, max(RecAg[["Obs"]], 
                                                    RecAg[["Exp"]]) * 1.05), type = "n", xaxs = "i", yaxs = "i", 
     xlab = labels[1], ylab = labels[2], main = labels[5], 
     cex.main = cex.main)
for (iy in 1:nrow(RecAg)) {
  xx <- c(RecAg[["Yr.S"]][iy] - width, RecAg[["Yr.S"]][iy] - 
            width, RecAg[["Yr.S"]][iy] + width, RecAg[["Yr.S"]][iy] + 
            width)
  yy <- c(0, RecAg[["Obs"]][iy], RecAg[["Obs"]][iy], 0)
  polygon(xx, yy, col = col4)
}

ggplot(RecAg, aes(x=Yr.S, y=Obs)) + 
  geom_bar(stat = "identity", color="black", fill="grey" )+
  geom_line(aes(RecAg[["Yr.S"]], RecAg[["Exp"]]),lwd=1.5)+theme_bw()+xlab("Year")+ylab("Frequency")+
  ggtitle("Post-latency tag recaptures aggregated across tag groups")

SavePlot("Tag_recovered",15,10)

#### nubmer of tags recapture by fleet####
out <- sc_ss3
SSplotTags(out,subplots=4)
SavePlot("Tag_Recapture_ByFleet",15,10)


#### recruitment deviates####
out <- sc_ss3
out$recruit$Yr<-  qtr2yearqtr(out$recruit$Yr,1950,13) 
debug(SSplotRecdevs)
SSplotRecdevs(out,subplots=1)
alldevs <- rbind(recdevEarly, recdev, recdevFore)[goodyrs, 
]
colvec <- c(rep(col2, nrow(recdevEarly)), rep(col1, nrow(recdev)), 
            rep(col2, nrow(recdevFore)))[goodyrs]
val <- alldevs[["Value"]]
Yr <- alldevs[["Yr"]]
#if (uncertainty) {
  std <- alldevs[["Parm_StDev"]]
  recdev_hi <- val + 1.96 * std
  recdev_lo <- val - 1.96 * std
  ylim <- range(recdev_hi, recdev_lo, na.rm = TRUE)
#}
else {
  ylim <- range(val, na.rm = TRUE)
}
Yr <- qtr2yearqtr(Yr,1950,13) 
plot(Yr, Yr, type = "n", xlab = labels[1], ylab = labels[3], 
     ylim = ylim)
abline(h = 0, col = "grey")
if (uncertainty) 
  arrows(Yr, recdev_lo, Yr, recdev_hi, length = 0.03, code = 3, 
         angle = 90, lwd = 1.2, col = colvec)
lines(Yr, val, lty = 3)
points(Yr, val, pch = 16, col = colvec)
}
SavePlot("RecDev_unceratainty",15,10)


#### Biomas by area ####
out$timeseries$Yr <- qtr2yearqtr(out$timeseries$Yr ,1950,13) 
undebug(SSplotTimeseries)
SSplotTimeseries(out,subplot=8)
SavePlot("SSB_by_area",15,10)
