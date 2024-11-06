#
# Create forecast catch data
#
#...........................................................

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



spat_config = '4A_io'
# SS base files path (in Sharepoint):
SS_base = 'models/RefModels/3_SplitCPUE_tag1_EC0_h08/nohess_win'

# SS configuration path (in Sharepoint):
SS_config = 'models/update'

# SS input data path (in Sharepoint):
SS_data = 'data/ss3_inputs/4A_io'


####


source("models/forecast/helper.r")
ssdat <- SS_readdat_3.30(file.path(SS_base,"data.ss"))
summary(ssdat$catch)
data <- ssdat$catch[-1,]

helper <- read.csv("models/forecast/helper.csv")
names(helper) <- c("ssyr","yrc","year","ss")
summary(helper)

mergeData <- merge(data,helper,by.x="year" ,by.y="ssyr")
mergeData

work = mergeData	%>% 
  group_by(year.y,ss,fleet) %>% 
  summarise(Catch = sum(catch)) %>% 
  dplyr::filter(year.y==2023) %>%
  dplyr::select(year.y,ss,fleet,Catch)


dat = work[order(paste(work$fleet,work$year.y,work$ss)),]
#fleets with catch 0 missing


fl <- 1:24
ind <- sort(dat$fleet[dat$ss==1])  #7,10,13,19
fl.ss1 <- fl[-ind]
fl.ss1
nss1 <- length(fl.ss1)
df.ss1 <- matrix(c(rep(2023,nss1),rep(1,nss1),fl.ss1,rep(0,nss1)),byrow=F,nrow=nss1)

ind <- sort(dat$fleet[dat$ss==2])  #7,10,13
fl.ss2 <- fl[-ind]
nss2 <- length(fl.ss2)
df.ss2 <- matrix(c(rep(2023,nss2),rep(2,nss2),fl.ss2,rep(0,nss2)),byrow=F,nrow=nss2)

ind <- sort(dat$fleet[dat$ss==3])  #3,7,10,13,16,19
fl.ss3 <- fl[-ind]
nss3 <- length(fl.ss3)
df.ss3 <- matrix(c(rep(2023,nss3),rep(3,nss3),fl.ss3,rep(0,nss3)),byrow=F,nrow=nss3)

ind <- sort(dat$fleet[dat$ss==4])  #7,10,13,19
fl.ss4 <- fl[-ind]
nss4 <- length(fl.ss4)
df.ss4 <- matrix(c(rep(2023,nss4),rep(4,nss4),fl.ss4,rep(0,nss4)),byrow=F,nrow=nss4)


catch_0<- as.data.frame(rbind(df.ss1,df.ss2,df.ss3,df.ss4))
names(catch_0)<- names(dat)


datAll <- rbind(dat,catch_0)
dat = datAll[order(paste(datAll$fleet,datAll$year.y,dat$ss)),]
dat = rbind(transform(dat,year.y = 2024),transform(dat,year.y = 2025),
            transform(dat,year.y = 2026),transform(dat,year.y = 2027),
            transform(dat,year.y = 2028),transform(dat,year.y = 2029),
            transform(dat,year.y = 2030),transform(dat,year.y = 2031),
            transform(dat,year.y = 2032),transform(dat,year.y = 2033),
            transform(dat,year.y = 2034))
dat = dat %>% 
  mutate(qtr =yearqtr2qtr(year.y,ss,1950,13),season=1) %>%
  dplyr::select(qtr,season,fleet,Catch)  %>%
  arrange(fleet,qtr)

Tot_catch_2023 <- sum(datAll$Catch)
proj.vec <- seq(0.6,1.2,0.05)

dat_0.6 = transform(dat, Catch = Catch*0.6)
dat_0.65 = transform(dat, Catch = Catch*0.65)
dat_0.7 = transform(dat, Catch = Catch*0.7)
dat_0.75 = transform(dat, Catch = Catch*0.75)
dat_0.8 = transform(dat, Catch = Catch*0.8)
dat_0.85 = transform(dat, Catch = Catch*0.85)
dat_0.9 = transform(dat, Catch = Catch*0.9)
dat_0.95 = transform(dat, Catch = Catch*0.95)
dat_1 = transform(dat, Catch = Catch*1)
dat_1.05 = transform(dat, Catch = Catch*1.05)
dat_1.1 = transform(dat, Catch = Catch*1.1)
dat_1.15 = transform(dat, Catch = Catch*1.15)
dat_1.2 = transform(dat, Catch = Catch*1.2)




#CHECK THAT IT'S OK
files <- c(paste0("dat_",seq(0.6,1.2,0.05)))

#check total catch
for(i in 1:length(files)){
  ForeCatch <- get(files[i])
  print(proj.vec[i]-sum(ForeCatch$Catch[ForeCatch$qtr %in% c(309,310,311,312)])/Tot_catch_2023)
}


#check catch by fleet
for(proj in 1:length(proj.vec)){
  for(fl in 1:24){
    for(yr in c(309:312)){
    ForeCatch <- get(files[proj])
    print(proj.vec[proj]-ForeCatch$Catch[ForeCatch$qtr==yr & ForeCatch$fleet==fl]/
            ssdat$catch$catch[ssdat$catch$year==yr-4 & ssdat$catch$fleet==fl])
  }}}



#check figure
base_dat <- ssdat
names(base_dat$catch)[2] <- "season"
names(base_dat$catch)[1] <- "qtr"

dfFore <- dat_0.6
p1<-ggplot()+geom_line(data=dfFore[dfFore$qtr<313,],aes(x=fleet,y=Catch))+facet_wrap(~qtr)
p1
df <- base_dat$catch[base_dat$catch$qtr>=305 & base_dat$catch$qtr<=308,][-1,]
  names(catch_0)[1:4] <- names(df)[1:4]
  catch_0 = catch_0 %>% 
  mutate(qtr =yearqtr2qtr(qtr,season,1950,13),season=1) 

df <- rbind(df[,1:4],catch_0)

df$qtr[df$qtr==305]<- 309
df$qtr[df$qtr==306]<- 310
df$qtr[df$qtr==307]<- 311
df$qtr[df$qtr==308]<- 312

dfFore <- dat_1
p1+geom_line(data=df,aes(x=fleet,y=catch))+facet_wrap(~qtr)
ggplot()+geom_line(data=dfFore[dfFore$qtr<313,],aes(x=fleet,y=Catch),color="blue")+facet_wrap(~qtr)+
geom_line(data=df,aes(x=fleet,y=catch),color="red")


proj.vec <- seq(0.6,1.2,0.05)
for(i in 1:length(proj.vec)){
  df <- get(paste0("dat_",proj.vec[i]))
  write.table(df,file=file.path("data","projections",paste0("dat_",proj.vec[i],".txt")),row.names=FALSE)
}

fore_base <- SS_readforecast(file.path("projections","Proj_Forecast","forecast_noScale.ss"))

options(max.print=1000000)

for(i in 1:length(proj.vec)){
readcatch <- read.table(file.path("projections","InputCatchProjections",paste0("dat_",proj.vec[i],".txt")),
                       header = TRUE )
fore_1 <- fore_base
summary(readcatch)
# fore_1$basis_for_fcast_catch_tuning
# fore_1$
# fore_1$basis_for_fcast_catch_tuning <- readcatch
fore_1$Nforecastyrs <- 40
SS_writeforecast(fore_1, dir = file.path("projections"),file=paste0("forecast_",proj.vec[i],".ss"), overwrite = T)
}


for( j in 1:length(nms)){
  for(i in 1:length(proj.vec)){
    fore_base <- SS_readforecast(file.path("projections",paste0("forecast_",proj.vec[i],".ss")))
    fore_1 <- fore_base
    tmp_dir <- file.path("projections","FinalGrid",nms[j])
    replist <-SS_plots(tmp_dir,uncertainty=T,png=T,forecastplot=F, fitrange = TRUE, 
                       parrows=5, parcols=4, showdev= TRUE)
    mult=exp(mean(myreplist$recruit$dev[253:300]))
    fore_1$fcast_rec_val <- mult
    
    
    SS_writeforecast(fore_1, dir = file.path("projections",nms[j]),file=paste0("forecast_scaled_",proj.vec[i],".ss"), overwrite = T)
  }}

