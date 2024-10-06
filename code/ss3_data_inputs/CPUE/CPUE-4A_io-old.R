rm(list = ls())

# Spatial configuration:
spat_config = '4A_io'

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))

# Read raw data in:
dat <- read_xlsx(file.path(shrpoint_path, 'data/raw/indices/LL/Joint_YFT_CPUE_2024.xlsx'), sheet="4 area QT", skip=1)


# -------------------------------------------------------------------------
# Organize data by region:

dat_R1 <- dat[,1:10]
dat_R2 <- dat[,12:21]
dat_R3 <- dat[,23:32]
dat_R4 <- dat[,34:43]

# work_R1 = dat_R1	%>% dplyr::rename(yq =1, pr=2) %>% select(yq,pr)	%>%
#   mutate(Quarter=(yq-floor(yq))*4+1) %>% 
#   mutate(yr=floor(yq)) %>% 
#   mutate(qtr =yearqtr2qtr(yr,Quarter,1950,13))  
# 
# work_R2 = dat_R2	%>% dplyr::rename(yq =1, pr=2) %>% select(yq,pr)	%>%
#   mutate(Quarter=(yq-floor(yq))*4+1) %>% 
#   mutate(yr=floor(yq)) %>% 
#   mutate(qtr =yearqtr2qtr(yr,Quarter,1950,13))  
# 
# work_R3 = dat_R3	%>% dplyr::rename(yq =1, pr=2) %>% select(yq,pr)	%>%
#   mutate(Quarter=(yq-floor(yq))*4+1) %>% 
#   mutate(yr=floor(yq)) %>% 
#   mutate(qtr =yearqtr2qtr(yr,Quarter,1950,13))  
# 
# work_R4 = dat_R4	%>% dplyr::rename(yq =1, pr=2) %>% select(yq,pr)	%>%
#   mutate(Quarter=(yq-floor(yq))*4+1) %>% 
#   mutate(yr=floor(yq)) %>% 
#   mutate(qtr =yearqtr2qtr(yr,Quarter,1950,13))  
# 
# # Merge all datasets:
# Data79 =  rbind (cbind(subset(work_R1, !is.na(pr),select=c('yq','pr','yr','qtr')),AssessmentAreaName='1b'),
#                  cbind(subset(work_R2, !is.na(pr),select=c('yq','pr','yr','qtr')),AssessmentAreaName='2'),
#                  cbind(subset(work_R3, !is.na(pr),select=c('yq','pr','yr','qtr')),AssessmentAreaName='3'),
#                  cbind(subset(work_R4, !is.na(pr),select=c('yq','pr','yr','qtr')),AssessmentAreaName='4'))
# 
# # Specify scaling factors (see Hoyle and Langley 2020, 10.1016/j.fishres.2020.105586)
# yftwts = list('7994 m8'= c(0.175+0.983+0.516,0.623,0.455,1.000))
# names(yftwts[['7994 m8']])=c('1','2','3','4')
# 
# # Do the scaling by region:
# data79 = Data79
# data79$pr_7994_m8 = data79$pr
# index = data79$AssessmentAreaName=='1b'
# work = data79[index,]
# work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * yftwts[['7994 m8']][1]
# data79[index,]=work
# index = data79$AssessmentAreaName=='2'
# work = data79[index,]
# work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * yftwts[['7994 m8']][2]
# data79[index,]=work
# index = data79$AssessmentAreaName=='3'
# work = data79[index,]
# work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * yftwts[['7994 m8']][3]
# data79[index,]=work
# index = data79$AssessmentAreaName=='4'
# work = data79[index,]
# work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * yftwts[['7994 m8']][4]
# data79[index,]=work
# 
# data <- data79 %>% mutate(season=1,cv=0.2) %>% 
#   mutate(fleet = case_when(AssessmentAreaName=='1b' ~ 22,
#                            
#                            AssessmentAreaName=='2' ~ 23,
#                            AssessmentAreaName=='3' ~ 24,TRUE ~ 25)) %>% 
#   select(qtr,season,fleet,pr_7994_m8,cv)
# 
# write.csv(data,file=file.path('data','ss3_inputs',spat_config,'scaled_cpue.csv' ), row.names = FALSE)
# 


# Standardize CV to a mean of 0.2:

workcv_R1 = dat_R1	%>% dplyr::rename(yq =1, pr=2,stdlower=3) %>% select(yq,pr,stdlower)	%>%
  mutate(Quarter=(yq-floor(yq))*4+1) %>% 
  mutate(yr=floor(yq)) %>% 
  mutate(qtr =yearqtr2qtr(yr,Quarter,1950,13)) %>% 
  mutate(std=(pr-stdlower)/1.96) %>% mutate(stdcv02=std/mean(std,na.rm=T)*0.2)

workcv_R2 = dat_R2	%>% dplyr::rename(yq =1, pr=2,stdlower=3) %>% select(yq,pr,stdlower)	%>%
  mutate(Quarter=(yq-floor(yq))*4+1) %>% 
  mutate(yr=floor(yq)) %>% 
  mutate(qtr =yearqtr2qtr(yr,Quarter,1950,13)) %>% 
  mutate(std=(pr-stdlower)/1.96) %>% mutate(stdcv02=std/mean(std,na.rm=T)*0.2)

workcv_R3 = dat_R3	%>% dplyr::rename(yq =1, pr=2,stdlower=3) %>% select(yq,pr,stdlower)	%>%
  mutate(Quarter=(yq-floor(yq))*4+1) %>% 
  mutate(yr=floor(yq)) %>% 
  mutate(qtr =yearqtr2qtr(yr,Quarter,1950,13)) %>% 
  mutate(std=(pr-stdlower)/1.96) %>% mutate(stdcv02=std/mean(std,na.rm=T)*0.2)

workcv_R4 = dat_R4	%>% dplyr::rename(yq =1, pr=2,stdlower=3) %>% select(yq,pr,stdlower)	%>%
  mutate(Quarter=(yq-floor(yq))*4+1) %>% 
  mutate(yr=floor(yq)) %>% 
  mutate(qtr =yearqtr2qtr(yr,Quarter,1950,13)) %>% 
  mutate(std=(pr-stdlower)/1.96) %>% mutate(stdcv02=std/mean(std,na.rm=T)*0.2)


# Merge:
Data79 =  rbind (cbind(subset(workcv_R1, !is.na(pr),select=c('yq','pr','yr','qtr','stdcv02')), AssessmentAreaName='1b'),
                 cbind(subset(workcv_R2, !is.na(pr),select=c('yq','pr','yr','qtr','stdcv02')), AssessmentAreaName='2'),
                 cbind(subset(workcv_R3, !is.na(pr),select=c('yq','pr','yr','qtr','stdcv02')), AssessmentAreaName='3'),
                 cbind(subset(workcv_R4, !is.na(pr),select=c('yq','pr','yr','qtr','stdcv02')), AssessmentAreaName='4'))

# Specify scaling factors (see Hoyle and Langley 2020, 10.1016/j.fishres.2020.105586)
yftwts = list('7994 m8'= c(0.175+0.983+0.516,0.623,0.455,1.000))
names(yftwts[['7994 m8']])=c('1','2','3','4')

# Perform scaling:
data79 = Data79
data79$pr_7994_m8 = data79$pr
index = data79$AssessmentAreaName=='1b'
work = data79[index,]
work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * yftwts[['7994 m8']][1]
data79[index,]=work
index = data79$AssessmentAreaName=='2'
work = data79[index,]
work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * yftwts[['7994 m8']][2]
data79[index,]=work
index = data79$AssessmentAreaName=='3'
work = data79[index,]
work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * yftwts[['7994 m8']][3]
data79[index,]=work
index = data79$AssessmentAreaName=='4'
work = data79[index,]
work$pr_7994_m8 = work$pr_7994_m8/mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * yftwts[['7994 m8']][4]
data79[index,]=work

# Prepare data for ss3:
data <- data79 %>% mutate(season = 1,cv = stdcv02) %>% 
  mutate(fleet = case_when(AssessmentAreaName=='1b' ~ 22,
                           AssessmentAreaName=='2' ~ 23,
                           AssessmentAreaName=='3' ~ 24,TRUE ~ 25)) %>% 
  select(qtr, season, fleet, pr_7994_m8, cv)

write.csv(data,file=file.path('data','ss3_inputs',spat_config,'scaled_cpue_Meancv_02.csv' ), row.names = FALSE)

