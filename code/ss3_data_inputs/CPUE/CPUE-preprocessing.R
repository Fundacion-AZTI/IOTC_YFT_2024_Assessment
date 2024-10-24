rm(list = ls())

# Sharepoint path:
source('sharepoint_path.R')

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))

# CPUE types:
cpue_type = c('qt', 'yr')

# -------------------------------------------------------------------------
# Quarterly index: used aggregated data for standardization
# Yearly index: used subsampled operational data

for(k in seq_along(cpue_type)) {

  # Read raw data in:
  if(cpue_type[k] == 'qt') {
    dat <- read_xlsx(file.path(shrpoint_path, 'data/raw/indices/LL/Joint_YFT_CPUE_2024.xlsx'), sheet="4 area QT", skip=1)
    # Organize data by region:
    dat_R1 <- dat[,1:10]
    dat_R2 <- dat[,12:21]
    dat_R3 <- dat[,23:32]
    dat_R4 <- dat[,34:43]
  }
  if(cpue_type[k] == 'yr') {
    dat <- read_xlsx(file.path(shrpoint_path, 'data/raw/indices/LL/Joint_YFT_CPUE_2024.xlsx'), sheet="4 area YR", skip=1)
    # Organize data by region:
    dat_R1 <- dat[,1:7]
    dat_R2 <- dat[,9:17]
    dat_R3 <- dat[,19:25]
    dat_R4 <- dat[,27:33]
  }
  
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
  
  # Save this object for analyses:
  if(cpue_type[k] == 'qt') write.csv(Data79, file = file.path(shrpoint_path, 'data/processed', 'cpue_unscaled-qt.csv'), row.names = FALSE)
  if(cpue_type[k] == 'yr') write.csv(Data79, file = file.path(shrpoint_path, 'data/processed', 'cpue_unscaled-yr.csv'), row.names = FALSE)

}