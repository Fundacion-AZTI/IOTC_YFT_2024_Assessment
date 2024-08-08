#
#       checking the lencomp and years used in the assessment 2021 and 
#      the data provided for 204
#
#............................................................

library(dplyr)
library(r4ss)
library(here)

proj_dir = here::here()
setwd(proj_dir)

source("code/analysingModels/auxiliary_functions_4analysingModels.R")
source("code/inputs/auxiliary_functions.R")
# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)

# SS base files path (in Sharepoint):
SS_base = 'models/base_win_vs_lin/4A_io_lin_v33022_FixedParam2_Fl11'
base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
SS_base = 'models/update/08_correctionLengthData'
dat_2024 = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))



misYrs <- NULL
fls <- NULL
fl <- length(unique(dat_2024$lencomp$fleet)))
for (i in fl){
x <- base_dat$lencomp  %>% dplyr::filter(fleet==i & year<=296) %>% select(year)
y <-  dat_2024$lencomp  %>% dplyr::filter(fleet==i & year<=296) %>% select(year)
misYr_fl <- setdiff( y,x)
if(length(misYr_fl>0)){
  misYrs <- c(misYrs,misYr_fl)
  fls <- c(fls,rep(i,length(misYr_fl$year)))
}}

LC_diff_AS2024_AS2021<- NULL
LC_diff_AS2024_AS2021$fleet <- fls
LC_diff_AS2024_AS2021$NewYr <-  misYrs
%>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))
write.csv(newLencomp, file="DifferencesLencomp_AS2021_AS2024.csv")
