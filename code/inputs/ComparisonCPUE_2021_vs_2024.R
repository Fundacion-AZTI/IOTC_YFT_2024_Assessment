#
#      comparison CPUE wptt2021 and wptt2024
#............................................................



library(r4ss)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

proj_dir = here::here()
setwd(proj_dir)

# Read auxiliary functions:
source(here('code', 'inputs', 'auxiliary_functions.R'))
source(here("code", "analysingModels","auxiliary_functions_4analysingModels.R"))
# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)

dir_plot <- "output/figures/"
dir_table <- "output/tables/"


# SS base files path (in Sharepoint):
SS_base = 'models/update/00_BC'

# SS configuration path (in Sharepoint):
SS_config = 'models/update'

# SS input data path (in Sharepoint):
SS_data = 'data/ss3_inputs/4A_io'

# -------------------------------------------------------------------------

# Read base SS inputs (from 2021 assessment)
base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
cpue_df = read.csv(file.path(shrpoint_path, SS_data, 'scaled_cpue.csv'))

cpue2021 <- base_dat$CPUE %>% mutate(scenario="WPTT2021")
cpue2024 <- cpue_df %>% mutate(scenario="WPTT2024") %>%   
  rename_with(.cols = 1:6,~ names(cpue2021))
cpue <- bind_rows(cpue2021,cpue2024) 
cpue<- cpue %>%  rename_with(.cols = 1,~ "qtr") %>%   mutate(yrqtr=qtr2yearqtr(qtr,1950,13))%>%
  mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))
cpue_new <- cpue
cpue_new$index <- as.factor(as.character(cpue_new$index))
levels(cpue_new$index) <- paste0("scaled cpue R",1:4)
order_levels <- c(levels(cpue_new$index)[1],levels(cpue_new$index)[4],levels(cpue_new$index)[2],levels(cpue_new$index)[3])
ggplot(cpue_new, aes(yrqtr,obs,colour=scenario))+ geom_line()+facet_wrap(~factor(index,levels=order_levels))+theme_fun()+
  xlab("Year")+ ylab("Scaled Joint LL")

SavePlot('CPUE_comparison_WPTT2021_2024',15,10)


# Read base SS inputs (from 2021 assessment)
base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
cpue_df = read.csv(file.path(shrpoint_path, SS_data, 'scaled_cpue.csv'))

cpue2021 <- base_dat$CPUE %>% mutate(scenario="WPTT2021")
cpue2024 <- cpue_df %>% mutate(scenario="WPTT2024") %>%   
  rename_with(.cols = 1:6,~ names(cpue2021))
cpue <- bind_rows(cpue2021,cpue2024) 
cpue<- cpue %>%  rename_with(.cols = 1,~ "qtr") %>%   mutate(yrqtr=qtr2yearqtr(qtr,1950,13))%>%
  mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))
cpue_new <- cpue
cpue_new$index <- as.factor(as.character(cpue_new$index))
levels(cpue_new$index) <- paste0("scaled cpue R",1:4)
order_levels <- c(levels(cpue_new$index)[1],levels(cpue_new$index)[4],levels(cpue_new$index)[2],levels(cpue_new$index)[3])
ggplot(cpue_new, aes(yrqtr,obs,colour=scenario))+ geom_line()+facet_wrap(~factor(index,levels=order_levels))+theme_fun()+
  xlab("Year")+ ylab("Scaled Joint LL")

SavePlot('CPUE_comparison_WPTT2021_2024',15,10)

#by season


ggplot(cpue[cpue$index==22,], aes(yrqtr,obs,colour=scenario))+ geom_line()+facet_wrap(~yrqtr_season)+theme_fun()+
  xlab("Year")+ ylab("Scaled Joint LL cpue R1 by season")
SavePlot('CPUE_comparison_WPTT2021_2024_season_R1',15,10)


ggplot(cpue[cpue$index==23,], aes(yrqtr,obs,colour=scenario))+ geom_line()+facet_wrap(~yrqtr_season)+theme_fun()+
  xlab("Year")+ ylab("Scaled Joint LL cpue R2 by season")
SavePlot('CPUE_comparison_WPTT2021_2024_season_R2',15,10)

ggplot(cpue[cpue$index==24,], aes(yrqtr,obs,colour=scenario))+ geom_line()+facet_wrap(~yrqtr_season)+theme_fun()+
  xlab("Year")+ ylab("Scaled Joint LL cpue R3 by season")
SavePlot('CPUE_comparison_WPTT2021_2024_season_R3',15,10)

ggplot(cpue[cpue$index==25,], aes(yrqtr,obs,colour=scenario))+ geom_line()+facet_wrap(~yrqtr_season)+theme_fun()+
  xlab("Year")+ ylab("Scaled Joint LL cpue R4 by season")

SavePlot('CPUE_comparison_WPTT2021_2024_season_R4',15,10)
