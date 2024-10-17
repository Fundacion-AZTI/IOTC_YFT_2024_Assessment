
library(ggplot2)
library(here)

# Set working directiry using here():
proj_dir = here()

source(here('code', 'auxiliary_functions.R'))

# Read path and parameters for plots:
source('sharepoint_path.R')
setwd(shrpoint_path)

dataRQ <- read.csv("data/ss3_inputs/4A_io/size-cwp55.csv") 
dataRQ <- dataRQ %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))

head(dataRQ)
data
ggplot(dataRQ, aes(x=yrqtr,y=ModelFleet,fill=Nsamp))+
  geom_tile(color = "white", lwd = 0.01, linetype = 1) +  coord_fixed()+
  theme_fun()+xlab("Year") + ylab("Fleet")
SavePlot('ReportQuality',15,10)

dataRQ_v2$Nsamp <- ifelse(dataRQ$Nsamp<=3,0,1)

ggplot(dataRQ_v2, aes(x=yrqtr,y=ModelFleet,fill=Nsamp))+
  geom_tile(color = "white", lwd = 0.01, linetype = 1) +  coord_fixed()+
  theme_fun()+xlab("Year") + ylab("Fleet")
SavePlot('GOODReportQuality_smalEq3',15,10)

dataRQ_v2 <- dataRQ
dataRQ_v2$Nsamp <- ifelse(dataRQ$Nsamp<=2,0,1)

ggplot(dataRQ_v2, aes(x=yrqtr,y=ModelFleet,fill=Nsamp))+
  geom_tile(color = "white", lwd = 0.01, linetype = 1) +  coord_fixed()+
  theme_fun()+xlab("Year") + ylab("Fleet")
SavePlot('GOODReportQuality_smalEq2',15,10)

write.csv(dataRQ_v2,file = file.path("data","ss3_inputs","4A_io","size_irregular-RQ-smalEq2.csv"),row.names=FALSE)


#.............................................................#
#
#       PREVIOUS REPORT QUALITY
#

SS_base = 'models/base_win_vs_lin/4A_io_lin_v33022_FixedParam2_Fl11'

# SS configuration path (in Sharepoint):
SS_config = 'models/update'

# SS input data path (in Sharepoint):
SS_data = 'data/ss3_inputs/4A_io'

# -------------------------------------------------------------------------

# Read base SS inputs (from 2021 assessment)
base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
lencomp <- base_dat$lencomp %>%   mutate(yrqtr=qtr2yearqtr(year,1950,13))
ggplot(lencomp, aes(x=yrqtr,y=fleet,fill=Nsamp))+
  geom_tile(color = "white", lwd = 0.01, linetype = 1) +  coord_fixed()+
  theme_fun()+xlab("Year") + ylab("Fleet")
SavePlot('LenData_2021',15,10)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Simple aggregation
size_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size-original.csv'))
size_dat = size_dat %>% select(Yr, ModelFleet, Nsamp)
size_dat = dplyr::rename(size_dat, c(time = 'Yr', fleet_number = 'ModelFleet'))
colnames(size_dat) = tolower(colnames(size_dat))
# Merge
size_dat = left_join(size_dat, fish_info)
size_dat = size_dat %>% mutate(fisherycode = str_sub(fleet_name, start = 1, end = 2)) # for colors
size_dat = size_dat %>% mutate(time2 = ssts2yq(time))

size_dat$nsampGB <- ifelse(size_dat$nsamp<=2,5,2)
pGB <-  ggplot(size_dat, aes(x = time2, y = fleet_name, fill = fisherycode)) + 
  geom_point(aes(size = nsampGB), pch = 21, color = 'black') +
  ylab(NULL) + xlab(NULL) +
  # scale_fill_manual(values = fleet_col) +
  scale_size_continuous(range = c(1, 3)) +
  coord_cartesian(xlim = c(1955, 2023)) +   
  theme_classic() +
  theme(legend.position = 'none') +
  ggtitle(label = 'Simple aggregation')

ggsave(file.path(shrpoint_path, plot_dir, paste0('rq_size_goodAndBad', img_type)), plot = pGB,
       width = img_width, height = 220, units = 'mm', dpi = img_res)

# Catch raised aggregation
size_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size-cwp55.csv'))
size_dat = size_dat %>% select(Yr, ModelFleet, Nsamp)
size_dat = dplyr::rename(size_dat, c(time = 'Yr', fleet_number = 'ModelFleet'))
colnames(size_dat) = tolower(colnames(size_dat))
# Merge
size_dat = left_join(size_dat, fish_info)
size_dat = size_dat %>% mutate(fisherycode = str_sub(fleet_name, start = 1, end = 2)) # for colors
size_dat = size_dat %>% mutate(time2 = ssts2yq(time))

#good <=2 and bad >2

#good <=2 and bad >2 catch raised

size_dat$nsampGB <- ifelse(size_dat$nsamp<=2,5,2)

pGB2<- ggplot(size_dat, aes(x = time2, y = fleet_name, fill = fisherycode)) + 
  geom_point(aes(size = nsampGB), pch = 21, color = 'black') +
  ylab(NULL) + xlab(NULL) +
  # scale_fill_manual(values = fleet_col) +
  scale_size_continuous(range = c(1, 3)) +
  coord_cartesian(xlim = c(1955, 2023)) +
  theme_classic() +
  theme(legend.position = 'none') +
  ggtitle(label = 'Catch-raised aggregation')

ggsave(file.path(shrpoint_path, plot_dir, paste0('rq_size_goodAndBad_catch_raised', img_type)), plot = pGB2,
       width = img_width, height = 220, units = 'mm', dpi = img_res)


