rm(list = ls())

# Sharepoint path and aux functions:
source('sharepoint_path.R')
source('code/auxiliary_functions.R')

# Spatial configuration:
spat_config = '2A_io'
spat_subconfig = 'agg'

# SS base files path (in Sharepoint):
# Use RM3 
SS_base = 'models/RefModels/3_SplitCPUE_tag1_EC0_h08/nohess'

# SS input data path (in Sharepoint):
SS_data = file.path('data/ss3_inputs', spat_config, spat_subconfig)


# -------------------------------------------------------------------------

# Read base SS inputs (from 2021 assessment)
base_dat = SS_readdat(file = file.path(shrpoint_path, SS_base, 'data.ss'))
base_ctl = SS_readctl(file = file.path(shrpoint_path, SS_base, 'control.ss'), datlist = base_dat)
base_fore = SS_readforecast(file = file.path(shrpoint_path, SS_base, 'forecast.ss'))
base_start = SS_readstarter(file = file.path(shrpoint_path, SS_base, 'starter.ss'))

# -------------------------------------------------------------------------
# Read data inputs:

# Catch:
catch_df = read_csv(file.path(shrpoint_path, SS_data, 'catch.csv'))

# CPUE:
cpue_df = read_csv(file.path(shrpoint_path, SS_data, 'cpue-ll-qt.csv'))

# Size:
size_df = read_csv(file.path(shrpoint_path, SS_data, 'size-cwp55.csv'))

# CAAL:
caal_df = read_csv(file.path(shrpoint_path, SS_data, 'caal.csv'))

# Tagging:
tagrel_df = read_csv(file.path(shrpoint_path, SS_data, 'tag-release.csv'))
tagrec_df = read_csv(file.path(shrpoint_path, SS_data, 'tag-recapture.csv'))

# -------------------------------------------------------------------------
# Adapt from 4A to 2A-agg:

# Starter:
new_start = base_start
new_start$init_values_src = 0 # control

# Data file:
new_dat = base_dat
# Get fishery names:
fish_names = get_fisheries(spat_config)$fleet_name
# Basic info:
new_dat$Nfleet = length(fish_names) + 2 # fisheries + LL1A2000 + LL2A2000
new_dat$Nfleets = new_dat$Nfleet + 4 # 4 CPUEs
new_dat$N_areas = 2
# Fleet info:
tmp_dat = base_dat$fleetinfo
tmp_dat = tmp_dat[c(1:9,13,12,14:15,19:22,24:25,28:29,31),]
tmp_dat$fleetname = c(fish_names, 'LL_1b_A2000', 'LL_2_A2000', 'CPUE_LL_1_P2000', 'CPUE_LL_2_P2000',
                      'CPUE_LL_1_A2000', 'CPUE_LL_2_A2000')
tmp_dat = tmp_dat %>% mutate(area = if_else(area > 2, 2, 1))
new_dat$fleetinfo = tmp_dat
# Catch df:
tmp_catch = catch_df
tmp_catch = tmp_catch %>% mutate(fleet = if_else(fleet == 7 & year >= 213, 17, fleet)) # LL1b
tmp_catch = tmp_catch %>% mutate(fleet = if_else(fleet == 10 & year >= 213, 18, fleet)) # LL2
new_dat$catch = as.data.frame(tmp_catch)
# CPUE info:
tmp_dat = base_dat$CPUEinfo
tmp_dat = tmp_dat[c(1:9,13,12,14:15,19:22,24:25,28:29,31),]
tmp_dat$fleet = 1:new_dat$Nfleets
new_dat$CPUEinfo = tmp_dat
# CPUE df:
tmp_cpue = cpue_df
tmp_cpue$index = tmp_cpue$index + 2
tmp_cpue = tmp_cpue %>% mutate(index = if_else(year >= 213, index + 2, index)) # LL1b
new_dat$CPUE = as.data.frame(tmp_cpue)
# Len info:
tmp_dat = base_dat$len_info
tmp_dat = tmp_dat[c(1:9,13,12,14:15,19:22,24:25,28:29,31),]
new_dat$len_info = tmp_dat
# Len df
tmp_size = size_df
tmp_size = tmp_size %>% mutate(FltSvy = if_else(FltSvy == 7 & Yr >= 213, 17, FltSvy)) # LL1b
tmp_size = tmp_size %>% mutate(FltSvy = if_else(FltSvy == 10 & Yr >= 213, 18, FltSvy)) # LL2
new_dat$lencomp = as.data.frame(tmp_size)
new_dat$lencomp$Nsamp = ifelse(new_dat$lencomp$Nsamp <= 2, 5, 2)
new_dat$lencomp$Nsamp[new_dat$lencomp$FltSvy %in% c(7,10)] = 5 # only before 2000, why?
# Age info:
tmp_dat = new_dat$len_info
tmp_dat$mintailcomp = -1
tmp_dat$addtocomp = 1e-07
tmp_dat$minsamplesize = 0.001
new_dat$age_info = tmp_dat
new_dat$agebin_vector = as.numeric(colnames(caal_df %>% select(`0`:`40`)))
new_dat$N_agebins = ncol(caal_df %>% select(`0`:`40`))
new_dat$N_ageerror_definitions = 1
tmp_dat = matrix(NA, nrow = 2, ncol = new_dat$Nages + 1)
tmp_dat[1,] = 0:new_dat$Nages + 0.5
tmp_dat[2,] = 0.001
new_dat$ageerror = as.data.frame(tmp_dat)
new_dat$Lbin_method = 3
# caal df:
caal_df = caal_df %>% mutate(FltSvy = if_else(FltSvy == 7 & Yr >= 213, 17, FltSvy)) # LL1b
caal_df = caal_df %>% mutate(FltSvy = if_else(FltSvy == 10 & Yr >= 213, 18, FltSvy)) # LL2
new_dat$agecomp = as.data.frame(caal_df)
# tag info:
new_dat$N_tag_groups = max(tagrel_df$tag)
new_dat$N_recap_events = nrow(tagrec_df)
# tag df:
new_dat$tag_releases = as.data.frame(tagrel_df)
new_dat$tag_recaps = as.data.frame(tagrec_df)
new_dat$tag_recaps = new_dat$tag_recaps %>% mutate(fleet = if_else(fleet == 7 & yr >= 213, 17, fleet)) # LL1b
new_dat$tag_recaps = new_dat$tag_recaps %>% mutate(fleet = if_else(fleet == 10 & yr >= 213, 18, fleet)) # LL2

# Control file:
new_ctl = base_ctl
new_ctl$recr_dist_pattern$area[2] = 2 # rec in area 2
new_ctl$N_areas = 2
new_ctl$N_moveDef = 2
new_ctl$moveDef = base_ctl$moveDef[c(1, 3), ]
new_ctl$MG_parms = new_ctl$MG_parms %>% dplyr::filter(!row_number() %in% c(30:31,34:39))
# change F_method later
# Q params:
tmp_ctl = base_ctl$Q_options
tmp_ctl = tmp_ctl[c(1,4:5,7), ]
tmp_ctl$fleet = 19:22
tmp_ctl$link_info[2:4] = 19
new_ctl$Q_options = tmp_ctl
new_ctl$Q_parms = base_ctl$Q_parms[c(1,4:5,7), ]
# Selectivity info:
tmp_ctl = base_ctl$size_selex_types
tmp_ctl = tmp_ctl[c(1:9,13,12,14:15,19:22,24:25,28:29,31),]
new_ctl$size_selex_types = tmp_ctl
tmp_ctl = base_ctl$age_selex_types
tmp_ctl = tmp_ctl[c(1:9,13,12,14:15,19:22,24:25,28:29,31),]
tmp_ctl$Special[19:22] = c(7,10,17,18)
new_ctl$age_selex_types = tmp_ctl
# Selectivity parameters:
tmp_selex = new_ctl$age_selex_parms %>% dplyr::filter(!row_number() %in% c(35:38, 61:62))
new_ctl$age_selex_parms = tmp_selex[c(1:34,41:46,35:40,47:58), ] # reorder
# Tagging params:
new_ctl$TG_Loss_init = base_ctl$TG_Loss_init[1:new_dat$N_tag_groups, ]
new_ctl$TG_Loss_chronic = base_ctl$TG_Loss_chronic[1:new_dat$N_tag_groups, ]
new_ctl$TG_overdispersion = base_ctl$TG_overdispersion[1:new_dat$N_tag_groups, ]
new_ctl$TG_Report_fleet = base_ctl$TG_Report_fleet[c(1:9, 13,12,14:15,19:22,24),]
new_ctl$TG_Report_fleet_decay = base_ctl$TG_Report_fleet_decay[c(1:9, 13,12,14:15,19:22,24),]

# -------------------------------------------------------------------------
# Write adapted files:
SS_writestarter(mylist = new_start, dir = file.path(shrpoint_path, 'models/base', spat_config, spat_subconfig), overwrite = TRUE)
SS_writeforecast(mylist = base_fore, dir = file.path(shrpoint_path, 'models/base', spat_config, spat_subconfig), overwrite = TRUE)
SS_writedat(datlist = new_dat, outfile = file.path(shrpoint_path, 'models/base', spat_config, spat_subconfig, 'data.ss'), overwrite = TRUE)
SS_writectl(ctllist = new_ctl, outfile = file.path(shrpoint_path, 'models/base', spat_config, spat_subconfig, 'control.ss'), overwrite = TRUE)
