rm(list = ls())

# Sharepoint path and aux functions:
source('sharepoint_path.R')
source('code/auxiliary_functions.R')

# Spatial configuration:
spat_config = '2A_io'
spat_subconfig = 'aaf'

# SS base files path (in Sharepoint):
# Use RM2 (16_LLsplit_LL1b_LL4_DN_min)
SS_base = 'models/update/16_LLsplit_LL1b_LL4_DN_min'

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

# CPUE:
cpue_df = read_csv(file.path(shrpoint_path, SS_data, 'cpue-ll-qt.csv'))

# CAAL:
caal_df = read_csv(file.path(shrpoint_path, SS_data, 'caal.csv'))

# Tagging:
tagrel_df = read_csv(file.path(shrpoint_path, SS_data, 'tag-release.csv'))
tagrec_df = read_csv(file.path(shrpoint_path, SS_data, 'tag-recapture.csv'))


# -------------------------------------------------------------------------
# Adapt from 4A to 2A-aaf:

# Starter:
new_start = base_start
new_start$init_values_src = 0 # control

# Data file:
new_dat = base_dat
# Get fishery names:
fish_names = get_fisheries('4A_io')$fleet_name
# Basic info:
new_dat$Nfleets = new_dat$Nfleet + 2 # only two indices
new_dat$N_areas = 2
# Fleet info:
tmp_dat = base_dat$fleetinfo
tmp_dat = tmp_dat[1:new_dat$Nfleets,]
tmp_dat$fleetname[(new_dat$Nfleet+1):nrow(tmp_dat)] = c('CPUE_LL1', 'CPUE_LL2')
tmp_dat = tmp_dat %>% mutate(area = if_else(area > 2, 2, 1))
new_dat$fleetinfo = tmp_dat
# Catch df:
# CPUE info:
tmp_dat = base_dat$CPUEinfo
tmp_dat = tmp_dat[1:new_dat$Nfleets,]
new_dat$CPUEinfo = tmp_dat
# CPUE df:
cpue_df$index = cpue_df$index + 8
new_dat$CPUE = as.data.frame(cpue_df)
# Len info:
tmp_dat = base_dat$len_info
tmp_dat = tmp_dat[1:new_dat$Nfleets,]
new_dat$len_info = tmp_dat
# Len df
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
caal_df = caal_df %>% mutate(FltSvy = if_else(FltSvy == 7 & Yr >= 213, 22, FltSvy)) # LL1b
caal_df = caal_df %>% mutate(FltSvy = if_else(FltSvy == 10 & Yr >= 213, 23, FltSvy)) # LL2
caal_df = caal_df %>% mutate(FltSvy = if_else(FltSvy == 13 & Yr >= 213, 24, FltSvy)) # LL4
new_dat$agecomp = as.data.frame(caal_df)
# tag info:
new_dat$N_tag_groups = max(tagrel_df$tag)
new_dat$N_recap_events = nrow(tagrec_df)
# tag df:
new_dat$tag_releases = as.data.frame(tagrel_df)
new_dat$tag_recaps = as.data.frame(tagrec_df)
new_dat$tag_recaps = new_dat$tag_recaps %>% mutate(fleet = if_else(fleet == 7 & yr >= 213, 22, fleet)) # LL1b
new_dat$tag_recaps = new_dat$tag_recaps %>% mutate(fleet = if_else(fleet == 10 & yr >= 213, 23, fleet)) # LL2
new_dat$tag_recaps = new_dat$tag_recaps %>% mutate(fleet = if_else(fleet == 13 & yr >= 213, 24, fleet)) # LL4

# Control file:
new_ctl = base_ctl
new_ctl$recr_dist_pattern$area[2] = 2 # rec in area 2
new_ctl$N_areas = 2
new_ctl$N_moveDef = 2
new_ctl$moveDef = base_ctl$moveDef[c(1, 3), ]
new_ctl$MG_parms = new_ctl$MG_parms %>% dplyr::filter(!row_number() %in% c(30:31,34:39)) # remove some movement and recdist params
# change F_method later
# Q params:
tmp_ctl = base_ctl$Q_options
tmp_ctl = tmp_ctl[1:2, ]
new_ctl$Q_options = tmp_ctl
new_ctl$Q_parms = base_ctl$Q_parms[1:2, ]
# Selectivity info:
tmp_ctl = base_ctl$size_selex_types
tmp_ctl = tmp_ctl[1:new_dat$Nfleets,]
new_ctl$size_selex_types = tmp_ctl
tmp_ctl = base_ctl$age_selex_types
tmp_ctl = tmp_ctl[1:new_dat$Nfleets,]
tmp_ctl$Special[nrow(tmp_ctl)] = 13 # use LL4 selex
new_ctl$age_selex_types = tmp_ctl
# Tagging params:
new_ctl$TG_Loss_init = base_ctl$TG_Loss_init[1:new_dat$N_tag_groups, ]
new_ctl$TG_Loss_chronic = base_ctl$TG_Loss_chronic[1:new_dat$N_tag_groups, ]
new_ctl$TG_overdispersion = base_ctl$TG_overdispersion[1:new_dat$N_tag_groups, ]

# -------------------------------------------------------------------------
# Write adapted files:
SS_writestarter(mylist = new_start, dir = file.path(shrpoint_path, 'models/base', spat_config, spat_subconfig), overwrite = TRUE)
SS_writeforecast(mylist = base_fore, dir = file.path(shrpoint_path, 'models/base', spat_config, spat_subconfig), overwrite = TRUE)
SS_writedat(datlist = new_dat, outfile = file.path(shrpoint_path, 'models/base', spat_config, spat_subconfig, 'data.ss'), overwrite = TRUE)
SS_writectl(ctllist = new_ctl, outfile = file.path(shrpoint_path, 'models/base', spat_config, spat_subconfig, 'control.ss'), overwrite = TRUE)
