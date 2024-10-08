rm(list = ls())

# Sharepoint path and aux functions:
source('sharepoint_path.R')
source('code/auxiliary_functions.R')

# Spatial configuration:
spat_config = '1A_io'

# SS base files path (in Sharepoint):
# Make sure to use some updated 4A model using Farley's growth and tag + caal data:
SS_base = 'models/update/15_recDev2021_cv_CAAL_growth'

# SS configuration path (in Sharepoint):
SS_config = paste0('models/configurations/', spat_config)

# SS input data path (in Sharepoint):
SS_data = paste0('data/ss3_inputs/', spat_config)


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
catch_df = catch_df[,c('qtr', 'Quarter', 'ModelFleet', 'Catch')]
colnames(catch_df) = c('year', 'seas', 'fleet', 'catch')
catch_df = catch_df %>% mutate(catch_se = 0.01, seas = 1)
catch_df = bind_rows(tibble::tibble(year = -999, seas = 1, fleet = sort(unique(catch_df$fleet)), catch = 0, catch_se = 0.01),
                     catch_df)

# CPUE:
cpue_df = read_csv(file.path(shrpoint_path, SS_data, 'scaled_cpue_Meancv_02.csv'))
cpue_df = cpue_df %>% select(qtr, season, fleet, pr7994_m8_2R, cv)
cpue_df = cpue_df %>% dplyr::rename(year = qtr, seas = season, index = fleet, obs = pr7994_m8_2R, se_log = cv)
cpue_df$seas = 7

# Size:
size_df = read_csv(file.path(shrpoint_path, SS_data, 'size-original.csv'))
size_df = size_df %>% dplyr::rename(FltSvy = ModelFleet)
size_df$Seas = 7

# CAAL:
caal_df = read_csv(file.path(shrpoint_path, SS_data, 'caal.csv'))
caal_df = caal_df %>% dplyr::rename(FltSvy = ModelFleet, Lbin_lo = LowBin, Lbin_hi = HighBin)
caal_df = caal_df %>% mutate(Seas = 7, .after = Yr)
caal_df = caal_df %>% mutate(Gender = 0, Part = 0, Ageerr = 1, .after = FltSvy)

# Tagging:
tagrel_df = read_csv(file.path(shrpoint_path, SS_data, 'tag-release-farley.csv'))
tagrel_df = tagrel_df %>% dplyr::rename(area = rel_assessment_area, yr = rel_yr, 
                                        sex = gender, age = rel_age, Nrel = number_prime)
tagrec_df = read_csv(file.path(shrpoint_path, SS_data, 'tag-recapture-farley.csv'))
tagrec_df = tagrec_df %>% dplyr::rename(yr = rec_yr, fleet = rec_model_fleet, number = number_prime)


# -------------------------------------------------------------------------
# Adapt from 4A to 1A:

# Data file:
new_dat = base_dat
# Get fishery names:
fish_names = get_fisheries(spat_config)$fleet_name
fish_names = gsub(pattern = ' ', replacement = '_', x = fish_names)
# Basic info:
new_dat$endyr = 308
new_dat$Nfleet = length(fish_names) # fisheries
new_dat$Nfleets = new_dat$Nfleet + 1 # 12 + 1
new_dat$N_areas = 1
# Fleet info:
tmp_dat = base_dat$fleetinfo
tmp_dat = tmp_dat[c(1:9,12,14,21:22),]
tmp_dat$fleetname = c(fish_names, 'LLi_1')
tmp_dat$area = 1
new_dat$fleetinfo = tmp_dat
# Catch df:
new_dat$catch = as.data.frame(catch_df)
# CPUE info:
tmp_dat = base_dat$CPUEinfo
tmp_dat = tmp_dat[c(1:9,12,14,21:22),]
tmp_dat$fleet = 1:new_dat$Nfleets
new_dat$CPUEinfo = tmp_dat
# CPUE df:
new_dat$CPUE = as.data.frame(cpue_df)
# Len info:
tmp_dat = base_dat$len_info
tmp_dat = tmp_dat[c(1:9,12,14,21:22),]
new_dat$len_info = tmp_dat
# Len df
new_dat$lencomp = as.data.frame(size_df)
new_dat$lencomp$Nsamp = scales::rescale(new_dat$lencomp$Nsamp*-1, to = c(1,5))
# Age info:
tmp_dat = base_dat$age_info
tmp_dat = tmp_dat[c(1:9,12,14,21:22),]
new_dat$age_info = tmp_dat
# caal df:
new_dat$agecomp = as.data.frame(caal_df)
# tag info:
new_dat$N_tag_groups = max(tagrel_df$tag)
new_dat$N_recap_events = nrow(tagrec_df)
# tag df:
new_dat$tag_releases = as.data.frame(tagrel_df)
new_dat$tag_recaps = as.data.frame(tagrec_df)

# Control file:
new_ctl = base_ctl
new_ctl$recr_dist_method = 4 # no rec dist params
new_ctl$recr_dist_read = 1
new_ctl$recr_dist_pattern = base_ctl$recr_dist_pattern[1,]
new_ctl$N_areas = 1
new_ctl$MG_parms = new_ctl$MG_parms %>% dplyr::filter(!row_number() %in% c(25:26,28:39))
# change F_method later
# Q params:
tmp_ctl = base_ctl$Q_options
tmp_ctl = tmp_ctl[c(1), ]
tmp_ctl$fleet = new_dat$Nfleet + 1
new_ctl$Q_options = tmp_ctl
new_ctl$Q_parms = base_ctl$Q_parms[1, ]
# Selectivity info:
tmp_ctl = base_ctl$size_selex_types
tmp_ctl = tmp_ctl[c(1:9,12,14,21:22),]
new_ctl$size_selex_types = tmp_ctl
tmp_ctl = base_ctl$age_selex_types
tmp_ctl = tmp_ctl[c(1:9,12,14,21:22),]
new_ctl$age_selex_types = tmp_ctl
# Selectivity parameters:
new_ctl$age_selex_parms = new_ctl$age_selex_parms %>% dplyr::filter(!row_number() %in% c(31:34, 41:42))
# Tagging params:
new_ctl$TG_Loss_init = base_ctl$TG_Loss_init[1:new_dat$N_tag_groups, ]
new_ctl$TG_Loss_chronic = base_ctl$TG_Loss_chronic[1:new_dat$N_tag_groups, ]
new_ctl$TG_overdispersion = base_ctl$TG_overdispersion[1:new_dat$N_tag_groups, ]
new_ctl$TG_Report_fleet = base_ctl$TG_Report_fleet[c(1:9,12,14,21),]
new_ctl$TG_Report_fleet_decay = base_ctl$TG_Report_fleet_decay[c(1:9,12,14,21),]

# Remove priors in MG parameters:
new_ctl$MG_parms[,6] = 0
# Fix growth:
new_ctl$MG_parms[c(3:4, 17:18),7] = abs(new_ctl$MG_parms[c(3:4, 17:18),7])*-1

# -------------------------------------------------------------------------
# Write adapted files:
SS_writestarter(mylist = base_start, dir = file.path(shrpoint_path, 'models/base', spat_config), overwrite = TRUE)
SS_writeforecast(mylist = base_fore, dir = file.path(shrpoint_path, 'models/base', spat_config), overwrite = TRUE)
SS_writedat(datlist = new_dat, outfile = file.path(shrpoint_path, 'models/base', spat_config, 'data.ss'), overwrite = TRUE)
SS_writectl(ctllist = new_ctl, outfile = file.path(shrpoint_path, 'models/base', spat_config, 'control.ss'), overwrite = TRUE)

