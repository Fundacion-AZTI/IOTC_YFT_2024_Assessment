# Code to generate new catch inputs with new estimates from IDN
# New data was provided by Dan Fu and Manu Chassot, but still need to be further processing
# This is just for sentivitiy purposes
rm(list = ls())

# Sharepoint path:
source('sharepoint_path.R')
source(here('code', 'parameters_for_plots.R'))

# Read auxiliary functions:
source(here('code', 'auxiliary_functions.R'))

# Only do this for 4A config:
spat_config = '4A_io'

# Fishery labels
fish_info = get_fisheries(spat_config)
ModelFisheries = fish_info$fleet_name

# -------------------------------------------------------------------------
# First, make sure that the ORIG dataset provided at annual scale is the same as the base database:
# IDN_ori = read_xlsx(file.path(shrpoint_path, 'data/raw', "IDN_raised_data_ORIG_20241017.xlsx"))
# IDN_ori = IDN_ori %>% select(-c(`Grand Total`))
# IDN_ori = gather(IDN_ori, 'Year', 'Catch', `1950`:`2023`)
# IDN_ori = IDN_ori %>% mutate(Year = as.integer(Year)) %>% 
#             dplyr::rename(Gear = GEAR_CODE, SchoolType = CATCH_SCHOOL_TYPE_CODE, FisheryCode = FISHERY_CODE)
# # Remove ART IND types:
# IDN_ori = IDN_ori %>% group_by(Year, Gear, FisheryCode, SchoolType) %>% summarise(Catch = sum(Catch)) %>% 
#             na.omit # remove missing information
# # Order dataframe to compare:
# IDN_ori = IDN_ori %>% arrange(Year, Gear, FisheryCode, SchoolType)
# 
# # Read traditional CE data after preprocessing:
# Data = read.csv(file.path(shrpoint_path, 'data/processed', 'catch_grid.csv'))
# IDN_base = Data %>% dplyr::filter(Fleet == 'IDN') %>% 
#           group_by(Year, Gear, SchoolType, FisheryCode) %>% summarise(Catch = sum(Catch))
# # Order dataframe to compare:
# IDN_base = IDN_base %>% arrange(Year, Gear, FisheryCode, SchoolType)
# 
# # Compare:
# range(IDN_ori$Catch - IDN_base$Catch)
# Correct, both are equal

# -------------------------------------------------------------------------
# New estimates:
IDN_est = read_csv(file.path(shrpoint_path, 'data/raw', "IDN_FinalEstimates_1950_2022_2023_20241017.csv"))
IDN_est = gather(IDN_est, 'Year', 'Catch', `1950`:`2023`)
IDN_est = IDN_est %>% mutate(Year = as.integer(Year)) %>% 
  dplyr::rename(Gear = GEAR, SchoolType = CATCH_SCHOOL_TYPE_CODE, FisheryCode = FISHERY)
# Remove ART IND types:
IDN_est = IDN_est %>% group_by(Year, Gear, FisheryCode, SchoolType) %>% summarise(Catch = sum(Catch)) %>% 
  na.omit # remove missing information

# Plot IDN annual catch between original and new estimates:
plot_data = rbind(IDN_ori %>% group_by(Year, FisheryCode) %>% summarise(Catch = sum(Catch)) %>% mutate(type = 'Original'),
                  IDN_est %>% group_by(Year, FisheryCode) %>% summarise(Catch = sum(Catch)) %>% mutate(type = 'New IDN estimates'))
p1 = ggplot(data = plot_data, aes(x = Year, y = Catch, color = type)) +
  geom_line() +
  ylab('Catch (mt)') + xlab(NULL) +
  scale_y_continuous(n.breaks = 4) +
  theme(legend.position = c(0.85, 0.15),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  guides(color = guide_legend(title = NULL)) +
  facet_wrap(~FisheryCode) 
ggsave(file.path(shrpoint_path, plot_dir, paste0('IDN_catch_comp', img_type)), 
       plot = p1, width = img_width, height = 170, units = 'mm', dpi = img_res)


# There is no quarter resolution, so we just assumed homogeneous fishing within a year:
# First repeat every row 4 times:
IDN_est = IDN_est %>% slice(rep(1:n(), each = 4))
# Now divide by 4 the Catch column and add quarter and Fleet columns:
IDN_est = IDN_est %>% mutate(Catch = Catch/4, Quarter = 1:4, Fleet = 'IDN')

# Read traditional CE data after preprocessing:
data = read.csv(file.path(shrpoint_path, 'data/processed', 'catch_grid.csv'))
# Add area information
data$Area = get_4Aarea_from_lonlat(data$long, data$lat)
# All samples from east IO. Approx 12% catch from Region 3.
# Here, remove IDN from base data, and then merge it with the new estimates:
data_tmp = data %>% dplyr::filter(!(Fleet == 'IDN'))
# Since there is not spatial information for new estimates, assumed Area == 5 (region 4), as suggested by Dan
data = bind_rows(data_tmp, IDN_est %>% mutate(Area = 5))

# Continue code as always:
data = create_4Aarea_cols(data)
data = data %>% 
  dplyr::mutate(ModelFishery = paste(FisheryCode, AssessmentAreaName, sep = '_')) %>% 
  dplyr::mutate(ModelFleet = as.numeric(factor(ModelFishery,levels=ModelFisheries)))
which(is.na(data$ModelFishery))
which(is.na(data$ModelFleet))
# Aggregate data:
agg_data = data %>% group_by(Year, Quarter, Fleet, Gear, ModelFleet, ModelFishery) %>%
  summarise(NCmtFish = sum(NCmtFish)) 
# Prepare for SS3:
work = data	%>% 
  group_by(Year,Quarter,ModelFishery) %>% 
  summarise(Catch = sum(Catch)) %>% as.data.frame() %>%	
  #spread(ModelFishery,Catch,fill=0) %>% 
  mutate(qtr = yearqtr2qtr(Year,Quarter,1950,13)) %>%
  mutate(ModelFleet = as.numeric(factor(ModelFishery,levels=ModelFisheries)))
table(work$ModelFleet)
which(is.na(work$ModelFleet))
catch_df = work[,c('qtr', 'Quarter', 'ModelFleet', 'Catch')]
colnames(catch_df) = c('year', 'seas', 'fleet', 'catch')
catch_df = catch_df %>% mutate(catch_se = 0.01, seas = 1)
catch_df = bind_rows(tibble::tibble(year = -999, seas = 1, fleet = sort(unique(catch_df$fleet)), catch = 0, catch_se = 0.01),
                     catch_df)
# Save SS catch input
write.csv(catch_df, file = file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'catch-IDN.csv'), row.names = FALSE)


# -------------------------------------------------------------------------
# Compare original catch input with IDN-new estimates input:

# 2024 catch:
catch_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'catch.csv'))
catch_dat = catch_dat[,c('qtr', 'ModelFleet', 'Catch')]
colnames(catch_dat) = c('time', 'fleet_number', 'catch')
catch_dat = catch_dat %>% mutate(type = 'Original')

# 2024 IDN catch:
catchIDN_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'catch-IDN.csv'))
catchIDN_dat = catchIDN_dat[,c('year', 'fleet', 'catch')]
colnames(catchIDN_dat) = c('time', 'fleet_number', 'catch')
catchIDN_dat = catchIDN_dat %>% mutate(type = 'New IDN estimates')

# Merge datasets:
merged_catch = rbind(catch_dat, catchIDN_dat)
merged_catch = merged_catch %>% dplyr::filter(time >= 13)
merged_catch = left_join(merged_catch, fish_info)
merged_catch$time = ssts2yq(merged_catch$time) # transform to yr-qt

# Make plot:
p1 = ggplot(data = merged_catch, aes(x = time, y = catch*1e-03)) +
  geom_line(aes(color = type), linewidth = 0.2) +
  ylab("Catch (thousands of tons)") + xlab(NULL) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        legend.position = c(0.6, 0.05)) +
  scale_y_continuous(breaks = breaks_extended(3)) +
  guides(color = guide_legend(title = NULL)) +
  facet_wrap( ~ fleet_name, scales = 'free_y', ncol = 4)
ggsave(file.path(shrpoint_path, plot_dir, paste0('compare_catch_IDN', img_type)), plot = p1,
       width = img_width, height = 200, units = 'mm', dpi = img_res)
