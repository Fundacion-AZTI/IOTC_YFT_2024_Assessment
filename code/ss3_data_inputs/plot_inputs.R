# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code', 'parameters_for_plots.R'))
source(here('code', 'auxiliary_functions.R'))

# -------------------------------------------------------------------------
# Plot 2024 SS inputs -----------------------------------------------------


# Catch per fleet and year as barplot -------------------------------------

spat_config = '4A_io'
catch_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'catch.csv'))
catch_dat = catch_dat %>% mutate(FisheryCode = str_sub(ModelFishery, start = 1, end = 2))

# Plot data:
plot_data = catch_dat %>% group_by(FisheryCode, Year) %>% summarise(Catch = sum(Catch)*1e-03)
colnames(plot_data) = tolower(colnames(plot_data))

p1 = ggplot(data = plot_data, aes(x = year, y = catch, fill = fisherycode)) +
  geom_col() +
  xlab(NULL) + ylab("Catch (thousands of tons)") +
  scale_fill_manual(values = fleet_col) +
  theme(legend.position = c(0.1, 0.75)) +
  guides(fill = guide_legend(title = NULL)) 
ggsave(file.path(shrpoint_path, plot_dir, paste0('ts_catch', img_type)), plot = p1,
       width = img_width, height = 130, units = 'mm', dpi = img_res)

p2 = ggplot(data = plot_data, aes(x = year, y = catch, fill = fisherycode)) +
  geom_col(position = 'fill') +
  xlab(NULL) + ylab("Catch fraction") +
  scale_fill_manual(values = fleet_col) +
  guides(fill = guide_legend(title = NULL)) 
ggsave(file.path(shrpoint_path, plot_dir, paste0('ts_catch_frac', img_type)), plot = p2,
       width = img_width, height = 130, units = 'mm', dpi = img_res)



# Catch per fleet, year, and area as barplot ------------------------------

spat_config = '4A_io'
catch_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'catch.csv'))
catch_dat = catch_dat %>% mutate(FisheryCode = str_sub(ModelFishery, start = 1, end = 2),
                                 Area = str_sub(ModelFishery, start = 4))
catch_dat = catch_dat %>% mutate(Area = if_else(Area %in% c('1a', '1b'), '1', Area))

# Plot data:
plot_data = catch_dat %>% group_by(FisheryCode, Year, Area) %>% summarise(Catch = sum(Catch)*1e-03)
colnames(plot_data) = tolower(colnames(plot_data))
plot_data$area = factor(plot_data$area, levels = c('1', '4', '2', '3'))

p1 = ggplot(data = plot_data, aes(x = year, y = catch, fill = fisherycode)) +
  geom_col() +
  xlab(NULL) + ylab("Catch (thousands of tons)") +
  scale_fill_manual(values = fleet_col) +
  guides(fill = guide_legend(title = NULL)) +
  facet_wrap( ~ area)
ggsave(file.path(shrpoint_path, plot_dir, paste0('ts_catch_area', img_type)), plot = p1,
       width = img_width, height = 130, units = 'mm', dpi = img_res)

p2 = ggplot(data = plot_data, aes(x = year, y = catch, fill = fisherycode)) +
  geom_col(position = 'fill') +
  xlab(NULL) + ylab("Catch fraction") +
  scale_fill_manual(values = fleet_col) +
  guides(fill = guide_legend(title = NULL)) +
  facet_wrap( ~ area)
ggsave(file.path(shrpoint_path, plot_dir, paste0('ts_catch_area_frac', img_type)), plot = p2,
       width = img_width, height = 130, units = 'mm', dpi = img_res)



# Scale LL CPUE per time step with CV -------------------------------------

spat_config = '4A_io'
cpue_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'scaled_cpue_Meancv_02.csv'))
cpue_dat = cpue_dat %>% mutate(area = if_else(fleet == 22, '1b', 
                                              if_else(fleet == 23, '2', 
                                                      if_else(fleet == 24, '3', '4'))))
cpue_dat = cpue_dat %>% mutate(time = ssts2yq(qtr))
cpue_dat = cpue_dat %>% dplyr::rename(obs = pr_7994_m8)
cpue_dat = cpue_dat %>% mutate(sd = obs*cv)

# Plot data:
plot_data = cpue_dat
plot_data$area = factor(plot_data$area, levels = c('1b', '4', '2', '3'))

p1 = ggplot(data = plot_data, aes(x = time, y = obs)) +
  geom_ribbon(aes(ymin = obs - sd, ymax = obs + sd), fill = 'grey70') +
  geom_line(aes(y = obs)) +
  coord_cartesian(ylim = c(0, NA)) + 
  xlab(NULL) + ylab("Scaled CPUE") +
  facet_wrap( ~ area, scales = 'free_y')
ggsave(file.path(shrpoint_path, plot_dir, paste0('ts_cpue_area', img_type)), plot = p1,
       width = img_width, height = 130, units = 'mm', dpi = img_res)



# -------------------------------------------------------------------------
# Compare 2021 and 2024 SS inputs -----------------------------------------

# Read 2021 SS data inputs
spat_config = '4A_io'
base_dat = SS_readdat(file = file.path(shrpoint_path, 'models/base', spat_config, 'data.ss'))


# Compare catch information -----------------------------------------------

# 2024 catch:
catch_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'catch.csv'))
fleet_name_df = catch_dat %>% group_by(ModelFleet) %>% summarise(fleet_name = unique(ModelFishery))
colnames(fleet_name_df) = c('fleet_number', 'fleet_name')
catch_dat = catch_dat[,c('qtr', 'ModelFleet', 'Catch')]
colnames(catch_dat) = c('time', 'fleet_number', 'catch')
catch_dat = catch_dat %>% mutate(type = '2024 assessment')

# 2021 catch:
old_catch_dat = base_dat$catch
old_catch_dat = old_catch_dat[,c('year', 'fleet', 'catch')]
colnames(old_catch_dat) = c('time', 'fleet_number', 'catch')
old_catch_dat = old_catch_dat %>% mutate(type = '2021 assessment')

# Merge datasets:
merged_catch = rbind(catch_dat, old_catch_dat)
merged_catch = merged_catch %>% dplyr::filter(time >= 13)
merged_catch = left_join(merged_catch, fleet_name_df)
merged_catch$time = ssts2yq(merged_catch$time) # transform to yr-qt

# Make plot:
p1 = ggplot(data = merged_catch, aes(x = time, y = catch*1e-03)) +
  geom_line(aes(color = type)) +
  ylab("Catch (thousands of tons)") + xlab(NULL) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        legend.position = c(0.6, 0.05)) +
  scale_y_continuous(breaks = breaks_extended(3)) +
  guides(color = guide_legend(title = NULL)) +
  facet_wrap( ~ fleet_name, scales = 'free_y', ncol = 4)
ggsave(file.path(shrpoint_path, plot_dir, paste0('compare_catch', img_type)), plot = p1,
       width = img_width, height = 200, units = 'mm', dpi = img_res)


# Compare LL CPUE information ---------------------------------------------

# 2024 CPUE:
cpue_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'scaled_cpue.csv'))
cpue_dat = cpue_dat[,c('qtr', 'fleet', 'pr_7994_m8')]
colnames(cpue_dat) = c('time', 'fleet_number', 'obs')
cpue_dat = cpue_dat %>% mutate(type = '2024 assessment')

# 2021 CPUE:
old_cpue_dat = base_dat$CPUE
old_cpue_dat = old_cpue_dat[,c('year', 'index', 'obs')]
colnames(old_cpue_dat)[1:2] = c('time', 'fleet_number')
old_cpue_dat = old_cpue_dat %>% mutate(type = '2021 assessment')

# Merge datasets:
merged_cpue = rbind(cpue_dat, old_cpue_dat)
merged_cpue = merged_cpue %>% mutate(fleet_name = if_else(fleet_number == 22, 'LL 1b', 
                                                          if_else(fleet_number == 23, 'LL 2', 
                                                                  if_else(fleet_number == 24, 'LL 3', 'LL 4'))))
merged_cpue$fleet_name = factor(merged_cpue$fleet_name, levels = c('LL 1b', 'LL 4', 'LL 2', 'LL 3'))
merged_cpue$time = ssts2yq(merged_cpue$time) # transform to yr-qt

# Make plot:
p1 = ggplot(data = merged_cpue, aes(x = time, y = obs)) +
  geom_line(aes(color = type)) +
  ylab("Scaled CPUE") + xlab(NULL) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        legend.position = 'bottom') +
  scale_y_continuous(breaks = breaks_extended(3)) +
  guides(color = guide_legend(title = NULL)) +
  facet_wrap( ~ fleet_name, ncol = 2)
ggsave(file.path(shrpoint_path, plot_dir, paste0('compare_cpue', img_type)), plot = p1,
       width = img_width, height = 170, units = 'mm', dpi = img_res)


# Compare (traditional) size information ------------------------------------------------
# This will use the size matrix with bug

# 2024 size:
size_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size-bug.csv'))
size_dat = size_dat %>% select(Yr, ModelFleet, L010:L198)
size_dat = dplyr::rename(size_dat, c(time = 'Yr', fleet_number = 'ModelFleet'))
colnames(size_dat) = tolower(colnames(size_dat))
size_dat = size_dat %>% dplyr::filter(time %in% 13:296) # same period for both assessments
# Aggregate over time:
size_dat = size_dat %>% group_by(fleet_number) %>% summarise_at(.vars = colnames(.)[3:ncol(size_dat)], sum)
# Sum by row to get freq:
size_dat = size_dat %>% ungroup() %>% mutate(across(-1)/rowSums(across(-1)))
size_dat = size_dat %>% mutate(type = '2024 assessment', .after = 'fleet_number')

# 2021 size:
old_size_dat = base_dat$lencomp
old_size_dat = old_size_dat %>% select(Yr, FltSvy, l10:l198)
old_size_dat = dplyr::rename(old_size_dat, c(time = 'Yr', fleet_number = 'FltSvy'))
old_size_dat = old_size_dat %>% dplyr::filter(time %in% 13:296)
# Aggregate over time:
old_size_dat = old_size_dat %>% group_by(fleet_number) %>% summarise_at(.vars = colnames(.)[3:ncol(old_size_dat)], sum)
# Sum by row to get freq:
old_size_dat = old_size_dat %>% ungroup() %>% mutate(across(-1)/rowSums(across(-1)))
old_size_dat = old_size_dat %>% mutate(type = '2021 assessment', .after = 'fleet_number')
colnames(old_size_dat)[3:ncol(old_size_dat)] = colnames(size_dat)[3:ncol(size_dat)]

# Merge datasets:
merged_size = rbind(size_dat, old_size_dat)
merged_size = gather(merged_size, 'len_bin', 'prop', 3:ncol(merged_size))
merged_size$len_bin = as.numeric(gsub(pattern = 'l', replacement = '', x = merged_size$len_bin))
merged_size = left_join(merged_size, fleet_name_df)

# Make plot:
p2 = ggplot(data = merged_size, aes(x = len_bin, y = prop)) +
  geom_line(aes(color = type)) +
  ylab("Proportion") + xlab('Length bin (cm)') +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        legend.position = c(0.875, 0.05)) +
  scale_y_continuous(breaks = breaks_extended(3)) +
  guides(color = guide_legend(title = NULL)) +
  facet_wrap( ~ fleet_name, scales = 'free_y', ncol = 4)
ggsave(file.path(shrpoint_path, plot_dir, paste0('compare_size', img_type)), plot = p2,
       width = img_width, height = 200, units = 'mm', dpi = img_res)


# Compare 2024 (traditional) size with and without bug ----------------------------------

# 2024 size with bug:
size_dat_bug = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size-bug.csv'))
size_dat_bug = size_dat_bug %>% select(Yr, ModelFleet, L010:L198)
size_dat_bug = dplyr::rename(size_dat_bug, c(time = 'Yr', fleet_number = 'ModelFleet'))
colnames(size_dat_bug) = tolower(colnames(size_dat_bug))
size_dat_bug = size_dat_bug %>% dplyr::filter(time >= 13) # same period for both assessments
# Aggregate over time:
size_dat_bug = size_dat_bug %>% group_by(fleet_number) %>% summarise_at(.vars = colnames(.)[3:ncol(size_dat_bug)], sum)
# Sum by row to get freq:
size_dat_bug = size_dat_bug %>% ungroup() %>% mutate(across(-1)/rowSums(across(-1)))
size_dat_bug = size_dat_bug %>% mutate(type = 'With bug', .after = 'fleet_number')

# 2024 size without bug:
size_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size.csv'))
size_dat = size_dat %>% select(Yr, ModelFleet, L010:L198)
size_dat = dplyr::rename(size_dat, c(time = 'Yr', fleet_number = 'ModelFleet'))
colnames(size_dat) = tolower(colnames(size_dat))
size_dat = size_dat %>% dplyr::filter(time >= 13) # same period for both assessments
# Aggregate over time:
size_dat = size_dat %>% group_by(fleet_number) %>% summarise_at(.vars = colnames(.)[3:ncol(size_dat)], sum)
# Sum by row to get freq:
size_dat = size_dat %>% ungroup() %>% mutate(across(-1)/rowSums(across(-1)))
size_dat = size_dat %>% mutate(type = 'Without bug', .after = 'fleet_number')

# Merge datasets:
merged_size = rbind(size_dat, size_dat_bug)
merged_size = gather(merged_size, 'len_bin', 'prop', 3:ncol(merged_size))
merged_size$len_bin = as.numeric(gsub(pattern = 'l', replacement = '', x = merged_size$len_bin))
merged_size = left_join(merged_size, fleet_name_df)

# Make plot:
p2 = ggplot(data = merged_size, aes(x = len_bin, y = prop)) +
  geom_line(aes(color = type)) +
  ylab("Proportion") + xlab('Length bin (cm)') +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        legend.position = c(0.875, 0.05)) +
  scale_y_continuous(breaks = breaks_extended(3)) +
  scale_color_brewer(palette="Dark2") +
  guides(color = guide_legend(title = NULL)) +
  facet_wrap( ~ fleet_name, scales = 'free_y', ncol = 4)
ggsave(file.path(shrpoint_path, plot_dir, paste0('compare_size_bug', img_type)), plot = p2,
       width = img_width, height = 200, units = 'mm', dpi = img_res)

# Compare 2024 traditional and catch-weighted size ----------------------------------

# 2024 size catch-weighted:
size_dat_wgt = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size_LF-w_RQ-w.csv'))
size_dat_wgt = size_dat_wgt %>% select(Yr, ModelFleet, L010:L198)
size_dat_wgt = dplyr::rename(size_dat_wgt, c(time = 'Yr', fleet_number = 'ModelFleet'))
colnames(size_dat_wgt) = tolower(colnames(size_dat_wgt))
size_dat_wgt = size_dat_wgt %>% dplyr::filter(time >= 13) # same period for both assessments
# Aggregate over time:
size_dat_wgt = size_dat_wgt %>% group_by(fleet_number) %>% summarise_at(.vars = colnames(.)[3:ncol(size_dat_wgt)], sum)
# Sum by row to get freq:
size_dat_wgt = size_dat_wgt %>% ungroup() %>% mutate(across(-1)/rowSums(across(-1)))
size_dat_wgt = size_dat_wgt %>% mutate(type = 'Catch-weighted', .after = 'fleet_number')

# 2024 size without bug:
size_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size.csv'))
size_dat = size_dat %>% select(Yr, ModelFleet, L010:L198)
size_dat = dplyr::rename(size_dat, c(time = 'Yr', fleet_number = 'ModelFleet'))
colnames(size_dat) = tolower(colnames(size_dat))
size_dat = size_dat %>% dplyr::filter(time >= 13) # same period for both assessments
# Aggregate over time:
size_dat = size_dat %>% group_by(fleet_number) %>% summarise_at(.vars = colnames(.)[3:ncol(size_dat)], sum)
# Sum by row to get freq:
size_dat = size_dat %>% ungroup() %>% mutate(across(-1)/rowSums(across(-1)))
size_dat = size_dat %>% mutate(type = 'No weighting', .after = 'fleet_number')

# Merge datasets:
merged_size = rbind(size_dat, size_dat_wgt)
merged_size = gather(merged_size, 'len_bin', 'prop', 3:ncol(merged_size))
merged_size$len_bin = as.numeric(gsub(pattern = 'l', replacement = '', x = merged_size$len_bin))
merged_size = left_join(merged_size, fleet_name_df)

# Make plot:
p2 = ggplot(data = merged_size, aes(x = len_bin, y = prop)) +
  geom_line(aes(color = type)) +
  ylab("Proportion") + xlab('Length bin (cm)') +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        legend.position = c(0.875, 0.05)) +
  scale_y_continuous(breaks = breaks_extended(3)) +
  scale_color_brewer(palette="Dark2") +
  guides(color = guide_legend(title = NULL)) +
  facet_wrap( ~ fleet_name, scales = 'free_y', ncol = 4)
ggsave(file.path(shrpoint_path, plot_dir, paste0('compare_size_weight', img_type)), plot = p2,
       width = img_width, height = 200, units = 'mm', dpi = img_res)
