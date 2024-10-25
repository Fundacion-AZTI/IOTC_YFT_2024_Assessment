rm(list = ls())

# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code', 'parameters_for_plots.R'))
source(here('code', 'auxiliary_functions.R'))

# Spatial configuration:
spat_config = '4A_io'

# Length bins
L_labels  =  c(Paste("L0",seq(10,98,2)), Paste("L",seq(100,198,2))) 

# -------------------------------------------------------------------------
# Scale LL CPUE per time step with CV -------------------------------------

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
  coord_cartesian(ylim = c(0, 5), expand = FALSE) + 
  xlab(NULL) + ylab("Scaled LL CPUE") +
  facet_wrap( ~ area)
ggsave(file.path(shrpoint_path, plot_dir, paste0('ts_cpue_area', img_type)), plot = p1,
       width = img_width, height = 130, units = 'mm', dpi = img_res)

# Free school and FOB PS CPUE --------------------------------------------------

# Free school CPUE:
cpue_dat = read.csv(file.path(shrpoint_path, 'data/raw/YFT/PS FSC/2024-cpue-standardization-iotc-yft-fsc.quarter-indices.just-essentials.csv'), sep = ';')
cpue_dat = cpue_dat %>% mutate(time = year + (quarter-1)/4)
cpue_dat = cpue_dat %>% dplyr::rename(obs = yft_adult_rate_Mean)
cpue_dat = cpue_dat %>% mutate(sd = obs*yft_adult_rate_cv)
plot_data = cpue_dat

p1 = ggplot(data = plot_data, aes(x = time, y = obs)) +
  geom_ribbon(aes(ymin = obs - sd, ymax = obs + sd), fill = 'grey70') +
  geom_line(aes(y = obs)) +
  coord_cartesian(ylim = c(0, NA), xlim = c(1991, 2023), expand = FALSE) + 
  scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = seq(from = 1990, to = 2020, by= 5)) +
  xlab(NULL) + ylab("FS CPUE")

# FOB CPUE:
cpue_dat = read.csv(file.path(shrpoint_path, 'data/raw/YFT/PSLS/st-GLMM_FOB.csv'))
cpue_dat = cpue_dat %>% mutate(time = Time)
cpue_dat = cpue_dat %>% dplyr::rename(obs = Est)
cpue_dat = cpue_dat %>% mutate(sd = SE)
plot_data = cpue_dat

p2 = ggplot(data = plot_data, aes(x = time, y = obs)) +
  geom_ribbon(aes(ymin = obs - sd, ymax = obs + sd), fill = 'grey70') +
  geom_line(aes(y = obs)) +
  coord_cartesian(ylim = c(0, NA), xlim = c(1991, 2023), expand = FALSE) + 
  scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = seq(from = 1990, to = 2020, by= 5)) +
  xlab(NULL) + ylab("LS CPUE")

# Merge plots:
p3 = grid.arrange(p1, p2, nrow =2)

ggsave(file.path(shrpoint_path, plot_dir, paste0('ts_ps_cpue', img_type)), plot = p3,
       width = img_width*0.5, height = 130, units = 'mm', dpi = img_res)


# Compare Free school and LL CPUE --------------------------------------------------

# LL CPUE:
cpue_ll = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'scaled_cpue_Meancv_02.csv'))
cpue_ll = cpue_ll %>% dplyr::filter(fleet == 22) %>% mutate(area = '1b') %>% mutate(time = ssts2yq(qtr)) %>% 
              dplyr::rename(obs = pr_7994_m8) %>% mutate(sd = obs*cv, type = 'LL CPUE')
cpue_ll$obs = cpue_ll$obs/mean(cpue_ll$obs) # rescale

# Free school CPUE:
cpue_fs = read.csv(file.path(shrpoint_path, 'data/raw/YFT/PS FSC/2024-cpue-standardization-iotc-yft-fsc.quarter-indices.just-essentials.csv'), sep = ';')
cpue_fs = cpue_fs %>% mutate(time = year + (quarter-1)/4) %>% dplyr::rename(obs = yft_adult_rate_Mean) %>% 
                mutate(sd = obs*yft_adult_rate_cv, type = 'FS CPUE')
cpue_fs$obs = cpue_fs$obs/mean(cpue_fs$obs) # rescale

# Merge:
plot_data = rbind(cpue_ll[,c('time', 'obs', 'sd', 'type')], cpue_fs[,c('time', 'obs', 'sd', 'type')])

p1 = ggplot(data = plot_data, aes(x = time, y = obs)) +
  geom_line(aes(color = type)) +
  coord_cartesian(ylim = c(0, 4), expand = FALSE) + 
  scale_y_continuous(n.breaks = 3) +
  xlab(NULL) + ylab("Scaled CPUE") +
  theme(legend.position = c(0.8, 0.85)) +
  guides(color = guide_legend(title = NULL)) 

ggsave(file.path(shrpoint_path, plot_dir, paste0('ts_llfs_cpue', img_type)), plot = p1,
       width = img_width*0.5, height = 80, units = 'mm', dpi = img_res)

# Compare LL CPUE information ---------------------------------------------
# Read 2021 SS data inputs
base_dat = SS_readdat(file = file.path(shrpoint_path, 'models/base', spat_config, 'data.ss'))

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

# Compare LL CPUE with effort creep ---------------------------------------------

# 2024 CPUE:
cpue_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'scaled_cpue.csv'))
cpue_dat = cpue_dat[,c('qtr', 'fleet', 'pr_7994_m8', 'cv')]
colnames(cpue_dat) = c('time', 'fleet_number', 'obs', 'cv')
cpue_dat = cpue_dat %>% mutate(type = 'No effort creep')

# effort creep applied:
cpue_dat_effcreep = apply_eff_creep(cpue_dat, yr_col = 'time', fleet_col = 'fleet_number',
                                    cpue_col = 'obs', cv_col = 'cv', rate = 0.005)
cpue_dat_effcreep = cpue_dat_effcreep %>% mutate(type = 'Effort creep: 0.5%')

# Merge datasets:
merged_cpue = rbind(cpue_dat, cpue_dat_effcreep)
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
ggsave(file.path(shrpoint_path, plot_dir, paste0('compare_cpue_effcreep_1prc', img_type)), plot = p1,
       width = img_width, height = 170, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------

# Compare yearly vs quartely LL indices

# Read data:
dat_qt1 = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-ll-qt.csv'))
dat_qt2 = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-ll-qt-seas1.csv'))
dat_qt3 = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-ll-qt-avgyr.csv'))
dat_yr1 = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-ll-yr.csv'))
dat_yr2 = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-ll-yr-constant.csv'))
dat_yr3 = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'cpue-ll-yr-seasonal.csv'))

# Merge:
plot_data = rbind(dat_qt1 %>% mutate(type = 'Qt', tstep = 'In model: every quarter'), 
                  dat_qt2 %>% mutate(type = 'Qt-season1', tstep = 'In model: only quarter 1'),
                  dat_qt3 %>% mutate(type = 'Qt-averageYr', tstep = 'In model: only quarter 1'),
                  dat_yr1 %>% mutate(type = 'Yr', tstep = 'In model: only quarter 1'), 
                  dat_yr2 %>% mutate(type = 'Yr-constant', tstep = 'In model: every quarter'),
                  dat_yr3 %>% mutate(type = 'Yr-seasonal', tstep = 'In model: every quarter'))
plot_data = plot_data %>% mutate(fleet_name = if_else(index == 22, 'LL 1b', 
                                                      if_else(index == 23, 'LL 2', 
                                                                  if_else(index == 24, 'LL 3', 'LL 4'))))
plot_data$time = ssts2yq(plot_data$year) # transform to yr-qt

# Make plot:
p1 = ggplot(data = plot_data, aes(x = time, y = obs)) +
  geom_line(aes(color = type)) +
  ylab("Scaled CPUE") + xlab(NULL) +
  scale_color_brewer(palette = "Dark2") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        legend.position = 'bottom') +
  scale_y_continuous(breaks = breaks_extended(3)) +
  guides(color = guide_legend(title = NULL, nrow = 1)) +
  facet_grid(fleet_name ~ tstep, scales = 'free_y')
ggsave(file.path(shrpoint_path, plot_dir, paste0('compare_cpue_treatment', img_type)), plot = p1,
       width = img_width*0.8, height = 150, units = 'mm', dpi = img_res)

