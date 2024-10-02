rm(list = ls())

# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code', 'parameters_for_plots.R'))
source(here('code', 'auxiliary_functions.R'))

# Spatial configuration:
spat_config = '4A_io'

# Length bins
L_labels  =  c(Paste("L0",seq(10,98,2)), Paste("L",seq(100,198,2))) 

# Read fleet labels
fleet_name_df = read.csv(file.path(shrpoint_path, tab_dir, paste0('fleet_label_', spat_config,'.csv')))

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
cpue_dat = read.csv(file.path(shrpoint_path, 'data/raw/indices/PS FSC/2024-cpue-standardization-iotc-yft-fsc.quarter-indices.just-essentials.csv'), sep = ';')
cpue_dat = cpue_dat %>% mutate(time = year + (quarter-1)/4)
cpue_dat = cpue_dat %>% dplyr::rename(obs = yft_adult_rate_Mean)
cpue_dat = cpue_dat %>% mutate(sd = obs*yft_adult_rate_cv)
plot_data = cpue_dat

p1 = ggplot(data = plot_data, aes(x = time, y = obs)) +
  geom_ribbon(aes(ymin = obs - sd, ymax = obs + sd), fill = 'grey70') +
  geom_line(aes(y = obs)) +
  coord_cartesian(ylim = c(0, NA), expand = FALSE) + 
  scale_y_continuous(n.breaks = 3) +
  xlab(NULL) + ylab("Scaled free school PS CPUE")

# FOB CPUE:
cpue_dat = read.csv(file.path(shrpoint_path, 'data/raw/indices/PSLS/st-GLMM_FOB.csv'))
cpue_dat = cpue_dat %>% mutate(time = Time)
cpue_dat = cpue_dat %>% dplyr::rename(obs = Est)
cpue_dat = cpue_dat %>% mutate(sd = SE)
plot_data = cpue_dat

p2 = ggplot(data = plot_data, aes(x = time, y = obs)) +
  geom_ribbon(aes(ymin = obs - sd, ymax = obs + sd), fill = 'grey70') +
  geom_line(aes(y = obs)) +
  coord_cartesian(ylim = c(0, NA), expand = FALSE) + 
  scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(breaks = seq(from = 2010, to = 2022, by= 2)) +
  xlab(NULL) + ylab("Scaled FOB PS CPUE")

# Merge plots:
p3 = grid.arrange(p1, p2, nrow =2)

ggsave(file.path(shrpoint_path, plot_dir, paste0('ts_ps_cpue', img_type)), plot = p3,
       width = img_width*0.5, height = 130, units = 'mm', dpi = img_res)

