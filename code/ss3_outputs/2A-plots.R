rm(list = ls())

# Read path and parameters for plots:
source('sharepoint_path.R')
source('code/parameters_for_plots.R')
source('code/auxiliary_functions.R')

# SS configuration path (in Sharepoint):
SS_config = file.path('models/configurations')

# -------------------------------------------------------------------------
# Read 1A models

# 2A aaf
mod1Aaaf_1 = SS_output(dir = file.path(shrpoint_path, SS_config, '2A_io/aaf', '1_BC'), covar = FALSE)
mod1Aaaf_2 = SS_output(dir = file.path(shrpoint_path, SS_config, '2A_io/aaf', '2_addTag'), covar = FALSE)
mod1Aaaf_3 = SS_output(dir = file.path(shrpoint_path, SS_config, '2A_io/aaf', '3_addCAAL'), covar = FALSE)
# 2A agg
mod1Aagg_1 = SS_output(dir = file.path(shrpoint_path, SS_config, '2A_io/agg', '1_BC'), covar = FALSE)
mod1Aagg_2 = SS_output(dir = file.path(shrpoint_path, SS_config, '2A_io/agg', '2_addTag'), covar = FALSE)
mod1Aagg_3 = SS_output(dir = file.path(shrpoint_path, SS_config, '2A_io/agg', '3_addCAAL'), covar = FALSE)

# 4A config:
mod4A = SS_output(dir = file.path(shrpoint_path, 'models/FinalGrid', '6_SplitCPUE_tag01_EC0_h0.8'), covar = FALSE)

# -------------------------------------------------------------------------
# SSB time series:
ts_2A = rbind(
          mod1Aaaf_1$timeseries %>% dplyr::filter(Era == 'TIME') %>% select(Area, Yr, SpawnBio) %>% group_by(Yr) %>% summarise(SpawnBio = sum(SpawnBio)) %>% mutate(dat = 'base', type = 'aaf'),
          mod1Aaaf_2$timeseries %>% dplyr::filter(Era == 'TIME') %>% select(Area, Yr, SpawnBio) %>% group_by(Yr) %>% summarise(SpawnBio = sum(SpawnBio)) %>% mutate(dat = 'addTag', type = 'aaf'),
          mod1Aaaf_3$timeseries %>% dplyr::filter(Era == 'TIME') %>% select(Area, Yr, SpawnBio) %>% group_by(Yr) %>% summarise(SpawnBio = sum(SpawnBio)) %>% mutate(dat = 'addCAAL', type = 'aaf'),
          mod1Aagg_1$timeseries %>% dplyr::filter(Era == 'TIME') %>% select(Area, Yr, SpawnBio) %>% group_by(Yr) %>% summarise(SpawnBio = sum(SpawnBio)) %>% mutate(dat = 'base', type = 'agg'),
          mod1Aagg_2$timeseries %>% dplyr::filter(Era == 'TIME') %>% select(Area, Yr, SpawnBio) %>% group_by(Yr) %>% summarise(SpawnBio = sum(SpawnBio)) %>% mutate(dat = 'addTag', type = 'agg'),
          mod1Aagg_3$timeseries %>% dplyr::filter(Era == 'TIME') %>% select(Area, Yr, SpawnBio) %>% group_by(Yr) %>% summarise(SpawnBio = sum(SpawnBio)) %>% mutate(dat = 'addCAAL', type = 'agg')
)
ts_4A = mod4A$timeseries %>% dplyr::filter(Era == 'TIME') %>% select(Area, Yr, SpawnBio) %>% group_by(Yr) %>% summarise(SpawnBio = sum(SpawnBio))

# Plot total SSB:
plot_data = ts_2A %>% mutate(Year = ssts2yq(Yr))
plot_data = plot_data %>% mutate(dat = factor(dat, levels = c('base', 'addTag', 'addCAAL')), type = factor(type, levels = c('agg', 'aaf')))
plot_data_4A = ts_4A %>% mutate(Year = ssts2yq(Yr))

p1 = ggplot(plot_data, aes(x = Year, y = SpawnBio, color = factor(dat))) +
        geom_line() +
        geom_line(data = plot_data_4A, color = 'black') +
        scale_color_brewer(palette = 'Dark2') +
        xlab(NULL) + ylab('Spawning biomass (t)') +
        theme(legend.position = 'bottom') +
        coord_cartesian(xlim = c(1950, 2023)) +
        guides(color = guide_legend(title = 'Data types in model:')) +
        facet_wrap(~ type)
ggsave(file.path(shrpoint_path, plot_dir, paste0('ts_SSB_2A', img_type)), plot = p1,
       width = img_width, height = 110, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# SSB/SSBmsy time series:
ts_1A = rbind(
  mod1Aaaf_1$Kobe %>% select(Yr, B.Bmsy) %>% mutate(dat = 'base', type = 'aaf'),
  mod1Aaaf_2$Kobe %>% select(Yr, B.Bmsy) %>% mutate(dat = 'addTag', type = 'aaf'),
  mod1Aaaf_3$Kobe %>% select(Yr, B.Bmsy) %>% mutate(dat = 'addCAAL', type = 'aaf'),
  mod1Aagg_1$Kobe %>% select(Yr, B.Bmsy) %>% mutate(dat = 'base', type = 'agg'),
  mod1Aagg_2$Kobe %>% select(Yr, B.Bmsy) %>% mutate(dat = 'addTag', type = 'agg'),
  mod1Aagg_3$Kobe %>% select(Yr, B.Bmsy) %>% mutate(dat = 'addCAAL', type = 'agg')
)
ts_4A = mod4A$Kobe %>% dplyr::filter(Yr <= 308) %>% select(Yr, B.Bmsy)

# Plot status:
plot_data = ts_1A %>% mutate(Year = ssts2yq(Yr))
plot_data = plot_data %>% mutate(dat = factor(dat, levels = c('base', 'addTag', 'addCAAL')), type = factor(type, levels = c('agg', 'aaf')))
plot_data_4A = ts_4A %>% mutate(Year = ssts2yq(Yr))

p2 = ggplot(plot_data, aes(x = Year, y = B.Bmsy, color = factor(dat))) +
  geom_line() +
  geom_line(data = plot_data_4A, color = 'black') +
  scale_color_brewer(palette = 'Dark2') +
  xlab(NULL) + ylab('SSB/SSBmsy') +
  theme(legend.position = 'bottom') +
  coord_cartesian(xlim = c(1950, 2023)) +
  guides(color = guide_legend(title = 'Data types in model:')) +
  facet_wrap(~ type)
ggsave(file.path(shrpoint_path, plot_dir, paste0('ts_status_2A', img_type)), plot = p2,
       width = img_width, height = 110, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# Recdevs:
ts_1A = rbind(
  mod1Aaaf_1$recruit %>% select(Yr, dev) %>% mutate(dat = 'base', type = 'aaf'),
  mod1Aaaf_2$recruit %>% select(Yr, dev) %>% mutate(dat = 'addTag', type = 'aaf'),
  mod1Aaaf_3$recruit %>% select(Yr, dev) %>% mutate(dat = 'addCAAL', type = 'aaf'),
  mod1Aagg_1$recruit %>% select(Yr, dev) %>% mutate(dat = 'base', type = 'agg'),
  mod1Aagg_2$recruit %>% select(Yr, dev) %>% mutate(dat = 'addTag', type = 'agg'),
  mod1Aagg_3$recruit %>% select(Yr, dev) %>% mutate(dat = 'addCAAL', type = 'agg')
)
ts_4A = mod4A$recruit %>% dplyr::filter(Yr <= 308) %>% select(Yr, dev) 

# Plot total SSB:
plot_data = ts_1A %>% mutate(Year = ssts2yq(Yr))
plot_data = plot_data %>% mutate(dat = factor(dat, levels = c('base', 'addTag', 'addCAAL')), type = factor(type, levels = c('agg', 'aaf')))
plot_data_4A = ts_4A %>% mutate(Year = ssts2yq(Yr))

p3 = ggplot(plot_data, aes(x = Year, y = dev, color = dat)) +
  geom_line() +
  geom_smooth(method = "loess", color = 'black', se = FALSE) +
  # geom_line(data = plot_data_4A, color = 'black', alpha = 0.5) +
  scale_color_brewer(palette = 'Dark2') +
  xlab(NULL) + ylab('Recruitment deviates') +
  theme(legend.position = 'none') +
  coord_cartesian(xlim = c(1950, 2023)) +
  facet_grid(dat ~ type)
ggsave(file.path(shrpoint_path, plot_dir, paste0('ts_recdev_2A', img_type)), plot = p3,
       width = img_width, height = 190, units = 'mm', dpi = img_res)
