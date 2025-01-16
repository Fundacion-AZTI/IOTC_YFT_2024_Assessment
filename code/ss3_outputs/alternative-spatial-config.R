rm(list = ls())

# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code', 'parameters_for_plots.R'))
source(here('code', 'auxiliary_functions.R'))

# SS configuration path (in Sharepoint):
SS_config = file.path('models/configurations')

# -------------------------------------------------------------------------
# Read 1A models

# 1A aaf
mod1Aaaf_1 = SS_output(dir = file.path(shrpoint_path, SS_config, '1A_io/aaf', '1_BC'), covar = FALSE)
mod1Aaaf_2 = SS_output(dir = file.path(shrpoint_path, SS_config, '1A_io/aaf', '2_addTag'), covar = FALSE)
mod1Aaaf_3 = SS_output(dir = file.path(shrpoint_path, SS_config, '1A_io/aaf', '3_addCAAL'), covar = FALSE)
# 1A agg
mod1Aagg_1 = SS_output(dir = file.path(shrpoint_path, SS_config, '1A_io/agg', '1_BC'), covar = FALSE)
mod1Aagg_2 = SS_output(dir = file.path(shrpoint_path, SS_config, '1A_io/agg', '2_addTag'), covar = FALSE)
mod1Aagg_3 = SS_output(dir = file.path(shrpoint_path, SS_config, '1A_io/agg', '3_addCAAL'), covar = FALSE)



# -------------------------------------------------------------------------
# SSB time series:
ts_1A = rbind(
          mod1Aaaf_1$timeseries %>% dplyr::filter(Era == 'TIME') %>% select(Area, Yr, SpawnBio) %>% mutate(spat = '1A', type = 'aaf'),
          #mod1Aaaf_2$timeseries %>% dplyr::filter(Era == 'TIME') %>% select(Area, Yr, SpawnBio) %>% mutate(spat = '1A', type = 'aaf'),
          mod1Aagg_1$timeseries %>% dplyr::filter(Era == 'TIME') %>% select(Area, Yr, SpawnBio) %>% mutate(spat = '1A', type = 'agg')
          #mod1Aagg_2$timeseries %>% dplyr::filter(Era == 'TIME') %>% select(Area, Yr, SpawnBio) %>% mutate(spat = '1A', type = 'agg')
)

ts_2A = rbind(
          mod2Aaaf_1$timeseries %>% dplyr::filter(Era == 'TIME') %>% select(Area, Yr, SpawnBio) %>% mutate(spat = '2A', type = 'aaf'),
          #mod2Aaaf_2$timeseries %>% dplyr::filter(Era == 'TIME') %>% select(Area, Yr, SpawnBio) %>% mutate(spat = '2A', type = 'aaf'),
          mod2Aagg_1$timeseries %>% dplyr::filter(Era == 'TIME') %>% select(Area, Yr, SpawnBio) %>% mutate(spat = '2A', type = 'agg')
          #mod2Aagg_2$timeseries %>% dplyr::filter(Era == 'TIME') %>% select(Area, Yr, SpawnBio) %>% mutate(spat = '2A', type = 'agg')
)
ts_4A = mod4A$timeseries %>% dplyr::filter(Era == 'TIME') %>% select(Area, Yr, SpawnBio) %>% mutate(spat = '4A')

# Plot total SSB:
plot_data = rbind(
            ts_1A %>% group_by(Yr, spat, type) %>% summarise(SpawnBio = sum(SpawnBio)) %>% mutate(Year = ssts2yq(Yr)),
            ts_2A %>% group_by(Yr, spat, type) %>% summarise(SpawnBio = sum(SpawnBio)) %>% mutate(Year = ssts2yq(Yr))
            )
plot_data_4A = ts_4A %>% group_by(Yr, spat) %>% summarise(SpawnBio = sum(SpawnBio)) %>% mutate(Year = ssts2yq(Yr))

p1 = ggplot(plot_data, aes(x = Year, y = SpawnBio, color = factor(spat))) +
        geom_line(aes(linetype = factor(type))) +
        geom_line(data = plot_data_4A, aes(x = Year, y = SpawnBio)) +
        scale_color_manual(values = mod_color) +
        xlab(NULL) + ylab('Spawning biomass (t)') +
        theme(legend.position = 'bottom') +
        guides(color = guide_legend(title = 'Spatial config'), 
               linetype = guide_legend(title = 'Aggregation type'))
ggsave(file.path(shrpoint_path, plot_dir, paste0('alt-config-ssb', img_type)), plot = p1,
       width = img_width, height = 130, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# SSB/SSBmsy time series:
ts_1A = rbind(
  mod1Aaaf_2$Kobe %>% select(Yr, B.Bmsy) %>% mutate(spat = '1A', type = 'aaf'),
  mod1Aagg_2$Kobe %>% select(Yr, B.Bmsy) %>% mutate(spat = '1A', type = 'agg')
)

ts_2A = rbind(
  mod2Aaaf_2$Kobe %>% select(Yr, B.Bmsy) %>% mutate(spat = '2A', type = 'aaf'),
  mod2Aagg_2$Kobe %>% select(Yr, B.Bmsy) %>% mutate(spat = '2A', type = 'agg')
)
ts_4A = mod4A$Kobe %>% dplyr::filter(Yr <= 308) %>% select(Yr, B.Bmsy) %>% mutate(spat = '4A')

# Plot total SSB:
plot_data = rbind(
  ts_1A %>% mutate(Year = ssts2yq(Yr)),
  ts_2A %>% mutate(Year = ssts2yq(Yr))
)
plot_data_4A = ts_4A %>% mutate(Year = ssts2yq(Yr))

p2 = ggplot(plot_data, aes(x = Year, y = B.Bmsy, color = factor(spat))) +
  geom_line(aes(linetype = factor(type))) +
  geom_line(data = plot_data_4A, aes(x = Year, y = B.Bmsy)) +
  scale_color_manual(values = mod_color) +
  xlab(NULL) + ylab('SSB/SSBmsy') +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(title = 'Spatial config'), 
         linetype = guide_legend(title = 'Aggregation type'))
ggsave(file.path(shrpoint_path, plot_dir, paste0('alt-config-status', img_type)), plot = p2,
       width = img_width, height = 130, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# Recruitment:
ts_1A = rbind(
  mod1Aaaf_2$recruit %>% select(Yr, pred_recr) %>% mutate(spat = '1A', type = 'aaf'),
  mod1Aagg_2$recruit %>% select(Yr, pred_recr) %>% mutate(spat = '1A', type = 'agg')
)

ts_2A = rbind(
  mod2Aaaf_2$recruit %>% select(Yr, pred_recr) %>% mutate(spat = '2A', type = 'aaf'),
  mod2Aagg_2$recruit %>% select(Yr, pred_recr) %>% mutate(spat = '2A', type = 'agg')
)
ts_4A = mod4A$recruit %>% dplyr::filter(Yr <= 308) %>% select(Yr, pred_recr) %>% mutate(spat = '4A')

# Plot total SSB:
plot_data = rbind(
  ts_1A %>% mutate(Year = ssts2yq(Yr)),
  ts_2A %>% mutate(Year = ssts2yq(Yr))
)
plot_data_4A = ts_4A %>% mutate(Year = ssts2yq(Yr))

p3 = ggplot(plot_data, aes(x = Year, y = pred_recr, color = factor(spat))) +
  geom_line(aes(linetype = factor(type))) +
  geom_line(data = plot_data_4A, aes(x = Year, y = pred_recr)) +
  scale_color_manual(values = mod_color) +
  xlab(NULL) + ylab('Global recruitment') +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(title = 'Spatial config'), 
         linetype = guide_legend(title = 'Aggregation type'))
ggsave(file.path(shrpoint_path, plot_dir, paste0('alt-config-recruits', img_type)), plot = p3,
       width = img_width, height = 130, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Recdevs:
ts_1A = rbind(
  mod1Aaaf_2$recruit %>% select(Yr, dev) %>% mutate(spat = '1A', type = 'aaf'),
  mod1Aagg_2$recruit %>% select(Yr, dev) %>% mutate(spat = '1A', type = 'agg')
)

ts_2A = rbind(
  mod2Aaaf_2$recruit %>% select(Yr, dev) %>% mutate(spat = '2A', type = 'aaf'),
  mod2Aagg_2$recruit %>% select(Yr, dev) %>% mutate(spat = '2A', type = 'agg')
)
ts_4A = mod4A$recruit %>% dplyr::filter(Yr <= 308) %>% select(Yr, dev) %>% mutate(spat = '4A')

# Plot total SSB:
plot_data = rbind(
  ts_1A %>% mutate(Year = ssts2yq(Yr)),
  ts_2A %>% mutate(Year = ssts2yq(Yr))
)
plot_data_4A = ts_4A %>% mutate(Year = ssts2yq(Yr))

p4 = ggplot(plot_data, aes(x = Year, y = dev, color = factor(spat))) +
  geom_line(aes(linetype = factor(type))) +
  geom_line(data = plot_data_4A, aes(x = Year, y = dev)) +
  scale_color_manual(values = mod_color) +
  xlab(NULL) + ylab('Recruitment deviates') +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(title = 'Spatial config'), 
         linetype = guide_legend(title = 'Aggregation type'))
ggsave(file.path(shrpoint_path, plot_dir, paste0('alt-config-recdevs', img_type)), plot = p4,
       width = img_width, height = 130, units = 'mm', dpi = img_res)



# -------------------------------------------------------------------------
# 2A models

# 2A aaf
mod2Aaaf_1 = SS_output(dir = file.path(shrpoint_path, SS_config, '2A_io/aaf', '2_addCAAL'), covar = FALSE)
mod2Aaaf_2 = SS_output(dir = file.path(shrpoint_path, SS_config, '2A_io/aaf', '3_addTag'), covar = FALSE)
# 2A agg
mod2Aagg_1 = SS_output(dir = file.path(shrpoint_path, SS_config, '2A_io/agg', '2_addCAAL'), covar = FALSE)
mod2Aagg_2 = SS_output(dir = file.path(shrpoint_path, SS_config, '2A_io/agg', '3_addTag'), covar = FALSE)

# 4A config:
mod4A = SS_output(dir = file.path(shrpoint_path, 'models/FinalGrid', '6_SplitCPUE_tag01_EC0_h0.8'), covar = FALSE)

# Define colors:
pal_1A = brewer.pal(n = 8, name = 'Set1')
pal_2A = brewer.pal(n = 8, name = 'Set1')

mod_color = c('1A' = pal_1A[1], 
              '2A' = pal_2A[2], 
              '4A' = 'black')

