# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code', 'parameters_for_plots.R'))
source(here('code', 'auxiliary_functions.R'))
theme_set(theme_classic())

# Read grid data:
load(file.path(shrpoint_path, 'data/processed', 'stdGrid.RData'))
load(file.path(shrpoint_path, 'data/processed', 'catchStd.RData'))
load(file.path(shrpoint_path, 'data/processed', 'sizeStd.RData'))


# Make grid map catch per fleet (aggregated over time) --------------------

plot_data = catchStd %>% group_by(grid_ID, fisherycode) %>% summarise(catch = sum(ncmtfish)*1e-03)
plot_data = left_join(stdGrid, plot_data, by = 'grid_ID') %>% na.omit

p1 = ggplot() +
  geom_sf(data = plot_data, aes(fill = catch, color = catch)) +
  scale_color_viridis() +
  scale_fill_viridis() +
  guides(fill = guide_colourbar(title = 'Catch (thousands of tons)',
                                theme = theme(legend.key.width  = unit(5, "cm"))),
         color = guide_colourbar(title = 'Catch (thousands of tons)',
                                 theme = theme(legend.key.width  = unit(5, "cm")))) + 
  theme(legend.position = 'bottom')
p1 = add_sf_map(p1)
p1 = p1 + facet_wrap(~fisherycode)
ggsave(file.path(shrpoint_path, plot_dir, paste0('map_catch_grid', img_type)), plot = p1,
       width = img_width, height = 160, units = 'mm', dpi = img_res)

# Make grid map size sampling frequency per fleet (aggregated over time) --------------------

plot_data = sizeStd %>% group_by(grid_ID, fisherycode) %>% summarise(Freq = n())
plot_data = left_join(stdGrid, plot_data, by = 'grid_ID') %>% na.omit

p1 = ggplot() +
  geom_sf(data = plot_data, aes(fill = Freq, color = Freq)) +
  scale_color_viridis() +
  scale_fill_viridis() +
  guides(fill = guide_colourbar(title = 'Size sampling frequency',
                                theme = theme(legend.key.width  = unit(5, "cm"))),
         color = guide_colourbar(title = 'Size sampling frequency',
                                 theme = theme(legend.key.width  = unit(5, "cm")))) + 
  theme(legend.position = 'bottom')
p1 = add_sf_map(p1)
p1 = p1 + facet_wrap(~fisherycode)
ggsave(file.path(shrpoint_path, plot_dir, paste0('map_size_freq_grid', img_type)), plot = p1,
       width = img_width, height = 160, units = 'mm', dpi = img_res)


# Make grid map size reporting quality per fleet (aggregated over time) --------------------

plot_data = sizeStd %>% group_by(grid_ID, fisherycode) %>% summarise(reporting_quality = mean(reporting_quality))
plot_data = left_join(stdGrid, plot_data, by = 'grid_ID') %>% na.omit

p1 = ggplot() +
  geom_sf(data = plot_data, aes(fill = reporting_quality, color = reporting_quality)) +
  scale_color_viridis() +
  scale_fill_viridis() +
  guides(fill = guide_colourbar(title = 'Size reporting quality score',
                                theme = theme(legend.key.width  = unit(5, "cm"))),
         color = guide_colourbar(title = 'Size reporting quality score',
                                 theme = theme(legend.key.width  = unit(5, "cm")))) + 
  theme(legend.position = 'bottom')
p1 = add_sf_map(p1)
p1 = p1 + facet_wrap(~fisherycode)
ggsave(file.path(shrpoint_path, plot_dir, paste0('map_size_repquality_grid', img_type)), plot = p1,
       width = img_width, height = 160, units = 'mm', dpi = img_res)


