# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code/data_exploration', 'parameters_for_plots.R'))
source(here('code/data_exploration', 'auxiliary_functions.R'))
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
  scale_fill_viridis() +
  scale_color_viridis() +
  guides(fill = guide_legend(title = 'Catch'), color = guide_legend(title = 'Catch')) +
  theme(legend.position = 'bottom')
p1 = add_sf_map(p1)
p1 = p1 + facet_wrap(~fisherycode)
ggsave(file.path(shrpoint_path, plot_dir, paste0('map_catch_grid', img_type)), plot = p1,
       width = 170, height = 170, units = 'mm', dpi = img_res)
