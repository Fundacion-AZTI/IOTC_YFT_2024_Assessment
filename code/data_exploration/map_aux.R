# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code', 'parameters_for_plots.R'))
source(here('code', 'auxiliary_functions.R'))
theme_set(theme_classic())

# Make IO map with 4A model areas -----------------------------------------

reg_lines_df1 = rbind(data.frame(lon1 = 75, lat1 = 20, lon2 = 75, lat2 = -15), # vertical 
                       data.frame(lon1 = 60, lat1 = -10, lon2 = 60, lat2 = -30), # vertical 
                       data.frame(lon1 = 40, lat1 = -30, lon2 = 40, lat2 = -40), # vertical 
                       data.frame(lon1 = 30, lat1 = -10, lon2 = 60, lat2 = -10), # horizontal 
                       data.frame(lon1 = 60, lat1 = -15, lon2 = 120, lat2 = -15), # horizontal 
                       data.frame(lon1 = 40, lat1 = -30, lon2 = 60, lat2 = -30) # horizontal 
                       )
reg_lines_df2 = data.frame(lon1 = 40, lat1 = 10, lon2 = 75, lat2 = 10) # 1a and 1b

p1 = ggplot() +
  geom_segment(data = reg_lines_df1, aes(x = lon1, y = lat1, xend = lon2, yend = lat2)) +
  geom_segment(data = reg_lines_df2, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), linetype = 2) +
  annotate('text', x = 63, y = 18, label = '1a', size = 7) +
  annotate('text', x = 60, y = 0, label = '1b', size = 7) +
  annotate('text', x = 90, y = 0, label = '4', size = 7) +
  annotate('text', x = 85, y = -25, label = '3', size = 7) +
  annotate('text', x = 38, y = -25, label = '2', size = 7)
p1 = add_sf_map(p1)
ggsave(file.path(shrpoint_path, plot_dir, paste0('map_IO_4ARegions', img_type)), plot = p1,
       width = 170, height = 130, units = 'mm', dpi = img_res)

# Make IO map with 2A model areas -----------------------------------------

reg_lines_df1 = rbind(data.frame(lon1 = 75, lat1 = 20, lon2 = 75, lat2 = -15), # vertical 
                      data.frame(lon1 = 60, lat1 = -15, lon2 = 60, lat2 = -30), # vertical 
                      data.frame(lon1 = 40, lat1 = -30, lon2 = 40, lat2 = -40), # vertical 
                      data.frame(lon1 = 60, lat1 = -15, lon2 = 75, lat2 = -15), # horizontal 
                      data.frame(lon1 = 40, lat1 = -30, lon2 = 60, lat2 = -30) # horizontal 
)
reg_lines_df2 = data.frame(lon1 = 40, lat1 = 10, lon2 = 75, lat2 = 10) # 1a and 1b

p1 = ggplot() +
  geom_segment(data = reg_lines_df1, aes(x = lon1, y = lat1, xend = lon2, yend = lat2)) +
  geom_segment(data = reg_lines_df2, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), linetype = 2) +
  annotate('text', x = 63, y = 18, label = '1a', size = 7) +
  annotate('text', x = 50, y = -10, label = '1b', size = 7) +
  annotate('text', x = 90, y = -10, label = '2', size = 7) 
p1 = add_sf_map(p1)
ggsave(file.path(shrpoint_path, plot_dir, paste0('map_IO_2ARegions', img_type)), plot = p1,
       width = 170, height = 130, units = 'mm', dpi = img_res)

# Make IO map with 1A model areas -----------------------------------------

reg_lines_df2 = data.frame(lon1 = 40, lat1 = 10, lon2 = 75, lat2 = 10) # 1a and 1b

p1 = ggplot() +
  geom_segment(data = reg_lines_df2, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), linetype = 2) +
  annotate('text', x = 63, y = 18, label = '1a', size = 7) +
  annotate('text', x = 75, y = -10, label = '1b', size = 7) 
p1 = add_sf_map(p1)
ggsave(file.path(shrpoint_path, plot_dir, paste0('map_IO_1ARegions', img_type)), plot = p1,
       width = 170, height = 130, units = 'mm', dpi = img_res)


