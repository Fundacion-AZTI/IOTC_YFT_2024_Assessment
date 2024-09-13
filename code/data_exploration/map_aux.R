# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code', 'parameters_for_plots.R'))
source(here('code', 'auxiliary_functions.R'))
theme_set(theme_classic())
data_folder = 'data/processed'


# Make IO map with 4A model areas -----------------------------------------

p1 = ggplot() +
  geom_segment(data = reg_lines_4A, aes(x = lon1, y = lat1, xend = lon2, yend = lat2)) +
  geom_segment(data = reg_lines_1ab, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), linetype = 2) +
  annotate('text', x = 63, y = 18, label = '1a', size = 7) +
  annotate('text', x = 60, y = 0, label = '1b', size = 7) +
  annotate('text', x = 90, y = 0, label = '4', size = 7) +
  annotate('text', x = 85, y = -25, label = '3', size = 7) +
  annotate('text', x = 38, y = -25, label = '2', size = 7) 
p1 = add_sf_map(p1)
ggsave(file.path(shrpoint_path, plot_dir, paste0('map_IO_4ARegions', img_type)), plot = p1,
       width = img_width, height = 130, units = 'mm', dpi = img_res)

# Make IO map with 2A model areas -----------------------------------------

p1 = ggplot() +
  geom_segment(data = reg_lines_2A, aes(x = lon1, y = lat1, xend = lon2, yend = lat2)) +
  geom_segment(data = reg_lines_1ab, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), linetype = 2) +
  annotate('text', x = 63, y = 18, label = '1a', size = 7) +
  annotate('text', x = 50, y = -10, label = '1b', size = 7) +
  annotate('text', x = 90, y = -10, label = '2', size = 7) 
p1 = add_sf_map(p1)
ggsave(file.path(shrpoint_path, plot_dir, paste0('map_IO_2ARegions', img_type)), plot = p1,
       width = img_width, height = 130, units = 'mm', dpi = img_res)

# Make IO map with 1A model areas -----------------------------------------

p1 = ggplot() +
  geom_segment(data = reg_lines_1ab, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), linetype = 2) +
  annotate('text', x = 63, y = 18, label = '1a', size = 7) +
  annotate('text', x = 75, y = -10, label = '1b', size = 7) 
p1 = add_sf_map(p1)
ggsave(file.path(shrpoint_path, plot_dir, paste0('map_IO_1ARegions', img_type)), plot = p1,
       width = img_width, height = 130, units = 'mm', dpi = img_res)


# Make IO map with size grid types ----------------------------------------

# Grid type = 1:
grid_type = 1
grid_size_lon = get_grid_lon_dim(grid_type)
grid_size_lat = get_grid_lat_dim(grid_type)
center_df = data.frame(lat = 15, long = 75) %>% st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
grid_t1 = st_make_grid(center_df, cellsize = c(grid_size_lon, grid_size_lat), 
                       offset = c(center_df$long - grid_size_lon*0.5, center_df$lat - grid_size_lat*0.5)) %>%
  st_set_crs(4326) %>% st_sf() %>% dplyr::mutate(grid_ID = 1:n(), grid_type = grid_type)
# Grid type = 2:
grid_type = 2
grid_size_lon = get_grid_lon_dim(grid_type)
grid_size_lat = get_grid_lat_dim(grid_type)
center_df = data.frame(lat = -5, long = 90) %>% st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
grid_t2 = st_make_grid(center_df, cellsize = c(grid_size_lon, grid_size_lat), 
                       offset = c(center_df$long - grid_size_lon*0.5, center_df$lat - grid_size_lat*0.5)) %>%
  st_set_crs(4326) %>% st_sf() %>% dplyr::mutate(grid_ID = 1:n(), grid_type = grid_type)
# Grid type = 3:
grid_type = 3
grid_size_lon = get_grid_lon_dim(grid_type)
grid_size_lat = get_grid_lat_dim(grid_type)
center_df = data.frame(lat = -5, long = 55) %>% st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
grid_t3 = st_make_grid(center_df, cellsize = c(grid_size_lon, grid_size_lat), 
                       offset = c(center_df$long - grid_size_lon*0.5, center_df$lat - grid_size_lat*0.5)) %>%
  st_set_crs(4326) %>% st_sf() %>% dplyr::mutate(grid_ID = 1:n(), grid_type = grid_type)
# Grid type = 4:
grid_type = 4
grid_size_lon = get_grid_lon_dim(grid_type)
grid_size_lat = get_grid_lat_dim(grid_type)
center_df = data.frame(lat = -30, long = 110) %>% st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
grid_t4 = st_make_grid(center_df, cellsize = c(grid_size_lon, grid_size_lat), 
                       offset = c(center_df$long - grid_size_lon*0.5, center_df$lat - grid_size_lat*0.5)) %>%
  st_set_crs(4326) %>% st_sf() %>% dplyr::mutate(grid_ID = 1:n(), grid_type = grid_type)
# Grid type = 5:
grid_type = 5
grid_size_lon = get_grid_lon_dim(grid_type)
grid_size_lat = get_grid_lat_dim(grid_type)
center_df = expand.grid(lat = seq(from = -19.5, to = -15.5, by = 1), long = seq(from = 50.5, to = 54.5, by = 1)) %>% 
                          st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
grid_t5 = st_make_grid(center_df, cellsize = c(grid_size_lon, grid_size_lat), 
                       offset = c(min(center_df$long) - grid_size_lon*0.5, min(center_df$lat) - grid_size_lat*0.5)) %>%
  st_set_crs(4326) %>% st_sf() %>% dplyr::mutate(grid_ID = 1:n(), grid_type = grid_type)
# Grid type = 6:
grid_type = 6
grid_size_lon = get_grid_lon_dim(grid_type)
grid_size_lat = get_grid_lat_dim(grid_type)
center_df = data.frame(lat = -17.5, long = 77.5) %>% st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
grid_t6 = st_make_grid(center_df, cellsize = c(grid_size_lon, grid_size_lat), 
                       offset = c(center_df$long - grid_size_lon*0.5, center_df$lat - grid_size_lat*0.5)) %>%
  st_set_crs(4326) %>% st_sf() %>% dplyr::mutate(grid_ID = 1:n(), grid_type = grid_type)

# Merge grid and get centroid:
grid_borders = rbind(grid_t1,grid_t2,grid_t3,grid_t4,grid_t5,grid_t6)
grid_centers = st_centroid(grid_borders) %>% dplyr::mutate(long = sf::st_coordinates(.)[,1], lat = sf::st_coordinates(.)[,2])

# Make plot:
p2 = ggplot(data = worldmap) +
  geom_sf(fill = "gray60", color = "gray60") +
  geom_sf(data = grid_borders, fill = 'blue', color = 'white', alpha = 0.3) +
  geom_sf(data = grid_centers, color = 'red', shape = 4, size = 2) +
  coord_sf(expand = FALSE, xlim = xLim, ylim = yLim) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(breaks = yBreaks) + scale_x_continuous(breaks = xBreaks)
ggsave(file.path(shrpoint_path, plot_dir, paste0('map_size_grid', img_type)), plot = p2,
       width = img_width, height = 130, units = 'mm', dpi = img_res)


# Make IO map with STD size grids ----------------------------------------

# Use grid DF created in previous chunk:
load(file.path(shrpoint_path, data_folder, 'stdGrid_5.RData'))
# Make STD grid centers:
grid_centers_tmp = grid_centers %>% mutate(grid_type = as.character(grid_type), grid_ID = 1:n())
st_geometry(grid_centers_tmp) = NULL
grid_centers_tmp = grid_centers_tmp %>% group_split(grid_ID) %>% 
  purrr::map(~ transform_to_stdgrid(.x)) %>% 
  list_rbind() %>% st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
  
# Filter intersect grids:
grid_borders_std = stdGrid[unique(unlist(st_intersects(grid_centers_tmp, stdGrid))), ]
grid_borders_std = grid_borders_std %>% dplyr::filter(portion_on_land < 0.999) # filter portion on land
grid_centers_std = st_centroid(grid_borders_std) 

# Make plot:
p2 = ggplot(data = worldmap) +
  geom_sf(fill = "gray60", color = "gray60") +
  geom_sf(data = grid_borders_std, fill = 'blue', color = 'white', alpha = 0.3) +
  geom_sf(data = grid_centers_std, color = 'red', shape = 4, size = 2) +
  coord_sf(expand = FALSE, xlim = xLim, ylim = yLim) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(breaks = yBreaks) + scale_x_continuous(breaks = xBreaks)
ggsave(file.path(shrpoint_path, plot_dir, paste0('map_size_grid_std', img_type)), plot = p2,
       width = img_width, height = 130, units = 'mm', dpi = img_res)


# Add 4A lines to raw size grid -----------------------------------------------

p3 = ggplot() +
  geom_sf(data = worldmap, fill = "gray60", color = "gray60") +
  geom_sf(data = grid_borders, fill = 'blue', color = 'white', alpha = 0.3) +
  geom_sf(data = grid_centers, color = 'red', shape = 4, size = 2) +
  geom_segment(data = reg_lines_4A, aes(x = lon1, y = lat1, xend = lon2, yend = lat2)) +
  geom_segment(data = reg_lines_1ab, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), linetype = 2) +
  coord_sf(expand = FALSE, xlim = xLim, ylim = yLim) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(breaks = yBreaks) + scale_x_continuous(breaks = xBreaks)
ggsave(file.path(shrpoint_path, plot_dir, paste0('map_size_grid_4A', img_type)), plot = p3,
       width = img_width, height = 130, units = 'mm', dpi = img_res)


# Add 4A lines to STD size grid -----------------------------------------------

p4 = ggplot() +
  geom_sf(data = worldmap, fill = "gray60", color = "gray60") +
  geom_sf(data = grid_borders_std, fill = 'blue', col = 'white', alpha = 0.3) +
  geom_sf(data = grid_centers_std, color = 'red', shape = 4, size = 2) +
  geom_segment(data = reg_lines_4A, aes(x = lon1, y = lat1, xend = lon2, yend = lat2)) +
  geom_segment(data = reg_lines_1ab, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), linetype = 2) +
  coord_sf(expand = FALSE, xlim = xLim, ylim = yLim) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(breaks = yBreaks) + scale_x_continuous(breaks = xBreaks)
ggsave(file.path(shrpoint_path, plot_dir, paste0('map_size_grid_std_4A', img_type)), plot = p4,
       width = img_width, height = 130, units = 'mm', dpi = img_res)


# Make IO map with catch grid types ----------------------------------------

# Grid type = 6:
grid_type = 6
grid_size_lon = get_grid_lon_dim(grid_type)
grid_size_lat = get_grid_lat_dim(grid_type)
center_df = data.frame(lat = c(-17.5, 2.5, -7.5), long = c(77.5, 62.5, 47.5)) %>% 
              st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
grid_borders = st_make_grid(center_df, cellsize = c(grid_size_lon, grid_size_lat), 
                            offset = c(min(center_df$long) - grid_size_lon*0.5, min(center_df$lat) - grid_size_lat*0.5)) %>%
               st_set_crs(4326) %>% st_sf() %>% dplyr::mutate(grid_ID = 1:n())
# Filter intersect grids:
grid_borders = grid_borders[unlist(st_intersects(center_df, grid_borders)), ]
# Get centroid:
grid_centers = st_centroid(grid_borders)

# Make plot:
p4 = ggplot(data = worldmap) +
  geom_sf(fill = "gray60", color = "gray60") +
  geom_sf(data = grid_borders, fill = 'blue', color = 'white', alpha = 0.3) +
  geom_sf(data = grid_centers, color = 'red', shape = 4, size = 2) +
  coord_sf(expand = FALSE, xlim = xLim, ylim = yLim) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(breaks = yBreaks) + scale_x_continuous(breaks = xBreaks)
ggsave(file.path(shrpoint_path, plot_dir, paste0('map_catch_grid', img_type)), plot = p4,
       width = img_width, height = 130, units = 'mm', dpi = img_res)
