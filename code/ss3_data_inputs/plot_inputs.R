rm(list = ls())

# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code', 'parameters_for_plots.R'))
source(here('code', 'auxiliary_functions.R'))

# Spatial configuration:
spat_config = '4A_io'

# Read fleet labels
fleet_name_df = read.csv(file.path(shrpoint_path, tab_dir, paste0('fleet_label_', spat_config,'.csv')))

# -------------------------------------------------------------------------
# Plot 2024 SS inputs -----------------------------------------------------

# Catch per fleet and year as barplot -------------------------------------

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



# Pie plot: catch by grid/fleet -------------------------------------------

catch_grid = read.csv(file.path(shrpoint_path, 'data/processed', 'catch_grid.csv'))

# Same processing as in CE-4A_io:
catch_grid$Area = get_4Aarea_from_lonlat(catch_grid$long, catch_grid$lat)
catch_grid = create_4Aarea_cols(catch_grid)
filter_data = catch_grid %>% dplyr::filter(Year >= 1980) # relevant period
plot_data = filter_data %>% group_by(Grid, lat, long, FisheryCode) %>% summarise(catch = sum(NCmtFish)) %>% 
  inner_join(filter_data %>%
               group_by(Grid, lat, long) %>%
               summarise(catch_tot = sum(NCmtFish)))
max_grid_catch = max(plot_data$catch_tot)
plot_data = plot_data %>% dplyr::filter(lat >= min(yLim), lat <= max(yLim), 
                                        long >= min(xLim), long <= max(xLim))
plot_data = plot_data %>% mutate(radius = (catch_tot/max_grid_catch),
                                 lat = factor(lat, levels = sort(unique(plot_data$lat), decreasing = TRUE)),
                                 long = factor(long, levels = sort(unique(plot_data$long))))

# Make plot:
p_pie = ggplot(plot_data, aes(x = radius/2, y = catch, fill = FisheryCode, width = radius)) +
  geom_bar(stat="identity", position = 'fill') +
  facet_grid(lat ~ long) + 
  coord_polar("y") +
  theme_void() + 
  scale_fill_manual(values = fleet_col) +
  theme(legend.position="none",
        panel.spacing = unit(-0.2, "cm"),
        strip.background = element_blank(),
        strip.text.x = element_blank(), strip.text.y = element_blank())

p_map = ggplot() + 
  geom_segment(data = reg_lines_4A, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), color = 'gray40') +
  geom_segment(data = reg_lines_1ab, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), color = 'gray40', linetype = 2) +
  theme_classic()
p_map = add_sf_map(p_map)

all_plot = p_map + inset_element(p_pie, 0.065, 0.05, 0.99, 0.99, align_to = 'full')
ggsave(file.path(shrpoint_path, plot_dir, paste0('catch_grid', img_type)), plot = all_plot,
       width = img_width, height = 130, units = 'mm', dpi = img_res)


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
  coord_cartesian(ylim = c(0, NA)) + 
  xlab(NULL) + ylab("Scaled CPUE") +
  facet_wrap( ~ area, scales = 'free_y')
ggsave(file.path(shrpoint_path, plot_dir, paste0('ts_cpue_area', img_type)), plot = p1,
       width = img_width, height = 130, units = 'mm', dpi = img_res)



# Aggregated len comps by fleet -------------------------------------------

size_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size.csv'))
size_dat = size_dat %>% select(Yr, ModelFleet, L010:L198)
size_dat = dplyr::rename(size_dat, c(time = 'Yr', fleet_number = 'ModelFleet'))
colnames(size_dat) = tolower(colnames(size_dat))
# Aggregate over time:
size_dat = size_dat %>% group_by(fleet_number) %>% summarise_at(.vars = colnames(.)[3:ncol(size_dat)], sum)
# Sum by row to get freq:
size_dat = size_dat %>% ungroup() %>% mutate(across(-1)/rowSums(across(-1)))
size_dat = gather(size_dat, 'len_bin', 'prop', 2:ncol(size_dat))
size_dat$len_bin = as.numeric(gsub(pattern = 'l', replacement = '', x = size_dat$len_bin))
size_dat = left_join(size_dat, fleet_name_df)
size_dat = size_dat %>% mutate(fisherycode = str_sub(fleet_name, start = 1, end = 2)) # for colors

# Make plot:
p2 = ggplot(size_dat, aes(x = len_bin, y = prop, fill = fisherycode, color = fisherycode)) + 
  geom_area(alpha = 0.2) +
  geom_line() +
  ylab("Proportion") + xlab('Length bin (cm)') +
  scale_fill_manual(values = fleet_col) +
  scale_color_manual(values = fleet_col) +
  theme(legend.position = 'none') +
  facet_wrap( ~ fleet_name, scales = 'free_y', ncol = 4)
ggsave(file.path(shrpoint_path, plot_dir, paste0('agg_size', img_type)), plot = p2,
       width = img_width, height = 200, units = 'mm', dpi = img_res)
