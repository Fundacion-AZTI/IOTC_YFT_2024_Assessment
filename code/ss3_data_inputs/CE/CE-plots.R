rm(list = ls())

# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code', 'parameters_for_plots.R'))
source(here('code', 'auxiliary_functions.R'))

# Spatial configuration:
spat_config = '4A_io'

# Length bins
L_labels  =  c(Paste("L0",seq(10,98,2)), Paste("L",seq(100,198,2))) 

#Fishery definiton
fish_info = read.csv(file.path('code/ss3_data_inputs', paste0('FisheryDefinitions_', spat_config, '.csv')), sep = ';')

# -------------------------------------------------------------------------
# Plot 2024 SS inputs -----------------------------------------------------

# Catch per fishery and year as barplot -------------------------------------

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

# Save legend for next plot:
fish_legend <- get_legend(p2)    
save(fish_legend, file = file.path(shrpoint_path, 'data/processed', 'fishery_legend.RData'))

# Catch per fishery, year, and area as barplot ------------------------------

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

# Pie plot: catch by grid/fishery -------------------------------------------

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

all_plot = p_map + inset_element(fish_legend, 0.15, 0.6, 0.1, 0.5, align_to = 'full') + 
  inset_element(p_pie, 0.065, 0.05, 0.99, 0.99, align_to = 'full')
ggsave(file.path(shrpoint_path, plot_dir, paste0('catch_grid', img_type)), plot = all_plot,
       width = img_width, height = 130, units = 'mm', dpi = img_res)

# Compare catch information -----------------------------------------------
# Read 2021 SS data inputs
base_dat = SS_readdat(file = file.path(shrpoint_path, 'models/base', spat_config, 'data.ss'))

# 2024 catch:
catch_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'catch.csv'))
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
merged_catch = left_join(merged_catch, fish_info)
merged_catch$time = ssts2yq(merged_catch$time) # transform to yr-qt

# Make plot:
p1 = ggplot(data = merged_catch, aes(x = time, y = catch*1e-03)) +
  geom_line(aes(color = type), linewidth = 0.2) +
  ylab("Catch (thousands of tons)") + xlab(NULL) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        legend.position = c(0.6, 0.05)) +
  scale_y_continuous(breaks = breaks_extended(3)) +
  guides(color = guide_legend(title = NULL)) +
  facet_wrap( ~ fleet_name, scales = 'free_y', ncol = 4)
ggsave(file.path(shrpoint_path, plot_dir, paste0('compare_catch', img_type)), plot = p1,
       width = img_width, height = 200, units = 'mm', dpi = img_res)

