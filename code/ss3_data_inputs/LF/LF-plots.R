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

# Read fishery legend in:
load(file.path(shrpoint_path, 'data/processed', 'fishery_legend.RData'))

# -------------------------------------------------------------------------
# Aggregated len comps by fleet -------------------------------------------

# Simple aggregation
size_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size-original.csv'))
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
size_dat = size_dat %>% mutate(type = 'Simple aggregation', .after = 'fleet_number')
size_dat1 = size_dat


# Catch raised aggregation
size_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size-cwp55.csv'))
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
size_dat = size_dat %>% mutate(type = 'Catch-raised aggregation', .after = 'fleet_number')
size_dat2 = size_dat

# Merge datasets:
plot_data = rbind(size_dat1, size_dat2)

# Make plot:
p2 = ggplot(plot_data %>% dplyr::filter(type == 'Simple aggregation'), aes(x = len_bin, y = prop, color = fisherycode)) + 
  geom_line() +
  geom_line(data = plot_data %>% dplyr::filter(type == 'Catch-raised aggregation'), aes(x = len_bin, y = prop), color = 'black') +
  ylab("Proportion") + xlab('Length bin (cm)') +
  scale_color_manual(values = fleet_col) +
  coord_cartesian(ylim = c(0, 0.25)) +
  theme_classic() +
  theme(legend.position = 'none') +
  facet_wrap( ~ fleet_name, ncol = 4)
ggsave(file.path(shrpoint_path, plot_dir, paste0('agg_size', img_type)), plot = p2,
       width = img_width, height = 180, units = 'mm', dpi = img_res)


# Nsamp (RQ) per fishery and time -----------------------------------------

# Simple aggregation
size_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size-original.csv'))
size_dat = size_dat %>% select(Yr, ModelFleet, Nsamp)
size_dat = dplyr::rename(size_dat, c(time = 'Yr', fleet_number = 'ModelFleet'))
colnames(size_dat) = tolower(colnames(size_dat))
# Merge
size_dat = left_join(size_dat, fleet_name_df)
size_dat = size_dat %>% mutate(fisherycode = str_sub(fleet_name, start = 1, end = 2)) # for colors
size_dat = size_dat %>% mutate(time2 = ssts2yq(time))

# Make plot:
p3 = ggplot(size_dat, aes(x = time2, y = fleet_name, fill = fisherycode)) + 
  geom_point(aes(size = nsamp), pch = 21, color = 'black') +
  ylab(NULL) + xlab(NULL) +
  scale_fill_manual(values = fleet_col) +
  scale_size_continuous(range = c(1, 3)) +
  coord_cartesian(xlim = c(1955, 2023)) +   
  theme_classic() +
  theme(legend.position = 'none') +
  ggtitle(label = 'Simple aggregation')

# Catch raised aggregation
size_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size-cwp55.csv'))
size_dat = size_dat %>% select(Yr, ModelFleet, Nsamp)
size_dat = dplyr::rename(size_dat, c(time = 'Yr', fleet_number = 'ModelFleet'))
colnames(size_dat) = tolower(colnames(size_dat))
# Merge
size_dat = left_join(size_dat, fleet_name_df)
size_dat = size_dat %>% mutate(fisherycode = str_sub(fleet_name, start = 1, end = 2)) # for colors
size_dat = size_dat %>% mutate(time2 = ssts2yq(time))

# Make plot:
p4 = ggplot(size_dat, aes(x = time2, y = fleet_name, fill = fisherycode)) + 
  geom_point(aes(size = nsamp), pch = 21, color = 'black') +
  ylab(NULL) + xlab(NULL) +
  scale_fill_manual(values = fleet_col) +
  scale_size_continuous(range = c(1, 3)) +
  coord_cartesian(xlim = c(1955, 2023)) +
  theme_classic() +
  theme(legend.position = 'none') +
  ggtitle(label = 'Catch-raised aggregation')

# Merge:
p5 = grid.arrange(p3, p4, ncol = 1)
ggsave(file.path(shrpoint_path, plot_dir, paste0('rq_size', img_type)), plot = p5,
       width = img_width, height = 220, units = 'mm', dpi = img_res)


# Mean length over the years ----------------------------------------------

# Original:
size_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size-original.csv'))
size_dat = size_dat %>% select(Yr, ModelFleet, L010:L198)
size_dat = dplyr::rename(size_dat, c(time = 'Yr', fleet_number = 'ModelFleet'))
colnames(size_dat) = tolower(colnames(size_dat))
# Find mean length per fleet and year:
tmp_dat = gather(size_dat, 'len_bin', 'freq', 3:ncol(size_dat))
tmp_dat = tmp_dat %>% mutate(len_bin = as.numeric(gsub(pattern = 'l', replacement = '', x = len_bin)))
tmp_dat = tmp_dat %>% group_by(time, fleet_number) %>% summarise(mean_len = weighted.mean(x = len_bin, w = freq))
# Add extra columns:
size_dat = tmp_dat
size_dat = size_dat %>% mutate(type = 'Simple aggregation', .after = 'fleet_number')
size_dat1 = size_dat

# cwp55
size_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size-cwp55.csv'))
size_dat = size_dat %>% select(Yr, ModelFleet, L010:L198)
size_dat = dplyr::rename(size_dat, c(time = 'Yr', fleet_number = 'ModelFleet'))
colnames(size_dat) = tolower(colnames(size_dat))
# Find mean length per fleet and year:
tmp_dat = gather(size_dat, 'len_bin', 'freq', 3:ncol(size_dat))
tmp_dat = tmp_dat %>% mutate(len_bin = as.numeric(gsub(pattern = 'l', replacement = '', x = len_bin)))
tmp_dat = tmp_dat %>% group_by(time, fleet_number) %>% summarise(mean_len = weighted.mean(x = len_bin, w = freq))
# Add extra columns:
size_dat = tmp_dat
size_dat = size_dat %>% mutate(type = 'Catch-raised aggregation', .after = 'fleet_number')
size_dat2 = size_dat

# Merge datasets:
merged_size = rbind(size_dat1, size_dat2)
merged_size$time = ssts2yq(merged_size$time)
all_times_df = expand.grid(time = seq(from = min(merged_size$time), to = max(merged_size$time), by = 0.25),
                           fleet_number = unique(merged_size$fleet_number), type = unique(merged_size$type))
plot_data_df = left_join(all_times_df, merged_size)
plot_data_df = left_join(plot_data_df, fleet_name_df)
plot_data_df = plot_data_df %>% mutate(fisherycode = str_sub(fleet_name, start = 1, end = 2)) # for colors

# Make plot:
p2 = ggplot(data = plot_data_df %>% dplyr::filter(type == 'Simple aggregation'), aes(x = time, y = mean_len, color = fisherycode)) +
  geom_point(size = 0.5) +
  geom_line() +
  geom_point(data = plot_data_df  %>% dplyr::filter(type == 'Catch-raised aggregation'), aes(x = time, y = mean_len), color = 'black', size = 0.5) +
  geom_line(data = plot_data_df  %>% dplyr::filter(type == 'Catch-raised aggregation'), aes(x = time, y = mean_len), color = 'black') +
  ylab("Mean length (cm)") + xlab(NULL) +
  scale_color_manual(values = fleet_col) +
  #coord_cartesian(ylim = c(10, 200)) +
  theme_classic() +
  theme(legend.position = 'none') +
  facet_wrap( ~ fleet_name, scales = 'free_y', ncol = 4)
ggsave(file.path(shrpoint_path, plot_dir, paste0('mlen', img_type)), plot = p2,
       width = img_width, height = 180, units = 'mm', dpi = img_res)


# Aggregated length by grid and fishery -------------------------------------

load(file.path(shrpoint_path, 'data/processed', 'mergedStd_5.RData'))
size_grid = mergedStd

colnames(size_grid) = str_to_title(colnames(size_grid))
colnames(size_grid)[c(6)] = c('FisheryCode')
# Same processing as in CE-4A_io:
size_grid$Area = get_4Aarea_from_lonlat(size_grid$Lon, size_grid$Lat)
size_grid = create_4Aarea_cols(size_grid)

filter_data = size_grid %>% dplyr::filter(Year >= 1980) # relevant period
plot_data = filter_data %>% group_by(Grid_id, Lat, Lon, FisheryCode) %>% dplyr::summarise_at(L_labels,list(Sum))
plot_data = plot_data %>% dplyr::filter(Lat >= min(yLim), Lat <= max(yLim), 
                                        Lon >= min(xLim), Lon <= max(xLim))
plot_data = plot_data %>% mutate(Lat = round(Lat, digits = 2), Lon = round(Lon, digits = 2)) # round centroids
# Sum by row to get freq:
plot_data = plot_data %>% ungroup() %>% mutate(across(-c(1:4))/rowSums(across(-c(1:4))))
plot_data = gather(plot_data, 'len_bin', 'prop', 5:ncol(plot_data))
plot_data$len_bin = as.numeric(gsub(pattern = 'L', replacement = '', x = plot_data$len_bin))
# Make lon lat as factors:
plot_data = plot_data %>% mutate(lat = factor(Lat, levels = sort(unique(plot_data$Lat), decreasing = TRUE)),
                                 lon = factor(Lon, levels = sort(unique(plot_data$Lon))))


# Make plot:
p_hist = ggplot(plot_data, aes(x = len_bin, y = prop, fill = FisheryCode)) +
  geom_area(alpha = 0.75) +
  facet_grid(lat ~ lon) + 
  theme_void() + 
  coord_cartesian(ylim = c(0, 0.3)) +
  scale_fill_manual(values = fleet_col) +
  # scale_color_manual(values = fleet_col) +
  theme(legend.position="none",
        panel.spacing = unit(-0.02, "cm"),
        strip.background = element_blank(),
        strip.text.x = element_blank(), strip.text.y = element_blank())

p_map = ggplot() + 
  geom_segment(data = reg_lines_4A, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), color = 'gray40') +
  geom_segment(data = reg_lines_1ab, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), color = 'gray40', linetype = 2) +
  theme_classic()
p_map = add_sf_map(p_map)

all_plot = p_map + inset_element(fish_legend, 0.15, 0.6, 0.1, 0.5, align_to = 'full') +
  inset_element(p_hist, 0.065, 0.055, 0.99, 0.99, align_to = 'full')
ggsave(file.path(shrpoint_path, plot_dir, paste0('size_grid', img_type)), plot = all_plot,
       width = img_width, height = 130, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# Compare (original) size information: aggregated size comps ----------------
# Read 2021 SS data inputs
base_dat = SS_readdat(file = file.path(shrpoint_path, 'models/base', spat_config, 'data.ss'))

# 2024 size:
size_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size-original.csv'))
size_dat = size_dat %>% select(Yr, ModelFleet, L010:L198)
size_dat = dplyr::rename(size_dat, c(time = 'Yr', fleet_number = 'ModelFleet'))
colnames(size_dat) = tolower(colnames(size_dat))
size_dat = size_dat %>% dplyr::filter(time %in% 13:296) # same period for both assessments
# Aggregate over time:
size_dat = size_dat %>% group_by(fleet_number) %>% summarise_at(.vars = colnames(.)[3:ncol(size_dat)], sum)
# Sum by row to get freq:
size_dat = size_dat %>% ungroup() %>% mutate(across(-1)/rowSums(across(-1)))
size_dat = size_dat %>% mutate(type = '2024 assessment', .after = 'fleet_number')

# 2021 size:
old_size_dat = base_dat$lencomp
old_size_dat = old_size_dat %>% select(Yr, FltSvy, l10:l198)
old_size_dat = dplyr::rename(old_size_dat, c(time = 'Yr', fleet_number = 'FltSvy'))
old_size_dat = old_size_dat %>% dplyr::filter(time %in% 13:296)
# Aggregate over time:
old_size_dat = old_size_dat %>% group_by(fleet_number) %>% summarise_at(.vars = colnames(.)[3:ncol(old_size_dat)], sum)
# Sum by row to get freq:
old_size_dat = old_size_dat %>% ungroup() %>% mutate(across(-1)/rowSums(across(-1)))
old_size_dat = old_size_dat %>% mutate(type = '2021 assessment', .after = 'fleet_number')
colnames(old_size_dat)[3:ncol(old_size_dat)] = colnames(size_dat)[3:ncol(size_dat)]

# Merge datasets:
merged_size = rbind(size_dat, old_size_dat)
merged_size = gather(merged_size, 'len_bin', 'prop', 3:ncol(merged_size))
merged_size$len_bin = as.numeric(gsub(pattern = 'l', replacement = '', x = merged_size$len_bin))
merged_size = left_join(merged_size, fleet_name_df)

# Make plot:
p2 = ggplot(data = merged_size, aes(x = len_bin, y = prop)) +
  geom_line(aes(color = type)) +
  ylab("Proportion") + xlab('Length bin (cm)') +
  theme(legend.position = 'bottom') +
  scale_y_continuous(breaks = breaks_extended(3)) +
  coord_cartesian(ylim = c(0, 0.25)) +
  guides(color = guide_legend(title = NULL)) +
  facet_wrap( ~ fleet_name, ncol = 4)
ggsave(file.path(shrpoint_path, plot_dir, paste0('compare_size', img_type)), plot = p2,
       width = img_width, height = 200, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# Compare (original) size information: mean length over the years --------------

# 2024 size:
size_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size-original.csv'))
size_dat = size_dat %>% select(Yr, ModelFleet, L010:L198)
size_dat = dplyr::rename(size_dat, c(time = 'Yr', fleet_number = 'ModelFleet'))
colnames(size_dat) = tolower(colnames(size_dat))
# Find mean length per fleet and year:
tmp_dat = gather(size_dat, 'len_bin', 'freq', 3:ncol(size_dat))
tmp_dat = tmp_dat %>% mutate(len_bin = as.numeric(gsub(pattern = 'l', replacement = '', x = len_bin)))
tmp_dat = tmp_dat %>% group_by(time, fleet_number) %>% summarise(mean_len = weighted.mean(x = len_bin, w = freq))
# Add extra columns:
size_dat = tmp_dat
size_dat = size_dat %>% mutate(type = '2024 assessment', .after = 'fleet_number')

# 2021 size:
old_size_dat = base_dat$lencomp
old_size_dat = old_size_dat %>% select(Yr, FltSvy, l10:l198)
old_size_dat = dplyr::rename(old_size_dat, c(time = 'Yr', fleet_number = 'FltSvy'))
# Find mean length per fleet and year:
tmp_dat = gather(old_size_dat, 'len_bin', 'freq', 3:ncol(old_size_dat))
tmp_dat = tmp_dat %>% mutate(len_bin = as.numeric(gsub(pattern = 'l', replacement = '', x = len_bin)))
tmp_dat = tmp_dat %>% group_by(time, fleet_number) %>% summarise(mean_len = weighted.mean(x = len_bin, w = freq))
# Add extra columns:
old_size_dat = tmp_dat
old_size_dat = old_size_dat %>% mutate(type = '2021 assessment', .after = 'fleet_number')

# Merge datasets:
merged_size = rbind(size_dat, old_size_dat)
merged_size$time = ssts2yq(merged_size$time)
all_times_df = expand.grid(time = seq(from = min(merged_size$time), to = max(merged_size$time), by = 0.25),
                           fleet_number = unique(merged_size$fleet_number), type = unique(merged_size$type))
plot_data_df = left_join(all_times_df, merged_size)
plot_data_df = left_join(plot_data_df, fleet_name_df)

# Make plot:
p2 = ggplot(data = plot_data_df, aes(x = time, y = mean_len)) +
  geom_point(aes(color = type), size = 0.25) +
  geom_line(aes(color = type)) +
  ylab("Mean length (cm)") + xlab(NULL) +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        legend.position = c(0.875, 0.05)) +
  scale_y_continuous(breaks = breaks_extended(3)) +
  guides(color = guide_legend(title = NULL)) +
  facet_wrap( ~ fleet_name, scales = 'free_y', ncol = 4)
ggsave(file.path(shrpoint_path, plot_dir, paste0('compare_mlen', img_type)), plot = p2,
       width = img_width, height = 200, units = 'mm', dpi = img_res)
