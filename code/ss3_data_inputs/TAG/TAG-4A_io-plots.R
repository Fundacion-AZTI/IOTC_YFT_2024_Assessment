rm(list = ls())

# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code', 'parameters_for_plots.R'))
source(here('code', 'auxiliary_functions.R'))

# Spatial configuration:
spat_config = '4A_io'

# Read fleet labels
fleet_name_df = read.csv(file.path(shrpoint_path, tab_dir, paste0('fleet_label_', spat_config,'.csv')))

# Read data in:
tag_rel = read_csv(file.path(shrpoint_path, 'data/processed', 'tag-filtered-release-farley.csv'))
tag_rec = read_csv(file.path(shrpoint_path, 'data/processed', 'tag-filtered-recapture-farley.csv'))
tag_rec = tag_rec %>% mutate(fleet_number = rec_model_fleet)
tag_rec = left_join(tag_rec, fleet_name_df, by = 'fleet_number')
tag_rec = tag_rec %>% mutate(fisherycode = str_sub(fleet_name, start = 1, end = 2))

# -------------------------------------------------------------------------
# Plot release and recapture locations -----------------------------------------------------

# Plot including all fleets:
plot_rel = tag_rel %>% select(rel_long, rel_lat) %>% 
            st_as_sf(coords = c("rel_long", "rel_lat"), crs = 4326, remove = FALSE)
plot_rec = tag_rec %>% select(rec_long, rec_lat) %>% 
            st_as_sf(coords = c("rec_long", "rec_lat"), crs = 4326, remove = FALSE)

p1 = ggplot() +
  geom_sf(data = plot_rec, color = 'darkslategray') +
  geom_sf(data = plot_rel, color = 'brown3') +
  geom_segment(data = reg_lines_4A, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), color = 'gray40') +
  geom_segment(data = reg_lines_1ab, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), color = 'gray40', linetype = 2) +
  theme_classic()
p1 = add_sf_map(p1)

# Plot including only PS fleet:
plot_rec = tag_rec %>% dplyr::filter(fisherycode %in% c('LS', 'FS')) %>% select(rec_long, rec_lat) %>% 
  st_as_sf(coords = c("rec_long", "rec_lat"), crs = 4326, remove = FALSE)

p2 = ggplot() +
  geom_sf(data = plot_rec, color = 'darkslategray') +
  geom_sf(data = plot_rel, color = 'brown3') +
  geom_segment(data = reg_lines_4A, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), color = 'gray40') +
  geom_segment(data = reg_lines_1ab, aes(x = lon1, y = lat1, xend = lon2, yend = lat2), color = 'gray40', linetype = 2) +
  theme_classic()
p2 = add_sf_map(p2)

# Merge plots:
p3 = grid.arrange(p1, p2, nrow = 2)
ggsave(file.path(shrpoint_path, plot_dir, paste0('tag_map', img_type)), plot = p3,
       width = img_width*0.5, height = 120, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# Plot number of fish tagged (by age) and recoveries (by fishery)

# Plot 1:
plot_rel = tag_rel %>% mutate(Time = paste(rel_year, rel_quarter, sep = '-'))
plot_rel = plot_rel %>% group_by(Time, rel_age) %>% summarise(n_obs = n())
plot_rel = plot_rel %>% mutate(Age = factor(rel_age, levels = sort(unique(plot_rel$rel_age))))
n_ages = length(unique(plot_rel$Age))
my_colors = colorRampPalette(brewer.pal(8, "Spectral"))(n_ages)

p1 = ggplot(plot_rel, aes(x = Time, y = n_obs, color = Age, fill = Age)) + 
  geom_col() +
  ylab('Number of fish released') + xlab(NULL) +
  scale_fill_manual(values = my_colors) +
  scale_color_manual(values = my_colors) +
  theme_classic() +
  theme(legend.position = c(0.15, 0.68),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(fill = guide_legend(ncol = 2, title = NULL), color = guide_legend(title = NULL))

# Plot 2:
plot_rec = tag_rec %>% mutate(Time = paste(rec_year, rec_quarter, sep = '-'))
plot_rec = plot_rec %>% group_by(Time, fisherycode) %>% summarise(n_obs = n())

p2 = ggplot(plot_rec, aes(x = Time, y = n_obs, color = fisherycode, fill = fisherycode)) + 
  geom_col() +
  ylab('Number of fish recaptured') + xlab(NULL) +
  scale_fill_manual(values = fleet_col) +
  scale_color_manual(values = fleet_col) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.68),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) 

# Merge plots:
p3 = grid.arrange(p1, p2, nrow = 2)
ggsave(file.path(shrpoint_path, plot_dir, paste0('tag_nfish', img_type)), plot = p3,
       width = img_width*0.75, height = 180, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# Plot Nfish in release and recovery region:
plot_dat = tag_rec %>% group_by(rec_year, rel_model_area, rec_model_area) %>% summarise(n_obs = n())
plot_dat = plot_dat %>% mutate(rel_model_area = factor(rel_model_area, levels = 1:2, labels = c('Release region: 1', 'Release region: 2')))
plot_dat = plot_dat %>% mutate(rec_model_area = factor(rec_model_area))

p1 = ggplot(plot_dat, aes(x = factor(rec_year), y = n_obs, color = rec_model_area, fill = rec_model_area)) + 
  geom_bar(stat = "identity", position =position_dodge(preserve = "single") ) +
  ylab('Number of fish recaptured') + xlab(NULL) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.25),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(fill = guide_legend(title = 'Recovery region'), color = guide_legend(title = 'Recovery region')) +
  facet_wrap(~ rel_model_area, nrow = 2, scales = 'free_y')
ggsave(file.path(shrpoint_path, plot_dir, paste0('tag_nfish_area', img_type)), plot = p1,
       width = img_width*0.75, height = 180, units = 'mm', dpi = img_res)
