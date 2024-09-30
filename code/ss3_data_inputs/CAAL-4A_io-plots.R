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


# -------------------------------------------------------------------------
# Plot CAAL SS3 input -----------------------------------------------------

caal_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'caal.csv'))

# Make plot by fleet (aggregate over the years):
plot_data = caal_dat %>% ungroup() %>% mutate(across(-c(1:5))/rowSums(across(-c(1:5))))
plot_data = plot_data %>% gather('Age', 'Prop', `0`:`28`) %>% mutate(Age = as.numeric(Age))
plot_data = plot_data %>% dplyr::rename(fleet_number = ModelFleet)
plot_data = left_join(plot_data, fleet_name_df)
plot_data = plot_data %>% mutate(fisherycode = str_sub(fleet_name, start = 1, end = 2)) # for colors
# plot_data = plot_data %>% dplyr::filter(!(Prop == 0))

p1 = ggplot(plot_data, aes(x = Age, y = LowBin, fill = fisherycode)) + 
  geom_point(aes(size = Prop), pch = 21, color = 'black') +
  ylab("Length bin (cm)") + xlab('Age (quarters)') +
  scale_fill_manual(values = fleet_col) +
  scale_size_continuous(range = c(0, 2)) +
  theme_classic() +
  theme(legend.position = 'none') +
  facet_wrap( ~ fleet_name, ncol = 3)
ggsave(file.path(shrpoint_path, plot_dir, paste0('caal', img_type)), plot = p1,
       width = img_width, height = 170, units = 'mm', dpi = img_res)
