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
# Plot CAAL SS3 input -----------------------------------------------------

caal_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'caal.csv'))

# Make plot by fleet (aggregate over the years):
plot_data = caal_dat %>% ungroup() %>% mutate(across(-c(1:5))/rowSums(across(-c(1:5))))
plot_data = plot_data %>% gather('Age', 'Prop', `0`:`28`) %>% mutate(Age = as.numeric(Age))
plot_data = plot_data %>% dplyr::rename(fleet_number = ModelFleet)
plot_data = left_join(plot_data, fish_info)
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


# Samples per year and fishery --------------------------------------------

caal_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'caal.csv'))

# Make plot by fleet (aggregate over the years):
plot_data = caal_dat
plot_data = plot_data %>% dplyr::rename(fleet_number = ModelFleet)
plot_data = left_join(plot_data, fish_info)
plot_data = plot_data %>% mutate(fisherycode = str_sub(fleet_name, start = 1, end = 2)) # for colors
plot_data$Year = floor(qtr2yearqtr(plot_data$Yr, initial = 1950, base = 13))
plot_data = plot_data %>% group_by(Year, fisherycode) %>% summarise(Nsamp = sum(Nsamp))

p1 = ggplot(plot_data, aes(x = Year, y = Nsamp, fill = fisherycode)) + 
  geom_col() +
  ylab('Number of sampled fish') + xlab(NULL) +
  scale_fill_manual(values = fleet_col) +
  coord_cartesian(xlim = c(2009, 2022)) +
  scale_x_continuous(breaks = 2009:2022) +
  theme_classic() +
  theme(legend.position = c(0.15, 0.7),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(fill = guide_legend(title = NULL))
ggsave(file.path(shrpoint_path, plot_dir, paste0('caal_nsamp', img_type)), plot = p1,
       width = img_width*0.5, height = 75, units = 'mm', dpi = img_res)

