# Exploration of HD length and catch data:

rm(list = ls())

# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code', 'parameters_for_plots.R'))
source(here('code', 'auxiliary_functions.R'))

# Length bins
L_labels  =  c(Paste("L0",seq(10,98,2)), Paste("L",seq(100,198,2))) 

# Spatial configuration:
spat_config = '4A_io'

# Catch information -------------------------------------------------------
catch_grid = read.csv(file.path(shrpoint_path, 'data/processed', 'catch_grid.csv'))
hd_catch = catch_grid %>% dplyr::filter(FisheryCode == 'HD')

tmp_dat = hd_catch %>% group_by(Year, Fleet) %>% summarise(Catch = sum(Catch))
tot_dat = tmp_dat %>% group_by(Fleet) %>% summarise(Catch = sum(Catch))
tot_dat = tot_dat[order(tot_dat$Catch, decreasing = T), ]
tot_dat = tot_dat %>% mutate(Fleet_label = if_else(Catch > 20000, Fleet, 'Others')) %>% select(-Catch)

plot_data = left_join(tmp_dat, tot_dat)
plot_data = plot_data %>% group_by(Year, Fleet_label) %>% summarise(Catch = sum(Catch))

d1 = ggplot(data = plot_data, aes(x = Year, y = Catch, fill = Fleet_label)) +
  geom_col() +
  xlab(NULL) + ylab("Catch (mt)") +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = guide_legend(title = NULL)) 
ggsave(file.path(shrpoint_path, plot_dir, paste0('hd_catch_cpc', img_type)), plot = d1,
       width = img_width, height = 130, units = 'mm', dpi = img_res)


# Length information ------------------------------------------------------

size_grid = read.csv(file.path(shrpoint_path, 'data/processed', 'size_grid.csv'))
hd_size = size_grid %>% dplyr::filter(FisheryCode == 'HD')

tmp_dat = hd_size %>%
  dplyr::group_by(Year,Fleet) %>% 
  dplyr::summarise_at(L_labels,list(Sum))

plot_data = left_join(tmp_dat, tot_dat)
plot_data = plot_data %>% dplyr::relocate(Fleet_label, .after = Fleet)
plot_data = plot_data %>%
  dplyr::group_by(Year,Fleet_label) %>% 
  dplyr::summarise_at(L_labels,list(Sum))
plot_data = plot_data %>% ungroup() %>% mutate(across(-c(1,2))/rowSums(across(-c(1,2))))
plot_data = gather(plot_data, 'len_bin', 'prop', 3:ncol(plot_data))
plot_data$len_bin = as.numeric(gsub(pattern = 'L', replacement = '', x = plot_data$len_bin))
plot_data$Fleet_label[is.na(plot_data$Fleet_label)] = 'Others'

d2 = ggplot(plot_data, aes(x = len_bin, y = prop, fill = Fleet_label, color = Fleet_label)) + 
  geom_area(alpha = 0.2) +
  geom_line() +
  ylab(NULL) + xlab('Length (cm)') +
  scale_fill_brewer(palette = 'Dark2') +
  scale_color_brewer(palette = 'Dark2') +
  coord_cartesian(ylim = c(0, 0.2), xlim = c(2, 198)) +
  theme_classic() +
  facet_wrap( ~ Year, ncol = 6)
ggsave(file.path(shrpoint_path, plot_dir, paste0('hd_size_cpc', img_type)), plot = d2,
       width = img_width, height = 170, units = 'mm', dpi = img_res)


# RQ information ----------------------------------------------------------

size_grid = read.csv(file.path(shrpoint_path, 'data/processed', 'size_grid.csv'))
hd_size = size_grid %>% dplyr::filter(FisheryCode == 'HD')

tmp_dat = hd_size %>%
  dplyr::group_by(Year,Fleet) %>% 
  dplyr::summarise(RQ = mean(REPORTING_QUALITY))

plot_data = left_join(tmp_dat, tot_dat)
plot_data = plot_data %>% dplyr::relocate(Fleet_label, .after = Fleet)
plot_data = plot_data %>%
  dplyr::group_by(Year,Fleet_label) %>% 
  dplyr::summarise(RQ = mean(RQ))
plot_data$Fleet_label[is.na(plot_data$Fleet_label)] = 'Others'

d3 = ggplot(plot_data, aes(x = Year, y = RQ, color = Fleet_label, fill = Fleet_label)) + 
  geom_col() +
  ylab('Reporting quality score') + xlab(NULL) +
  scale_fill_brewer(palette = 'Dark2') +
  scale_color_brewer(palette = 'Dark2') +
  theme_classic() +
  theme(legend.position = 'bottom') +
  facet_wrap( ~ Fleet_label, ncol = 3)
ggsave(file.path(shrpoint_path, plot_dir, paste0('hd_rq_cpc', img_type)), plot = d3,
       width = img_width, height = 170, units = 'mm', dpi = img_res)

