# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code/data_exploration', 'parameters_for_plots.R'))

# -------------------------------------------------------------------------
# Compare current data inputs with inputs from 2021 assessment
spat_config = '4A_io'
base_dat = SS_readdat(file = file.path(shrpoint_path, 'models/base', spat_config, 'data.ss'))


# -------------------------------------------------------------------------
# Compare catch information:

# Current catch:
catch_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'catch.csv'))
fleet_name_df = catch_dat %>% group_by(ModelFleet) %>% summarise(fleet_name = unique(ModelFishery))
colnames(fleet_name_df) = c('fleet_number', 'fleet_name')
catch_dat = catch_dat[,c('qtr', 'ModelFleet', 'Catch')]
colnames(catch_dat) = c('time', 'fleet_number', 'catch')
catch_dat = catch_dat %>% mutate(type = 'current')

# 2021 catch:
old_catch_dat = base_dat$catch
old_catch_dat = old_catch_dat[,c('year', 'fleet', 'catch')]
colnames(old_catch_dat) = c('time', 'fleet_number', 'catch')
old_catch_dat = old_catch_dat %>% mutate(type = '2021 assessment')

merged_catch = rbind(catch_dat, old_catch_dat)
merged_catch = merged_catch %>% dplyr::filter(time %in% 13:304)
merged_catch = left_join(merged_catch, fleet_name_df)
merged_catch$time = ssts2yq(merged_catch$time) # transform to yr-qt

# Make plot:
p1 = ggplot(data = merged_catch, aes(x = time, y = catch*1e-03)) +
      geom_line(aes(color = type)) +
      ylab("Catch (thousands of tons)") + xlab('Model time') +
      theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
            legend.position = 'bottom') +
      scale_y_continuous(breaks = breaks_extended(3)) +
      guides(color = guide_legend(title = NULL)) +
      facet_wrap( ~ fleet_name, scales = 'free_y', ncol = 4)
ggsave(file.path(shrpoint_path, plot_dir, paste0('compare_catch', img_type)), plot = p1,
       width = 170, height = 200, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# Compare CPUE information

# Current CPUE:
cpue_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'scaled_cpue.csv'))
cpue_dat = cpue_dat[,c('qtr', 'fleet', 'pr_7994_m8')]
colnames(cpue_dat) = c('time', 'fleet_number', 'obs')
cpue_dat = cpue_dat %>% mutate(type = 'current')

# 2021 CPUE:
old_cpue_dat = base_dat$CPUE
old_cpue_dat = old_cpue_dat[,c('year', 'index', 'obs')]
colnames(old_cpue_dat)[1:2] = c('time', 'fleet_number')
old_cpue_dat = old_cpue_dat %>% mutate(type = '2021 assessment')

merged_cpue = rbind(cpue_dat, old_cpue_dat)
merged_cpue = merged_cpue %>% mutate(fleet_name = if_else(fleet_number == 22, 'LL 1b', 
                                                          if_else(fleet_number == 23, 'LL 2', 
                                                                  if_else(fleet_number == 24, 'LL 3', 'LL 4'))))
merged_cpue$fleet_name = factor(merged_cpue$fleet_name, levels = c('LL 1b', 'LL 4', 'LL 2', 'LL 3'))
merged_cpue$time = ssts2yq(merged_cpue$time) # transform to yr-qt

# Make plot:
p1 = ggplot(data = merged_cpue, aes(x = time, y = obs)) +
  geom_line(aes(color = type)) +
  ylab("Scaled CPUE") + xlab('Model time') +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        legend.position = 'bottom') +
  scale_y_continuous(breaks = breaks_extended(3)) +
  guides(color = guide_legend(title = NULL)) +
  facet_wrap( ~ fleet_name, ncol = 2)
ggsave(file.path(shrpoint_path, plot_dir, paste0('compare_cpue', img_type)), plot = p1,
       width = 170, height = 170, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# Compare size information (only until 296):

# Current size:
size_dat = read_csv(file.path(shrpoint_path, 'data/ss3_inputs', spat_config, 'size.csv'))
size_dat = size_dat[,c(1, 3, 7:ncol(size_dat))]
colnames(size_dat)[1:2] = c('time', 'fleet_number')
colnames(size_dat) = tolower(colnames(size_dat))
size_dat = size_dat %>% dplyr::filter(time %in% 13:296)
# Aggregate over time:
size_dat = size_dat %>% group_by(fleet_number) %>% summarise_at(.vars = colnames(.)[3:ncol(size_dat)], sum)
# Sum by row to get freq:
size_dat = size_dat %>% ungroup() %>% mutate(across(-1)/rowSums(across(-1)))
size_dat = size_dat %>% mutate(type = 'current', .after = 'fleet_number')

# 2021 size:
old_size_dat = base_dat$lencomp
old_size_dat = old_size_dat[,c(1, 3, 7:ncol(old_size_dat))]
colnames(old_size_dat)[1:2] = c('time', 'fleet_number')
old_size_dat = old_size_dat %>% dplyr::filter(time %in% 13:296)
# Aggregate over time:
old_size_dat = old_size_dat %>% group_by(fleet_number) %>% summarise_at(.vars = colnames(.)[3:ncol(old_size_dat)], sum)
# Sum by row to get freq:
old_size_dat = old_size_dat %>% ungroup() %>% mutate(across(-1)/rowSums(across(-1)))
old_size_dat = old_size_dat %>% mutate(type = '2021 assessment', .after = 'fleet_number')
colnames(old_size_dat)[3:ncol(old_size_dat)] = colnames(size_dat)[3:ncol(size_dat)]

merged_size = rbind(size_dat, old_size_dat)
merged_size = gather(merged_size, 'len_bin', 'prop', 3:ncol(merged_size))
merged_size$len_bin = as.numeric(gsub(pattern = 'l', replacement = '', x = merged_size$len_bin))
merged_size = left_join(merged_size, fleet_name_df)

# Make plot:
p2 = ggplot(data = merged_size, aes(x = len_bin, y = prop)) +
  geom_line(aes(color = type)) +
  ylab("Proportion") + xlab('Length bin (cm)') +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        legend.position = 'bottom') +
  scale_y_continuous(breaks = breaks_extended(3)) +
  guides(color = guide_legend(title = NULL)) +
  facet_wrap( ~ fleet_name, scales = 'free_y', ncol = 4)
ggsave(file.path(shrpoint_path, plot_dir, paste0('compare_size', img_type)), plot = p2,
       width = 170, height = 200, units = 'mm', dpi = img_res)

