rm(list = ls())

# Read path and parameters for plots:
source('sharepoint_path.R')
source('code/parameters_for_plots.R')
source('code/auxiliary_functions.R')

# SS model with projections:
SS_model = file.path('projections/6_SplitCPUE_tag01_EC0_h0.8')

# Read reports:
mod1 = SS_output(dir = file.path(shrpoint_path, SS_model), repfile = 'Reportc1.sso')
mod2 = SS_output(dir = file.path(shrpoint_path, SS_model), repfile = 'Report-scale-c1.sso')
mod3 = SS_output(dir = file.path(shrpoint_path, SS_model), repfile = 'Report-scale20-c1.sso')

mod_period = mod1$recruit %>% dplyr::filter(Yr <= 308) %>% mutate(type = 'Model period')
mod_proj = rbind(mod1$recruit %>% dplyr::filter(Yr >= 307) %>% mutate(type = 'Scalar = 1'),
                  mod2$recruit %>% dplyr::filter(Yr >= 307) %>% mutate(type = 'Scalar from last 12 years'),
                  mod3$recruit %>% dplyr::filter(Yr >= 307) %>% mutate(type = 'Scalar from last 20 years'))
mod_period = mod_period %>% mutate(Year = ssts2yq(Yr))
mod_proj = mod_proj %>% mutate(Year = ssts2yq(Yr))

p1 = ggplot(data = mod_period, aes(x = Year, y = pred_recr)) +
  geom_line() +
  geom_line(data = mod_proj, aes(x = Year, y = pred_recr, color = type)) +
  geom_vline(xintercept = 2024, linetype = 'dashed') +
  coord_cartesian(xlim = c(2002, 2034)) +
  ylab('Recruitment') + xlab(NULL) +
  theme(legend.position = 'bottom') +
  guides(color = guide_legend(title = NULL))
ggsave(file.path(shrpoint_path, plot_dir, paste0('ts_rec_proj', img_type)), plot = p1,
       width = img_width, height = 110, units = 'mm', dpi = img_res)
