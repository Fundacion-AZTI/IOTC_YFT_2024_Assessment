rm(list = ls())

# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code', 'parameters_for_plots.R'))
source(here('code', 'auxiliary_functions.R'))

# SS3 model to extract growth function (make sure growth is fixed, and use 40+ plus group):
mod_2024 = SS_output(dir = file.path(shrpoint_path, 'models/RefModels/2_TwoBlock_LLsel'))
# Make sure to use a model with two sexes and fixed growth
mod_sex_2024 = SS_output(dir = file.path(shrpoint_path, 'models/update/sensitivities_15_M/15_recDev2021_gender'))

# 2021 base model:
mod_2021 = SS_output(dir = file.path(shrpoint_path, 'models/base/4A_io'))

# -------------------------------------------------------------------------
# Plot SS3-growth in 2021 and 2024, and SS3-growth in 2024 with Farley

# Ages vector:
ages_vec = seq(from = 0, to = 10, by = 0.25)

# Farley growth:
tmp_df1 = data.frame(ages = ages_vec, mean_len = twoStage_vB(all_ages = ages_vec, Linf_par = 167.5, gamma_par = 0.34,
                                                             k1_par = 3.1, a_0 = -0.01, k2_par = 0.39, alpha_par = 0.82),
                     type = 'Farley et al. (2023)')
# SS3 2021 growth (28+):
tmp_df2 = data.frame(ages = ages_vec[1:29], mean_len = mod_2021$endgrowth$Len_Beg, type = '2021 assessment')

# SS3 2024 growth (40+):
tmp_df3 = data.frame(ages = ages_vec, mean_len = mod_2024$endgrowth$Len_Beg, type = '2024 assessment')

# Merge for plotting:
plot_df = rbind(tmp_df1, tmp_df2, tmp_df3)

# Make plot:
p1 = ggplot(data = plot_df, aes(x = ages, y = mean_len, color = type)) +
      geom_line() +
      xlab('Ages') + ylab('Mean length (cm)') +
      scale_x_continuous(limits = c(0, 10), breaks = seq(from = 0, to = 10, by = 1)) +
      theme(legend.position = c(0.75, 0.25)) +
      guides(color = guide_legend(title = NULL))
ggsave(file.path(shrpoint_path, plot_dir, paste0('growth_1', img_type)), plot = p1,
       width = img_width*0.75, height = 110, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------

# Plot growth by sex (2024):
plot_df = mod_sex_2024$endgrowth %>% dplyr::select(Sex, Age_Beg, Len_Beg)
plot_df = plot_df %>% mutate(Sex = factor(Sex, levels = 1:2, labels = c('Females', 'Males')))

# Make plot:
p1 = ggplot(data = plot_df, aes(x = Age_Beg, y = Len_Beg, color = Sex)) +
  geom_line() +
  xlab('Ages') + ylab('Mean length (cm)') +
  scale_x_continuous(limits = c(0, 28), breaks = seq(from = 0, to = 28, by = 4), labels = c(0:7)) +
  theme(legend.position = c(0.75, 0.25)) +
  guides(color = guide_legend(title = NULL))
ggsave(file.path(shrpoint_path, plot_dir, paste0('growth_sex', img_type)), plot = p1,
       width = img_width*0.75, height = 110, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------

# Plot L-W relationship
plot_df = mod_2024$biology

# Make plot:
p1 = ggplot(data = plot_df, aes(x = Len_mean, y = Wt_F)) +
  geom_line() +
  xlab('Length (cm)') + ylab('Weight (kg)') 
ggsave(file.path(shrpoint_path, plot_dir, paste0('LW', img_type)), plot = p1,
       width = img_width*0.75, height = 110, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------

# Plot natural mortality:
vec_2024 = mod_2024$Natural_Mortality[1,] %>% select(`0`:`40`)
vec_2021 = mod_2021$Natural_Mortality[1,] %>% select(`0`:`28`)

# plot:
plot_df = rbind(data.frame(ages = 0:40, M = as.vector(as.matrix(vec_2024)), type = '2024 assessment'),
                data.frame(ages = 0:28, M = as.vector(as.matrix(vec_2021)), type = '2021 assessment'))

# Make plot:
p1 = ggplot(data = plot_df, aes(x = ages, y = M, color = type)) +
  geom_line() +
  xlab('Ages') + ylab(expression(paste("Natural mortality (", year^{-1}, ")"))) + 
  scale_x_continuous(limits = c(0, 40), breaks = seq(from = 0, to = 40, by = 4), labels = c(0:10)) +
  scale_y_continuous(limits = c(0, 3.5)) +
  theme(legend.position = c(0.75, 0.75)) +
  guides(color = guide_legend(title = NULL))
ggsave(file.path(shrpoint_path, plot_dir, paste0('natmort', img_type)), plot = p1,
       width = img_width*0.75, height = 110, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------

# Plot maturity in current assessment, and compare it with Zudaire 2022
df_2024 = rbind(mod_2024$endgrowth %>% select(Len_Beg, Len_Mat) %>% mutate(type = 'SS3 (2024 assessment)'),
                data.frame(Len_Beg = mod_2024$biology$Len_lo, Len_Mat = maturity_Zudaire(mod_2024$biology$Len_lo), type = 'Zudaire et al. (2022)'))
df_2024 = df_2024 %>% mutate(type = factor(type, levels = c('Zudaire et al. (2022)', 'SS3 (2024 assessment)')))

# Plot:
p1 = ggplot(data = df_2024, aes(x = Len_Beg, y = Len_Mat, color = type)) +
        geom_line() +
        xlab('Length (cm)') + ylab('Maturity')  +
        theme(legend.position = c(0.75, 0.25)) +
        guides(color = guide_legend(title = NULL))

# Plot maturity at age current and 2021 assessment:
age_mat_df = rbind(data.frame(mod_2021$endgrowth %>% select(Age_Beg, Age_Mat) %>% dplyr::rename(Mat = Age_Mat) %>% mutate(type = '2021 assessment')),
                   data.frame(mod_2024$endgrowth %>% select(Age_Beg, Len_Mat) %>% dplyr::rename(Mat = Len_Mat) %>% mutate(type = '2024 assessment')))

# Plot:
p2 = ggplot(data = age_mat_df, aes(x = Age_Beg, y = Mat, color = type)) +
  geom_line() +
  xlab('Age') + ylab('Maturity') +
  scale_x_continuous(limits = c(0, 40), breaks = seq(from = 0, to = 40, by = 4), labels = c(0:10)) +
  theme(legend.position = c(0.75, 0.25)) +
  guides(color = guide_legend(title = NULL))

# Merge:
p3 = grid.arrange(p1, p2, ncol = 1)

ggsave(file.path(shrpoint_path, plot_dir, paste0('maturity', img_type)), plot = p3,
       width = img_width*0.75, height = 160, units = 'mm', dpi = img_res)

