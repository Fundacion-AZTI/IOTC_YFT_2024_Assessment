# Produce plots related to size filtering that are included in the report
rm(list = ls())

# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code', 'parameters_for_plots.R'))
source(here('code', 'auxiliary_functions.R'))

# Color vector:
col_vec = brewer.pal(n = 8, name = 'Dark2')

# Length bins
L_labels  =  c(Paste("L0",seq(10,98,2)), Paste("L",seq(100,198,2))) 

# Read size data
size_dat = read_csv(file.path(shrpoint_path, 'data/processed', 'agg-size-original.csv'))
size_dat = size_dat %>% mutate(Time = paste(Year, Quarter, sep = '-'), .after = 'Quarter')

# Read data in:
catch_dat = read_csv(file.path(shrpoint_path, 'data/processed', 'agg-catch.csv'))
catch_dat = catch_dat %>% mutate(Time = paste(Year, Quarter, sep = '-'), .after = 'Quarter')

# Fleet plus group:
nfish_plus = 8

# -------------------------------------------------------------------------
# Plot catch per CPC, and Nsamp per CPC, and Mean length per CPC

# Select fisheries to plot:
sel_fisheries = c('OT_4') # no filtering
# sel_fisheries = c('LL_1b', 'LL_4') # do filtering

# Year range:
yr_range = c(1990, 2024)

# Do size filtering?
do_filter = FALSE

# ----
# Catch plot:
fish_catch = catch_dat %>% dplyr::filter(ModelFishery %in% sel_fisheries)
fish_catch = fish_catch %>% group_by(Year, Fleet, ModelFishery) %>% summarise(Catch = sum(NCmtFish))
n_fleets = length(unique(fish_catch$Fleet))

if(n_fleets > nfish_plus) {
  summ_dat = fish_catch %>% group_by(Fleet) %>% summarise(Catch = sum(Catch))
  summ_dat = summ_dat[order(summ_dat$Catch, decreasing = TRUE), ]
  summ_dat = summ_dat %>% mutate(Fleet_label = Fleet) %>% select(-Catch)
  summ_dat$Fleet_label[nfish_plus:nrow(summ_dat)] = 'Other'
  # Merge:
  fish_catch = left_join(fish_catch, summ_dat)
  fish_catch = fish_catch %>% mutate(Fleet = Fleet_label)
}

# Aggregate again:
tmp_catch = fish_catch %>% group_by(Year, Fleet, ModelFishery) %>% summarise(Catch = sum(Catch))

# Define colors:
tmp_fleets = unique(tmp_catch$Fleet)
col_tmp = col_vec[1:length(tmp_fleets)]
names(col_tmp) = tmp_fleets

# Plot catch:
d1 = ggplot(tmp_catch, aes(x = Year, y = Catch, color = Fleet, fill = Fleet)) + 
  geom_col() +
  ylab('Catch (mt)') + xlab(NULL) +
  scale_fill_manual(values = col_tmp) +
  scale_color_manual(values = col_tmp) +
  scale_x_continuous(limits = yr_range, breaks = c(1960, 1980, 2000, 2020)) +
  theme(legend.position = 'top',
        legend.key.size = unit(0.35, 'cm'),
        legend.text = element_text(size=8),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  guides(fill=guide_legend(nrow=1,title=NULL), 
         color=guide_legend(nrow=1,title=NULL)) +
  facet_wrap(~ ModelFishery)

# Nfish sampled plot:
size_dat_filt = size_dat
if(do_filter) size_dat_filt = filter_LF_4A_type2(size_dat_filt) # do filtering
fish_size = size_dat_filt %>% dplyr::filter(ModelFishery %in% sel_fisheries)
fish_size = fish_size %>% mutate(Fleet = if_else(Fleet %in% names(col_tmp), Fleet, 'Other'))
fish_size = fish_size %>% group_by(Year, Fleet, ModelFishery) %>% summarise(Nsamp = sum(Nfish_samp))

# For plotting:
tmp_size = fish_size

# Plot size:
d2 = ggplot(tmp_size, aes(x = Year, y = Nsamp, color = Fleet, fill = Fleet)) + 
  geom_col() +
  ylab('Number of fish sampled') + xlab(NULL) +
  scale_fill_manual(values = col_tmp) +
  scale_color_manual(values = col_tmp) +
  scale_x_continuous(limits = yr_range, breaks = c(1960, 1980, 2000, 2020)) +
  theme(legend.position = 'none',
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  guides(fill=guide_legend(nrow=1,title=NULL), 
         color=guide_legend(nrow=1,title=NULL)) +
  facet_wrap(~ ModelFishery)

# Mean length plot:
size_df = size_dat_filt %>% dplyr::filter(ModelFishery %in% sel_fisheries)
size_df = size_df %>% dplyr::mutate(Yr = Year + (Quarter-1)/4, .after = Year)
size_df = size_df %>% mutate(Fleet = if_else(Fleet %in% names(col_tmp), Fleet, 'Other'))
size_df = size_df %>% group_by(Yr, Fleet, ModelFishery) %>% dplyr::summarise_at(c('Nfish_samp', L_labels),list(sum))
size_tdf =  gather(size_df, 'len_bin', 'Nfish', 5:ncol(size_df))
size_tdf$len_bin = as.numeric(gsub(pattern = 'L', replacement = '', x = size_tdf$len_bin))
size_tdf = size_tdf %>% group_by(Yr, Fleet, ModelFishery) %>% summarise(mlen = weighted.mean(x = len_bin, w = Nfish))

# For plotting:
all_times_df = expand.grid(Yr = seq(from = 1950, to = 2024, by = 0.25),
                           Fleet = unique(size_tdf$Fleet), ModelFishery = unique(size_tdf$ModelFishery))
plot_size = left_join(all_times_df, size_tdf)

d3 = ggplot(data = plot_size, aes(x = Yr, y = mlen, color = Fleet)) +
  geom_point(size = 1) +
  geom_line() +
  ylab("Mean length (cm)") + xlab(NULL) +
  scale_color_manual(values = col_tmp) +
  scale_x_continuous(limits = yr_range, breaks = c(1960, 1980, 2000, 2020)) +
  theme(legend.position = 'none',
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  facet_wrap(~ ModelFishery)

# Merge plots:
d4 = grid.arrange(d1, d2, d3, ncol = 1)

ggsave(file.path(shrpoint_path, plot_dir, paste0(substr(sel_fisheries[1], start = 1, stop = 2), '_catch-size_CPC', img_type)), 
       plot = d4, width = img_width*0.75, height = 220, units = 'mm', dpi = img_res)
