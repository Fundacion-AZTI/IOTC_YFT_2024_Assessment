rm(list = ls())

# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code', 'parameters_for_plots.R'))
source(here('code', 'auxiliary_functions.R'))

# Length bins
L_labels  =  c(Paste("L0",seq(10,98,2)), Paste("L",seq(100,198,2))) 

# Read size data
size_dat = read_csv(file.path(shrpoint_path, 'data/processed', 'agg-size-original.csv'))
size_dat = size_dat %>% mutate(Time = paste(Year, Quarter, sep = '-'), .after = 'Quarter')

# Read data in:
catch_dat = read_csv(file.path(shrpoint_path, 'data/processed', 'agg-catch.csv'))
catch_dat = catch_dat %>% mutate(Time = paste(Year, Quarter, sep = '-'), .after = 'Quarter')

# Create folder to save plots:
dir.create(file.path(shrpoint_path, plot_dir, 'exploration'))
n_fig_dim = 42 # 6 cols * 7 rows = 42


# -------------------------------------------------------------------------
# Plot Nsamp + size by CPC and year ---------------------------------------

# Now create subfolders:
dir.create(file.path(shrpoint_path, plot_dir, 'exploration/Nsamp_by_year_cpc'))
dir.create(file.path(shrpoint_path, plot_dir, 'exploration/RQ_by_year_cpc'))
dir.create(file.path(shrpoint_path, plot_dir, 'exploration/size_by_year'))
dir.create(file.path(shrpoint_path, plot_dir, 'exploration/size_by_year_cpc'))

# Find SS fisheries:
all_ss_fish = unique(size_dat$ModelFishery)

for(i in seq_along(all_ss_fish)) {
  
  fsh_dat = size_dat %>% dplyr::filter(ModelFishery %in% all_ss_fish[i])

  # Plot 0: aggregated by Time size comps
  tmp_dat0 = fsh_dat %>% group_by(Time) %>% dplyr::summarise_at(c('Nfish_samp', L_labels),list(sum)) %>% 
    ungroup() %>% mutate(across(-c(1,2))/rowSums(across(-c(1,2))))
  tmp_dat0 = gather(tmp_dat0, 'len_bin', 'prop', 3:ncol(tmp_dat0))
  tmp_dat0$len_bin = as.numeric(gsub(pattern = 'L', replacement = '', x = tmp_dat0$len_bin))
  all_ts = unique(tmp_dat0$Time)
  n_groups = ceiling(length(all_ts)/n_fig_dim) # 6 cols * 7 rows = 42
  
  for(j in 1:n_groups) {
    these_ts = all_ts[(1:n_fig_dim) + (j-1)*n_fig_dim]
    plot_dat = tmp_dat0 %>% dplyr::filter(Time %in% these_ts[!is.na(these_ts)])
    plot_dat = plot_dat %>% mutate(Label = if_else(Nfish_samp < 10000, paste0(Time, '|n=', round(Nfish_samp)), paste0(Time, '|n>9999')))
    
    d0 = ggplot(plot_dat, aes(x = len_bin, y = prop)) + 
      geom_line() +
      ylab(NULL) + xlab('Length (cm)') +
      coord_cartesian(ylim = c(0, 0.2), xlim = c(2, 198)) +
      theme_classic() +
      facet_wrap( ~ Label, ncol = 6)
    ggsave(file.path(shrpoint_path, plot_dir, 'exploration/size_by_year', paste0(all_ss_fish[i], '-', j, img_type)), plot = d0,
           width = img_width, height = 200, units = 'mm', dpi = img_res)
  }
    
  # Plot 1: size comps by CPC
  tmp_dat1 = fsh_dat %>% group_by(Time, Fleet) %>% dplyr::summarise_at(L_labels,list(Sum)) %>% 
                  ungroup() %>% mutate(across(-c(1,2))/rowSums(across(-c(1,2))))
  tmp_dat1 = gather(tmp_dat1, 'len_bin', 'prop', 3:ncol(tmp_dat1))
  tmp_dat1$len_bin = as.numeric(gsub(pattern = 'L', replacement = '', x = tmp_dat1$len_bin))
  n_col = length(unique(tmp_dat1$Fleet))
  tmp_col = colorRampPalette(brewer.pal(8, "Dark2"))(n_col)
  
  for(j in 1:n_groups) {
    these_ts = all_ts[(1:n_fig_dim) + (j-1)*n_fig_dim]
    plot_dat = tmp_dat1 %>% dplyr::filter(Time %in% these_ts[!is.na(these_ts)])
    
    d1 = ggplot(plot_dat, aes(x = len_bin, y = prop, color = Fleet)) + 
      geom_line() +
      ylab(NULL) + xlab('Length (cm)') +
      scale_color_manual(values = tmp_col) +
      coord_cartesian(ylim = c(0, 0.2), xlim = c(2, 198)) +
      theme_classic() +
      theme(legend.position = 'bottom') +
      facet_wrap( ~ Time, ncol = 6)
    ggsave(file.path(shrpoint_path, plot_dir, 'exploration/size_by_year_cpc', paste0(all_ss_fish[i], '-', j, img_type)), plot = d1,
           width = img_width, height = 200, units = 'mm', dpi = img_res)
  }
  
  # Plot 2: reporting quality
  tmp_dat2 = fsh_dat %>% group_by(Time, Fleet) %>% summarise(RQ = mean(Quality))
  
  for(j in 1:n_groups) {
    these_ts = all_ts[(1:n_fig_dim) + (j-1)*n_fig_dim]
    plot_dat = tmp_dat2 %>% dplyr::filter(Time %in% these_ts[!is.na(these_ts)])
    
    d2 = ggplot(plot_dat, aes(x = Fleet, y = RQ, color = Fleet, fill = Fleet)) + 
      geom_col() +
      ylab('Reporting quality score') + xlab(NULL) +
      scale_fill_manual(values = tmp_col) +
      scale_color_manual(values = tmp_col) +
      theme_classic() +
      theme(legend.position = 'none',
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      facet_wrap( ~ Time, ncol = 6)
    ggsave(file.path(shrpoint_path, plot_dir, 'exploration/RQ_by_year_cpc', paste0(all_ss_fish[i], '-', j, img_type)), plot = d2,
           width = img_width, height = 200, units = 'mm', dpi = img_res)
  }

  # Plot 3: N fish sampled
  tmp_dat3 = fsh_dat %>% group_by(Time, Fleet) %>% summarise(Nsamp = sum(Nfish_samp))

  for(j in 1:n_groups) {
    these_ts = all_ts[(1:n_fig_dim) + (j-1)*n_fig_dim]
    plot_dat = tmp_dat3 %>% dplyr::filter(Time %in% these_ts[!is.na(these_ts)])
    
    d3 = ggplot(plot_dat, aes(x = Fleet, y = Nsamp, color = Fleet, fill = Fleet)) + 
      geom_col() +
      ylab('N fish sampled') + xlab(NULL) +
      scale_fill_manual(values = tmp_col) +
      scale_color_manual(values = tmp_col) +
      scale_y_continuous(n.breaks = 3) +
      theme_classic() +
      theme(legend.position = 'none',
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
      facet_wrap( ~ Time, ncol = 6, scales = 'free_y')
    ggsave(file.path(shrpoint_path, plot_dir, 'exploration/Nsamp_by_year_cpc', paste0(all_ss_fish[i], '-', j, img_type)), plot = d3,
           width = img_width, height = 200, units = 'mm', dpi = img_res) 
  }
  
}


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Plot catch per Fleet:

# Create folder
dir.create(file.path(shrpoint_path, plot_dir, 'exploration/catch_by_year_cpc'))

# Find SS fisheries:
all_ss_fish = unique(catch_dat$ModelFishery)

for(i in seq_along(all_ss_fish)) {
  
  fsh_dat = catch_dat %>% dplyr::filter(ModelFishery %in% all_ss_fish[i])
  tmp_dat2 = fsh_dat %>% group_by(Year, Fleet) %>% summarise(Catch = sum(NCmtFish))
  n_fleets = length(unique(tmp_dat2$Fleet))

  if(n_fleets > 8) {
    summ_dat = tmp_dat2 %>% group_by(Fleet) %>% summarise(Catch = sum(Catch))
    summ_dat = summ_dat[order(summ_dat$Catch, decreasing = TRUE), ]
    summ_dat = summ_dat %>% mutate(Fleet_label = Fleet) %>% select(-Catch)
    summ_dat$Fleet_label[8:nrow(summ_dat)] = 'Other fleets'
    # Merge:
    tmp_dat2 = left_join(tmp_dat2, summ_dat)
    tmp_dat2 = tmp_dat2 %>% mutate(Fleet = Fleet_label)
    tmp_dat2 = tmp_dat2 %>% group_by(Year, Fleet) %>% summarise(Catch = sum(Catch))
  }
  
  d2 = ggplot(tmp_dat2, aes(x = Year, y = Catch, color = Fleet, fill = Fleet)) + 
    geom_col() +
    ylab('Catch (mt)') + xlab(NULL) +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") +
    theme_classic() +
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(file.path(shrpoint_path, plot_dir, 'exploration/catch_by_year_cpc', paste0(all_ss_fish[i], img_type)), plot = d2,
         width = img_width, height = 130, units = 'mm', dpi = img_res)

}



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Compare HD vs Maldives size: --------------------------------------------

# Create folder
dir.create(file.path(shrpoint_path, plot_dir, 'exploration/size_HD_fleetMDV'))

# Filter HD data:
fsh_dat = size_dat[grep(pattern = 'HD', x = size_dat$ModelFishery), ]

# Find number of plot groups:
all_ts = unique(fsh_dat$Time)
n_groups = ceiling(length(all_ts)/n_fig_dim) # 6 cols * 7 rows = 42

# For all fleets:
all_dat = fsh_dat %>% group_by(Time) %>% dplyr::summarise_at(c('Nfish_samp', L_labels),list(sum)) %>% 
  ungroup() %>% mutate(across(-c(1,2))/rowSums(across(-c(1,2))))
all_dat = gather(all_dat, 'len_bin', 'prop', 3:ncol(all_dat))
all_dat$len_bin = as.numeric(gsub(pattern = 'L', replacement = '', x = all_dat$len_bin))

# For MDV fleet:
mdv_dat = fsh_dat %>% dplyr::filter(Fleet == 'MDV') %>% group_by(Time) %>% dplyr::summarise_at(c('Nfish_samp', L_labels),list(sum)) %>% 
  ungroup() %>% mutate(across(-c(1,2))/rowSums(across(-c(1,2))))
mdv_dat = gather(mdv_dat, 'len_bin', 'prop', 3:ncol(mdv_dat))
mdv_dat$len_bin = as.numeric(gsub(pattern = 'L', replacement = '', x = mdv_dat$len_bin))

for(j in 1:n_groups) {
  
  these_ts = all_ts[(1:n_fig_dim) + (j-1)*n_fig_dim]
  plot_dat1 = all_dat %>% dplyr::filter(Time %in% these_ts[!is.na(these_ts)])
  plot_dat2 = mdv_dat %>% dplyr::filter(Time %in% these_ts[!is.na(these_ts)])
  
  d0 = ggplot(plot_dat1, aes(x = len_bin, y = prop), color = 'black') + 
    geom_line() +
    geom_line(data = plot_dat2, aes(x = len_bin, y = prop), color = 'darksalmon') +
    ylab(NULL) + xlab('Length (cm)') +
    coord_cartesian(ylim = c(0, 0.2), xlim = c(2, 198)) +
    theme_classic() +
    facet_wrap( ~ Time, ncol = 6)
  ggsave(file.path(shrpoint_path, plot_dir, 'exploration/size_HD_fleetMDV', paste0('HD', '-', j, img_type)), plot = d0,
         width = img_width, height = 200, units = 'mm', dpi = img_res)
}

