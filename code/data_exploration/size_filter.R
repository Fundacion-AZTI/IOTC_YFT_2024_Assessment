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


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Catch + Nsamp + grid covered by CPC
# Create folder
do_filter = FALSE
dir.create(file.path(shrpoint_path, plot_dir, 'exploration/catch_len_samp_cpc'))

# Color vector:
col_vec = brewer.pal(n = 8, name = 'Dark2')

# Read catch grid data:
load(file.path(shrpoint_path, 'data/processed', 'catchStd_5.RData'))
catch_grid = catchStd
colnames(catch_grid) = str_to_title(colnames(catch_grid))
colnames(catch_grid)[c(6)] = c('FisheryCode')
# Find 4A fisheries catch:
catch_grid$Area = get_4Aarea_from_lonlat(catch_grid$Lon, catch_grid$Lat)
catch_grid = create_4Aarea_cols(catch_grid)
catch_df = catch_grid %>% dplyr::mutate(ModelFishery = paste(FisheryCode, AssessmentAreaName, sep = '_'))
catch_df = catch_df %>% group_by(Grid_id, Year, Fleet, ModelFishery) %>% summarise(Catch = sum(Ncmtfish))

# Read merge size data:
load(file.path(shrpoint_path, 'data/processed', 'mergedStd_5.RData'))
size_grid = mergedStd
colnames(size_grid) = str_to_title(colnames(size_grid))
colnames(size_grid)[c(6)] = c('FisheryCode')
# Find 4A fisheries catch:
size_grid$Area = get_4Aarea_from_lonlat(size_grid$Lon, size_grid$Lat)
size_grid = create_4Aarea_cols(size_grid)
size_df = size_grid %>% dplyr::mutate(ModelFishery = paste(FisheryCode, AssessmentAreaName, sep = '_'))
if(do_filter) size_df = filter_LF_4A_type2(size_df, filter_nsamp = FALSE) # to make sure we use same data as in the assessment
size_df = size_df %>% group_by(Grid_id, Year, Fleet, ModelFishery) %>% summarise(Nsamp = sum(Nfish_samp))

# Find SS fisheries:
all_ss_fish = get_fisheries(config = '4A_io')$fleet_name

# fishery plus group
nfish_plus = 7

for(i in seq_along(all_ss_fish)) {
  
  # Read catch dat:
  fsh_dat = catch_df %>% dplyr::filter(ModelFishery %in% all_ss_fish[i])
  fsh_dat = fsh_dat %>% mutate(decade = cut(Year, breaks = c(seq(from = 1950, to = 2020, by = 10), 2024), right = FALSE))
  all_decades = sort(unique(fsh_dat$decade))
  n_decades = length(all_decades)
  
  # Read size dat:
  len_dat = size_df %>% dplyr::filter(ModelFishery %in% all_ss_fish[i])
  len_dat = len_dat %>% mutate(decade = cut(Year, breaks = c(seq(from = 1950, to = 2020, by = 10), 2024), right = FALSE))
  
  for(j in 1:n_decades) {
    
    # ---
    # Plot 1: catch per year and fleet
    filter_catch = fsh_dat %>% dplyr::filter(decade == all_decades[j])
    n_fleets = length(unique(filter_catch$Fleet))
    
    if(n_fleets > nfish_plus) {
      summ_dat = filter_catch %>% group_by(Fleet) %>% summarise(Catch = sum(Catch))
      summ_dat = summ_dat[order(summ_dat$Catch, decreasing = TRUE), ]
      summ_dat = summ_dat %>% mutate(Fleet_label = Fleet) %>% select(-Catch)
      summ_dat$Fleet_label[nfish_plus:nrow(summ_dat)] = 'Other'
      # Merge:
      filter_catch = left_join(filter_catch, summ_dat)
      filter_catch = filter_catch %>% mutate(Fleet = Fleet_label)
    }
    
    # Aggregate:
    tmp_catch = filter_catch %>% group_by(Year, Fleet) %>% summarise(Catch = sum(Catch))
    
    # Define colors for this decade:
    tmp_fleets = unique(tmp_catch$Fleet)
    col_tmp = col_vec[1:length(tmp_fleets)]
    names(col_tmp) = tmp_fleets
    
    #Time factor:
    tmp_catch = tmp_catch %>% mutate(Year = factor(Year))
    
    d1 = ggplot(tmp_catch, aes(x = Year, y = Catch, color = Fleet, fill = Fleet)) + 
      geom_col() +
      ylab('Catch (mt)') + xlab(NULL) +
      scale_fill_manual(values = col_tmp) +
      scale_color_manual(values = col_tmp) +
      theme(legend.position = 'top',
            legend.key.size = unit(0.25, 'cm'),
            legend.text = element_text(size=6),
            axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
      guides(fill=guide_legend(nrow=1,title=NULL), 
             color=guide_legend(nrow=1,title=NULL))
    
    # ---
    # Plot 2: Nsamp year and fleet
    filter_size = len_dat %>% dplyr::filter(decade == all_decades[j])
    # Only proceed if length info available
    if(nrow(filter_size) > 0) {
      filter_size$Fleet[!(filter_size$Fleet %in% tmp_fleets)] = 'Other'
      # Aggregate:
      tmp_size = filter_size %>% group_by(Year, Fleet) %>% summarise(Nsamp = sum(Nsamp))
      # Time factor:
      tmp_size = tmp_size %>% mutate(Year = factor(Year, levels = levels(tmp_catch$Year)))
      
      d2 = ggplot(tmp_size, aes(x = Year, y = Nsamp, color = Fleet, fill = Fleet)) + 
        geom_col() +
        ylab('N fish sampled') + xlab(NULL) +
        scale_fill_manual(values = col_tmp) +
        scale_color_manual(values = col_tmp) +
        scale_x_discrete(limits = levels(tmp_catch$Year)) +
        theme(legend.position = 'none',
              axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5))
    } else { d2 = ggplot() }
    
    # ---
    # Plot 3: Ngrids catch 
    gr_catch = filter_catch %>% group_by(Year, Fleet) %>% summarise(n_grid = length(unique(Grid_id)))
    gr_catch = gr_catch %>% mutate(Year = factor(Year))
    
    d3 = ggplot(data = gr_catch, aes(x = Year, y = n_grid)) +
      geom_bar(aes(fill = Fleet), position =position_dodge(preserve = "single"), stat="identity") +
      ylab('Number of grids (catch)') + xlab(NULL) +
      scale_fill_manual(values = col_tmp) +
      scale_x_discrete(limits = levels(tmp_catch$Year)) +
      theme(legend.position = 'none',
            axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5))
    
    # ---
    # Plot 4: Ngrids size
    if(nrow(filter_size) > 0) {
      gr_size = filter_size %>% group_by(Year, Fleet) %>% summarise(n_grid = length(unique(Grid_id)))
      gr_size = gr_size %>% mutate(Year = factor(Year))      
      
      d4 = ggplot(data = gr_size, aes(x = Year, y = n_grid)) +
        geom_bar(aes(fill = Fleet), position =position_dodge(preserve = "single"), stat="identity") +
        ylab('Number of grids (size)') + xlab(NULL) +
        scale_fill_manual(values = col_tmp) +
        scale_x_discrete(limits = levels(tmp_catch$Year)) +
        theme(legend.position = 'none',
              axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5))
    } else { d4 = ggplot() }
    
    d5 = grid.arrange(d1, d3, d2, d4, nrow = 2)
    ggsave(file.path(shrpoint_path, plot_dir, 'exploration/catch_len_samp_cpc', paste0(all_ss_fish[i], '-', j, img_type)), plot = d5,
           width = img_width, height = 160, units = 'mm', dpi = img_res)
    
  } # loop by decade
  
} # loop by modelfishery


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Mean length by CPC
# Create folder
dir.create(file.path(shrpoint_path, plot_dir, 'exploration/mlen_cpc'))

# Filter size data:
size_df = filter_LF_4A_type2(size_dat) # to make sure we use same data as in the assessment
size_df = size_df %>% dplyr::mutate(Yr = Year + (Quarter-1)/4, .after = Year)
size_df = size_df %>% group_by(Yr, Fleet, ModelFishery) %>% dplyr::summarise_at(c('Nfish_samp', L_labels),list(sum))
size_tdf =  gather(size_df, 'len_bin', 'Nfish', 5:ncol(size_df))
size_tdf$len_bin = as.numeric(gsub(pattern = 'L', replacement = '', x = size_tdf$len_bin))
size_tdf = size_tdf %>% group_by(Yr, Fleet, ModelFishery) %>% summarise(mlen = weighted.mean(x = len_bin, w = Nfish))

# Find SS fisheries:
all_ss_fish = get_fisheries(config = '4A_io')$fleet_name

for(i in seq_along(all_ss_fish)) {
  
  # Read catch dat:
  tmp_dat = size_tdf %>% dplyr::filter(ModelFishery %in% all_ss_fish[i]) %>% select(-ModelFishery)
  n_fleets = length(unique(tmp_dat$Fleet))
  my_colors = colorRampPalette(brewer.pal(8, "Dark2"))(n_fleets)
  
  # Merge with standard df:
  all_times_df = expand.grid(Yr = seq(from = 1950, to = 2024, by = 0.25),
                             Fleet = unique(tmp_dat$Fleet))
  plot_data_df = left_join(all_times_df, tmp_dat)
  
  p1 = ggplot(data = plot_data_df, aes(x = Yr, y = mlen, color = Fleet)) +
    geom_point(size = 1) +
    geom_line() +
    ylab("Mean length (cm)") + xlab(NULL) +
    scale_color_manual(values = my_colors) +
    coord_cartesian(xlim = c(1950, 2024)) +
    theme(legend.position = 'bottom')
  ggsave(file.path(shrpoint_path, plot_dir, 'exploration/mlen_cpc', paste0(all_ss_fish[i], img_type)), plot = p1,
         width = img_width, height = 160, units = 'mm', dpi = img_res)
  
} # loop by modelfishery
