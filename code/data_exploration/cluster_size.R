rm(list = ls())

# Read path and parameters for plots:
source('sharepoint_path.R')
source(here('code', 'parameters_for_plots.R'))
source(here('code', 'auxiliary_functions.R'))

# Length bins
L_labels  =  c(Paste("L0",seq(10,98,2)), Paste("L",seq(100,198,2))) 

# Create folder where outputs will be saved
out_folder = file.path(shrpoint_path, 'output', 'freqTree')
dir.create(out_folder, showWarnings = FALSE)

# -------------------------------------------------------------------------
# Read data:
load(file.path(shrpoint_path, 'data/processed', 'mergedStd_5.RData'))
size_dat = mergedStd
# Filter as in the SS3 input:
colnames(size_dat) = str_to_title(colnames(size_dat))
colnames(size_dat)[c(6)] = c('FisheryCode')
size_dat$Area = get_4Aarea_from_lonlat(size_dat$Lon, size_dat$Lat)
size_dat = create_4Aarea_cols(size_dat)
size_dat = size_dat %>% 
  dplyr::mutate(ModelFishery = paste(FisheryCode, AssessmentAreaName))
size_dat = filter_LF_4A_type2(size_dat) # filter before clustering

# -------------------------------------------------------------------------
# FishFreqTree method:

# Select fisherycode
selFishery = 'FS'

# Create folders to save results:
save_dir = file.path(out_folder, selFishery)
dir.create(save_dir, showWarnings = FALSE)
save_dir = paste0(save_dir, '/')

#Organize data:
sample_data = size_dat %>% dplyr::filter(FisheryCode == selFishery)
sample_data = sample_data %>% dplyr::select(Grid_id, Year, Quarter, Lat, Lon, Nfish_samp, L010:L198)
sample_data = sample_data %>% group_by(Lon, Lat, Year, Quarter) %>%
                dplyr::summarise_at(c('Nfish_samp', L_labels),list(sum))
sample_data = sample_data %>% ungroup() %>% mutate(across(-c(1:5))/rowSums(across(-c(1:5))))
colnames(sample_data) = tolower(colnames(sample_data))

# Specify some parameters to run clustering
fcol = 6 # first column with length data
lcol = ncol(sample_data) # last column with length data
bins = as.numeric(gsub(pattern = 'l', replacement = '', x = colnames(sample_data)[fcol:lcol]))
colnames(sample_data)[fcol:lcol] = bins
Nsplit = 3 # number of splits

# Run clustering:
LF_Tree = run_regression_tree(sample_data, fcol, lcol, bins, Nsplit, save_dir)
var_exp = round(sum(LF_Tree$Record$Var_explained)*100, digits = 2)
# LF_Tree$Record
# make.split.map(LF_Tree$LF, Nsplit, save_dir, width = 10, height = 7)

# Prepare data for plotting:
find_flag = grep(pattern = 'Flag', x = colnames(LF_Tree$LF)) # select last Flag
plot_data = sample_data
plot_data$cluster = LF_Tree$LF %>% pull(max(find_flag))

# Plot 1:
plot_dat1 = tidyr::gather(plot_data, 'len_bin', 'prop', which(colnames(plot_data) %in% bins))
plot_dat1 = plot_dat1 %>% mutate(freq = prop*nfish_samp)
plot_dat1 = plot_dat1 %>% group_by(quarter, cluster, len_bin) %>% summarise(freq = sum(freq))
tmp_Nsamp = plot_dat1 %>% group_by(quarter, cluster) %>% summarise(nsamp = sum(freq), .groups = 'drop')
plot_dat1 = inner_join(plot_dat1, tmp_Nsamp)
plot_dat1 = plot_dat1 %>% mutate(prop = freq/nsamp)
plot_dat1$len_bin = as.numeric(plot_dat1$len_bin)
plot_dat1 = plot_dat1 %>% mutate(quarter = factor(quarter, levels = 1:4, labels = paste0('Quarter ', 1:4)), 
                                 cluster = factor(cluster, levels = 1:(Nsplit+1)))

p1 = ggplot(data = plot_dat1, aes(x = len_bin, y = prop)) +
  geom_line(aes(color = cluster)) +
  theme(legend.position = 'bottom') +
  guides(color=guide_legend(title="Cluster: ")) +
  xlab('Length (cm)') + ylab('Proportion') +
  ggtitle(label = paste0('Var explained: ', var_exp, '%')) +
  facet_wrap(~quarter, scales = 'free_y')
ggsave(filename = paste0('lenfreq_cluster_qrt', img_type), path = save_dir, plot = p1, 
       width = img_width, height = 140, units = 'mm', dpi = img_res)

# Make cluster grid plot by season:
plot_dat2 = plot_data %>% dplyr::select(quarter, cluster, lon, lat) 
plot_dat2 = plot_dat2 %>% mutate(quarter = factor(quarter, levels = 1:4, labels = paste0('Quarter ', 1:4)), 
                                 cluster = factor(cluster, levels = 1:(Nsplit+1)))
plot_dat2 = plot_dat2 %>% st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

p2 = ggplot() +
  geom_sf(data = plot_dat2, aes(fill = cluster, color = cluster)) + 
  guides(color=guide_legend(title="Cluster: "), fill=guide_legend(title="Cluster: ")) +
  theme(legend.position = 'bottom', legend.text = element_text(size=8)) +
  facet_wrap(~quarter)
p2 = add_sf_map(p2)
ggsave(filename = paste0('lenfreq_cluster_grid', img_type), path = save_dir, plot = p2, 
       width = img_width, height = 140, units = 'mm', dpi = img_res)
