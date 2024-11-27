rm(list = ls())

# Read path and parameters for plots:
source('sharepoint_path.R')
source('code/parameters_for_plots.R')
source('code/auxiliary_functions.R')

# SS model:
model_folder = file.path(shrpoint_path, 'projections/6_SplitCPUE_tag01_EC0_h0.8')
SS_model = r4ss::SS_output(dir = model_folder, repfile = 'Reportc1.sso')

# Identify fishery groups based on SS_model$definitions (only fisheries):
mod_fisheries = SS_model$definitions %>% dplyr::filter(fleet_type == 1)
fish_id = str_split(string = mod_fisheries$Fleet_name, pattern = '_', simplify = TRUE)[,2]

# Create folder:
impact_folder = file.path(shrpoint_path, 'models/stock_status/impact_plot')
dir.create(impact_folder, showWarnings = FALSE)

# Copy SS inputs to impact folder:
copy_SS_inputs(dir.old = model_folder, file.path(impact_folder, 'base'), overwrite = TRUE, copy_par = TRUE)

# Read base starter and forecast files:
base_starter = SS_readstarter(file.path(impact_folder, 'base', 'starter.ss'))
base_fore = SS_readforecast(file.path(impact_folder, 'base', 'forecast.ss'))

# Change some info in starter and forecast
base_starter$last_estimation_phase = 0
base_starter$init_values_src = 1
base_starter$detailed_age_structure = 2 # make it faster
base_starter$parmtrace = 0
base_starter$prior_like = 0
base_fore$benchmarks = 0
# Write updated files:
SS_writestarter(mylist = base_starter, dir = file.path(impact_folder, 'base'), overwrite = TRUE)
SS_writeforecast(mylist = base_fore, dir = file.path(impact_folder, 'base'), overwrite = TRUE)

# -------------------------------------------------------------------------
# Start dynamic folder to run close fleets:
dir.create(file.path(impact_folder, 'dynamic'), showWarnings = FALSE)

# Close none fleet:
mod_name = 'none'
copy_SS_inputs(dir.old = file.path(impact_folder, 'base'), 
               dir.new = file.path(impact_folder, 'dynamic', mod_name), 
               overwrite = TRUE, copy_par = TRUE)
# Run model:
r4ss::run(dir = file.path(impact_folder, 'dynamic', mod_name), show_in_console = FALSE,
          exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')

# Close per fishery group (sequentially)
fish_groups = unique(fish_id)
fish_groups = fish_groups[c(4,5,8,2,1,9,3,7,6)] # reorder if needed, not mandatory
for(i in seq_along(fish_groups)) {
  
  mod_name = paste(fish_groups[1:i], collapse = '-')
  copy_SS_inputs(dir.old = file.path(impact_folder, 'base'), 
                 dir.new = file.path(impact_folder, 'dynamic', mod_name), 
                 overwrite = TRUE, copy_par = TRUE)
  # Change input catch:
  tmp_dat = SS_readdat(file = file.path(impact_folder, 'dynamic', mod_name, 'data.ss'))
  fish_sel = which(fish_id %in% fish_groups[1:i])
  tmp_dat$catch$catch[tmp_dat$catch$fleet %in% fish_sel] = 0
  SS_writedat(datlist = tmp_dat, outfile = file.path(impact_folder, 'dynamic', mod_name, 'data.ss'), overwrite = TRUE)
  # Run model:
  r4ss::run(dir = file.path(impact_folder, 'dynamic', mod_name), show_in_console = FALSE,
            exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
  
}


# -------------------------------------------------------------------------
# Read outputs:

all_scenarios = list.files(file.path(impact_folder, 'dynamic'))
all_scenarios = c('none', all_scenarios[-which(all_scenarios == 'none')]) # 'none' always first
save_df = list()
for(i in seq_along(all_scenarios)) {
  output = r4ss::SS_output(dir = file.path(impact_folder, 'dynamic', all_scenarios[i]),  
                           covar = FALSE, NoCompOK = TRUE, verbose = FALSE, printstats = FALSE)
  subdat = output$derived_quants[grep(pattern = 'SSB_', x = output$derived_quants$Label)[-c(1,2)], ] # remove Init and Virg
  subdat = subdat %>% mutate(Year = ssts2yq(as.numeric(gsub(pattern = 'SSB_', replacement = '', x = Label)))) %>% select(Year, Value)
  tmpdat = subdat %>% mutate(scenario = all_scenarios[i], id = i-1)
  rownames(tmpdat) = NULL
  save_df[[i]] = tmpdat
  print(i)
}
merged_df = bind_rows(save_df)
merged_df = merged_df %>% dplyr::filter(Year < 2024) # Select years
write.csv(merged_df, file = file.path(impact_folder, 'merged_df.csv'), row.names = FALSE)

# -------------------------------------------------------------------------
# Make plot:
# my_cols = r4ss::rich.colors.short(n = max(merged_df$id))

# Make base plot:
p1 = ggplot(data = merged_df %>% dplyr::filter(id == 0), aes(x = Year, y = Value)) +
  geom_line() + xlab(NULL) + ylab('Spawning biomass (t)') 

# Add polygons:
for(j in 1:max(merged_df$id)) {
  tmp_dat1 = merged_df %>% dplyr::filter(id %in% (j-1))
  tmp_dat2 = merged_df %>% dplyr::filter(id %in% j)
  poly_df = data.frame(Year = c(tmp_dat1$Year, rev(tmp_dat2$Year)), Value = c(tmp_dat1$Value, rev(tmp_dat2$Value)))
  label_j = as.vector(str_split(unique(tmp_dat2$scenario), pattern = '-', simplify = TRUE))
  label_j = label_j[length(label_j)]
  col_j = fleet_col[names(fleet_col) %in% label_j]
  p1 = p1 + geom_polygon(data = poly_df, aes(x = Year, y = Value), color = col_j, fill = col_j) +
    annotate('text', x = min(tmp_dat2$Year)+5, y = 200000*j, label = label_j,  color = col_j, hjust = 0)
}
p1 = p1 + theme_classic() + theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) 
ggsave(file.path(shrpoint_path, plot_dir, paste0('impact_plot', img_type)), plot = p1,
       width = img_width, height = 130, units = 'mm', dpi = img_res)

