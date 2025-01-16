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
dir.create(file.path(impact_folder, 'dynamic-2'), showWarnings = FALSE)

# Close none fleet:
mod_name = 'none'
copy_SS_inputs(dir.old = file.path(impact_folder, 'base'), 
               dir.new = file.path(impact_folder, 'dynamic-2', mod_name), 
               overwrite = TRUE, copy_par = TRUE)
# Run model:
r4ss::run(dir = file.path(impact_folder, 'dynamic-2', mod_name), show_in_console = FALSE,
          exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')

# Close per fishery group (individually)
fish_groups = unique(fish_id)
for(i in seq_along(fish_groups)) {
  
  mod_name = fish_groups[i]
  copy_SS_inputs(dir.old = file.path(impact_folder, 'base'), 
                 dir.new = file.path(impact_folder, 'dynamic-2', mod_name), 
                 overwrite = TRUE, copy_par = TRUE)
  # Change input catch:
  tmp_dat = SS_readdat(file = file.path(impact_folder, 'dynamic-2', mod_name, 'data.ss'))
  fish_sel = which(fish_id %in% fish_groups[i])
  tmp_dat$catch$catch[tmp_dat$catch$fleet %in% fish_sel] = 0
  SS_writedat(datlist = tmp_dat, outfile = file.path(impact_folder, 'dynamic-2', mod_name, 'data.ss'), overwrite = TRUE)
  # Run model:
  r4ss::run(dir = file.path(impact_folder, 'dynamic-2', mod_name), show_in_console = FALSE,
            exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')
  
}

# Close all fleets:
mod_name = 'all'
copy_SS_inputs(dir.old = file.path(impact_folder, 'base'), 
               dir.new = file.path(impact_folder, 'dynamic-2', mod_name), 
               overwrite = TRUE, copy_par = TRUE)
# Change input catch:
tmp_dat = SS_readdat(file = file.path(impact_folder, 'dynamic-2', mod_name, 'data.ss'))
tmp_dat$catch$catch = 0
SS_writedat(datlist = tmp_dat, outfile = file.path(impact_folder, 'dynamic-2', mod_name, 'data.ss'), overwrite = TRUE)
# Run model:
r4ss::run(dir = file.path(impact_folder, 'dynamic-2', mod_name), show_in_console = FALSE,
          exe = file.path(shrpoint_path, 'ss3_3022.exe'), extras = '-nohess')

# -------------------------------------------------------------------------
# Read outputs:
max_year = 2024

# Read 'none'
output = r4ss::SS_output(dir = file.path(impact_folder, 'dynamic-2', 'none'),  
                         covar = FALSE, NoCompOK = TRUE, verbose = FALSE, printstats = FALSE)
subdat = output$derived_quants[grep(pattern = 'SSB_', x = output$derived_quants$Label)[-c(1,2)], ] # remove Init and Virg
subdat = subdat %>% mutate(Year = ssts2yq(as.numeric(gsub(pattern = 'SSB_', replacement = '', x = Label)))) %>% select(Year, Value)
subdat = subdat %>% dplyr::filter(Year < max_year)
SPB = subdat$Value
tstep = subdat$Year

# Read 'all'
output = r4ss::SS_output(dir = file.path(impact_folder, 'dynamic-2', 'all'),  
                         covar = FALSE, NoCompOK = TRUE, verbose = FALSE, printstats = FALSE)
subdat = output$derived_quants[grep(pattern = 'SSB_', x = output$derived_quants$Label)[-c(1,2)], ] # remove Init and Virg
subdat = subdat %>% mutate(Year = ssts2yq(as.numeric(gsub(pattern = 'SSB_', replacement = '', x = Label)))) %>% select(Year, Value)
subdat = subdat %>% dplyr::filter(Year < max_year)
SPBNoFishing = subdat$Value

# Read each scenario:
all_scenarios = list.files(file.path(impact_folder, 'dynamic-2'))
all_scenarios = all_scenarios[-which(all_scenarios %in% c('all', 'none'))] # 'none' always first
all_scenarios = all_scenarios[c(8,1,9,4,3,5,6,7,2)] # reorder if needed, not mandatory

mat = matrix(0, length(all_scenarios), length(SPBNoFishing))
mat_d = matrix(0, length(all_scenarios), length(SPBNoFishing))
mat_c = matrix(0, length(all_scenarios), length(SPBNoFishing))
for(i in seq_along(all_scenarios)) {
  output = r4ss::SS_output(dir = file.path(impact_folder, 'dynamic-2', all_scenarios[i]),  
                           covar = FALSE, NoCompOK = TRUE, verbose = FALSE, printstats = FALSE)
  subdat = output$derived_quants[grep(pattern = 'SSB_', x = output$derived_quants$Label)[-c(1,2)], ] # remove Init and Virg
  subdat = subdat %>% mutate(Year = ssts2yq(as.numeric(gsub(pattern = 'SSB_', replacement = '', x = Label)))) %>% select(Year, Value)
  subdat = subdat %>% dplyr::filter(Year < max_year)
  mat[i,] = subdat$Value
  rownames(mat) = all_scenarios
  print(i)
}
write.csv(mat, file = file.path(impact_folder, 'merged_df-2.csv'))

# Postprocessing:
SPB_d = SPBNoFishing - SPB
for(i in 1:length(all_scenarios)) mat_d[i,] = mat[i,]-SPB
temp = apply(mat_d,2,sum)
for(i in 1:length(all_scenarios)) mat_d[i,] = mat_d[i,]/temp
for(i in 1:length(all_scenarios)) mat_d[i,] = mat_d[i,] * SPB_d
mat_d[,1] = 0
mat_c = apply(mat_d,2,cumsum)
for(i in 1:length(all_scenarios)) mat_c[i,] = mat_c[i,] + SPB


# -------------------------------------------------------------------------
# Make plot:
png(file.path(shrpoint_path, plot_dir, paste0('impact_plot-2', img_type)), res = img_res, width=img_width, height=130, units = 'mm')
par(mfrow = c(1,1), mar = c(3,4,0.5,0.5))
plot(tstep, SPBNoFishing, ylab="Spawning biomass (t)", ylim=c(0,max(SPBNoFishing, na.rm=TRUE)), type="n", lwd=2, col="black", xlab="")
lines(tstep, SPBNoFishing, col='gray', lwd=2)
lines(tstep, SPB, col='gray', lwd=2)
for(i in 1:nrow(mat)) {
  col_j = fleet_col[names(fleet_col) %in% rownames(mat)[i]]
  if(i == 1) {
    polygon(c(tstep,rev(tstep)), c(SPB, rev(mat_c[i,])), col=col_j)
  } else {
    polygon(c(tstep,rev(tstep)),c(mat_c[i-1,], rev(mat_c[i,])), col=col_j)
  }
}
legend('bottomleft', legend = names(fleet_col), fill = fleet_col)
dev.off()

