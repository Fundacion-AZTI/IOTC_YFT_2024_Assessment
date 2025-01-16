rm(list = ls())

# Read functions;
source("code/ss3_status/SS3_helper.r")
source("code/ss3_status/SS3_support_functions.R")
'%&%' <- function(x, y) paste0(x,y)

# Specify parameters:
mod_dir = 'C:/Use/OneDrive - AZTI/General - IOTC_YFT_2024/models/FinalGrid-usb/'
save_dir = 'C:/Use/OneDrive - AZTI/General - IOTC_YFT_2024/models/stock_status'
year_mod = 'WPTT26'
avgYears = '20' # '0', '12' or '20'

# Define labels for each scalar scenario:
if(avgYears == '0') scalarLab = ''
if(avgYears == '12') scalarLab = '-scale-'
if(avgYears == '20') scalarLab = '-scale20-'

# Catch multipliers:
cmult = c("c1.2", "c1.15", "c1.1", "c1.05", "c1", "c0.95", "c0.9", "c0.85",
          "c0.8", "c0.75", "c0.7", "c0.65", "c0.6")
# cmult = c("c1.2", "c1.1", "c1", "c0.9", "c0.8", "c0.7", "c0.6")
# Scalar:
scalar_ffmsy = 1 # should change?

# Create object to save:
PROJ_INPUT_OBJECT=list();
PROJ_INPUT_OBJECT[[year_mod]]=list()
PROJ_INPUT_OBJECT[[year_mod]][['gridIO']] = list()
PROJ_INPUT_OBJECT[[year_mod]][['gridIO']][['proj']] = list()
PROJ_INPUT_OBJECT[[year_mod]][['gridIO']][['MVN']] = list()

##########################################
### WPTT26
##########################################

GridName = 'gridIO'
GridList = list.files(mod_dir)
Grid = list()
gridDir = mod_dir
# Define ts labels:
pars =c('Bratio','F')
Bfloor = 0.01
Fceiling = 10
Grid2 = NULL
for (i in 1:length(GridList)) {
    Grid[[GridList[i]]] = list()
    # Find scalar:
    if(avgYears == '0') scalar_sbmsy = 1
    if(avgYears == '12') { 
      myfore = SS_readforecast(file.path(mod_dir, GridList[i], 'forecast_scaled_1.ss'), verbose = FALSE)
      scalar_sbmsy = myfore$fcast_rec_val
    }
    if(avgYears == '20') { 
      myfore = SS_readforecast(file.path(mod_dir, GridList[i], 'forecast_scaled20_1.ss'), verbose = FALSE)
      scalar_sbmsy = myfore$fcast_rec_val
    }
    tmp <- SS_output(dir = gridDir %&% GridList[i], 
                     repfile = paste0("Report", scalarLab, "c1.sso"), 
                     covar=FALSE, printstats = FALSE, verbose = FALSE)
    tmp2 <- tmp$derived_quants
    tmp3 <- subset(tmp2, substring(tmp2$Label, 1, 9) == "ForeCatch")
    maxYear <- max(as.numeric(substring(tmp3$Label, 11, 14)))
    if (length(which(substring(tmp2$Label, 1, 9) == "ForeCatch" &  (tmp2$Value < 0 | tmp2$Value>1e6))) >= 1) {
        crashElement <- min(which(substring(tmp2$Label, 1, 9) == "ForeCatch" & (tmp2$Value < 0 | tmp2$Value>1e6)))
        firstCrashYear <- substring(tmp2[crashElement, ]$Label, 11, 14)
        tmp2[substring(tmp2$Label, 5, 8) %in% c((as.numeric(firstCrashYear) - 1):as.numeric(maxYear)), ]$Value <- 0.1
        tmp2[substring(tmp2$Label, 3, 6) %in% c((as.numeric(firstCrashYear) - 1):as.numeric(maxYear)), ]$Value <- 5
    }
    tmp <- list(tmp2, tmp$recruit, tmp$maximum_gradient_component, tmp$likelihoods_used, tmp$cpue, tmp$len_comp_fit_table) # not sure if this is the right length df
    names(tmp) <- c("derived_quants", "recruit", "maximum_gradient_component", "likelihoods_used", "cpue", "Length_comp_Eff_N_tuning_check")
    Grid[[GridList[i]]][['model']] = tmp
    # Start loop to read cmult:
    save_list = list()
    for(pr in seq_along(cmult)) {
      c_temp <- SS_output(dir = gridDir %&% GridList[i], 
                        repfile = paste0("Report", scalarLab, cmult[pr], ".sso"), 
                        covar=FALSE, printstats = FALSE, verbose = FALSE)
  		c_tmp_df <- c_temp$derived_quants
      tmp2 <- c_tmp_df
      tmp3 <- subset(tmp2, substring(tmp2$Label, 1, 9) == "ForeCatch")
      maxYear <- max(as.numeric(substring(tmp3$Label, 11, 14)))
      if (length(which(substring(tmp2$Label, 1, 9) == "ForeCatch" & (tmp2$Value < 0 | tmp2$Value>1e6))) >= 1) {
          crashElement <- min(which(substring(tmp2$Label, 1, 9) == "ForeCatch" & (tmp2$Value < 0 | tmp2$Value>1e6)))
          firstCrashYear <- substring(tmp2[crashElement, ]$Label, 11, 14)
  				tmp2[paste0('SSB_',c((as.numeric(firstCrashYear) - 1):as.numeric(maxYear))),]$Value <- 0.1
  				tmp2[paste0('SPRratio_',c((as.numeric(firstCrashYear) - 1):as.numeric(maxYear))),]$Value <- 0
  				tmp2[paste0('Bratio_',c((as.numeric(firstCrashYear) - 1):as.numeric(maxYear))),]$Value <- 0
  				tmp2[paste0('F_',c((as.numeric(firstCrashYear) - 1):as.numeric(maxYear))),]$Value <- 10
  				tmp2[paste0('ForeCatch_',c((as.numeric(firstCrashYear) - 1):as.numeric(maxYear))),]$Value <- 0
      }
      if (length(which(substring(tmp2$Label, 1, 2) == "F_" & (tmp2$Value < 0 | tmp2$Value >10 ))) >= 1) {
          crashElement <- min(which(substring(tmp2$Label, 1, 2) == "F_" & (tmp2$Value < 0 | tmp2$Value>10)))
          firstCrashYear <- substring(tmp2[crashElement, ]$Label, 3, 6)
  				tmp2[paste0('SSB_',c((as.numeric(firstCrashYear) - 1):as.numeric(maxYear))),]$Value <- 0.1
  				tmp2[paste0('SPRratio_',c((as.numeric(firstCrashYear) - 1):as.numeric(maxYear))),]$Value <- 0
  				tmp2[paste0('Bratio_',c((as.numeric(firstCrashYear) - 1):as.numeric(maxYear))),]$Value <- 0
  				tmp2[paste0('F_',c((as.numeric(firstCrashYear) - 1):as.numeric(maxYear))),]$Value <- 10
  				tmp2[paste0('ForeCatch_',c((as.numeric(firstCrashYear) - 1):as.numeric(maxYear))),]$Value <- 0
      }
      save_list[[pr]] = tmp2
      
      # ----------------------------------
      # Second section
      y <- c_tmp_df
      yprj <- subset(y, substring(y$Label, 1, 9) == "ForeCatch")
      fyrs= (substr(y[y$Label %in% paste0(paste0(pars[1],"_"),1:100000),1],8,20))
      yprj <- subset(y, substring(y$Label, 1, 9) == "ForeCatch")
      prjyr = as.numeric(substring(yprj$Label, 11,100))
      maxYear <- max(as.numeric(fyrs))
      minYear <- min(as.numeric(fyrs))
      yrs = minYear:maxYear
      if (length(which(substring(y$Label, 1, 9) == "ForeCatch" &  (y$Value < 0 | y$Value>1e6))) >= 1) {
        crashElement <- min(which(substring(y$Label, 1, 9) == "ForeCatch" & (y$Value < 0 | y$Value>1e6)))
        firstCrashYear <- substring(y[crashElement, ]$Label, 11, 14)
        yrs_crashed = (as.numeric(firstCrashYear) - 1):as.numeric(maxYear)
        y[y$Label %in% paste0(paste0(pars[1],"_"),yrs_crashed),2]= Bfloor
        y[y$Label %in% paste0(paste0(pars[2],"_"),yrs_crashed),2]= Fceiling
      }
      if (length(which(substring(y$Label, 1, 2) == "F_" &  (y$Value < 0 | y$Value>10))) >= 1) {
        crashElement <- min(which(substring(y$Label, 1,2) == "F_" & (y$Value < 0 | y$Value>10)))
        firstCrashYear <- substring(y[crashElement, ]$Label, 3, 6)
        yrs_crashed = (as.numeric(firstCrashYear) - 1):as.numeric(maxYear)
        y[y$Label %in% paste0(paste0(pars[1],"_"),yrs_crashed),2]= Bfloor
        y[y$Label %in% paste0(paste0(pars[2],"_"),yrs_crashed),2]= Fceiling
      }    
      Grid2 = rbind(Grid2,data.frame(model=GridList[i],yr=yrs,
                                   stock=(y[y$Label %in% paste0(paste0(pars[1],"_"),fyrs),2])/scalar_sbmsy,
                                   harvest=(y[y$Label %in% paste0(paste0(pars[2],"_"),fyrs),2])*scalar_ffmsy, # confirm this
                                   tac=yprj$Value[1],
                                   tac.perc=gsub(pattern = 'c', replacement = '', cmult[pr]),
                                   type=ifelse(yrs %in% prjyr,"prj","fit"))) 
    } # cmult loop
    names(save_list) <- cmult
    Grid[[GridList[i]]][['project']] = save_list
    cat("model", GridList[i], "done", "\n")
}
PROJ_INPUT_OBJECT[[year_mod]][[GridName]][['proj']] = Grid
PROJ_INPUT_OBJECT[[year_mod]][[GridName]][['MVN']] = Grid2

# Save output object
save(PROJ_INPUT_OBJECT, file = file.path(save_dir, paste0('GRID_PROJ_INPUT_OBJECT_', avgYears,'.RData')))
