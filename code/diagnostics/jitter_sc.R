
library(r4ss)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

proj_dir = here::here()
setwd(proj_dir)
source(here('code', 'auxiliary_functions.R'))
# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)

dir_plot <- "output/figures/"
dir_table <- "output/tables/"

scs <- "jitter_1_Not_Split_LL"  
scs_wd <-paste0("models/retrospectives_ref_models/models/",scs)


ncores <- parallelly::availableCores(omit = 1)
future::plan(future::multisession, workers = ncores)
numjitter <- 4
jit.likes <- jitter(
  dir = scs_wd, Njitter = numjitter,
  jitter_fraction = 0.1, exe="ss3_3022.exe")

future::plan(future::sequential)

#### Read in results using other r4ss functions
# (note that un-jittered model can be read using keyvec=0:numjitter)
profilemodels <- SSgetoutput(dirvec = scs_wd, keyvec = 1:numjitter, getcovar = FALSE)
# summarize output
profilesummary <- SSsummarize(profilemodels)
# Likelihoods
profilesummary[["likelihoods"]][1, ]
# Parameters
profilesummary[["pars"]]

SSplotComparisons(profilesummary)
