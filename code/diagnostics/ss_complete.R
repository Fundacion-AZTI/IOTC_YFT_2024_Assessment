
# Script to fit a set of SS models to follow the SSdiags recomendations
# Depending on which iters are launched in the call of the cluster
# a subset can be fit.
#
# YFT IOTC
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())


cluster <- TRUE

if (cluster) {
  
  usnam   <- R.utils::getUsername.System()
  lib.dir <- file.path("/home",usnam,"rlibs")
  
  # load libraries
  
  .libPaths( c( .libPaths(), lib.dir) ) # Add new library path
  .libPaths(.libPaths()[2])             # Select the default
  
  library(r4ss, lib.loc = lib.dir)
  
  
  # Directories
  
  maindir <- file.path(getwd(), Sys.getenv("SLURM_JOB_NAME")) #'bc'
  tmpdir  <- Sys.getenv("TMP_DIR") # /tmp/jobs/<usnam>/<jobtaskid>
  
  
  
  iter <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  
  prefix <- "./"
  exefile_to_run <- "ss"
  print( file.path(getwd(), exefile_to_run))
  print(tmpdir)
  file.copy(file.path(getwd(), exefile_to_run), file.path(maindir, exefile_to_run))
  system("chmod +x ss")
  
} else {
  
  library(r4ss)
  #set the path whwere the folder saly is in the script
  folder <- "saly" 
  maindir <- tmpdir <- file.path(getwd(), folder)
  
  iter <- 1  # values: 1 = nohess, 2 = hess, 3:8 = retros, 9:(8+nr0) = R0 profile, > (8+nr0) = jitteing.
  
  prefix <- "./"
  exefile_to_run <- "ss.exe"
  
}

sessionInfo()
setwd(maindir)



system("chmod +x ss")
data.file <- dir(pattern = 'data.ss')
control.file <- dir(pattern = 'control.ss')

##out_bc <- SS_output(getwd())
#LNR0   <- out_bc$parameters["SR_LN(R0)",'Value'] 
LNR0 <- 11.61
R0.vec <- LNR0*seq(0.92,1.08,0.02)


h.vec <- c(seq(0.4,0.64,0.025),0.645654,seq(0.7,0.95,0.025))

sigmaR.vec <- seq(0.1,0.7,0.05)
k.vec <- c(0.17,0.19,0.1959340,seq(0.21,0.27,0.02))
Linf.vec <- c(seq(140, 164,2),167,c(seq(168, 175,2)))
NatM.vec <- c(seq(0.2, 0.4,0.1),0.462,c(seq(0.5, 0.8,0.1)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  1.    No hessian                         ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(iter == 1){

  dir.create(file.path(tmpdir, 'nohess'))
  
  file.copy(file.path(maindir, exefile_to_run), file.path(tmpdir, 'nohess', exefile_to_run))
  file.copy(file.path(maindir, 'starter.ss'),   file.path(tmpdir, 'nohess', 'starter.ss'))
  file.copy(file.path(maindir, 'forecast.ss'),  file.path(tmpdir, 'nohess', 'forecast.ss'))
  file.copy(file.path(maindir, data.file),      file.path(tmpdir, 'nohess', data.file))
  file.copy(file.path(maindir, control.file),   file.path(tmpdir, 'nohess', control.file))
  
  setwd(file.path(tmpdir, 'nohess'))
  
  extras <- "-nox -nohess"

  command <- paste0(prefix, exefile_to_run, " ", extras)

  message("Running model in ", getwd(), "\n", "using the command:\n   ", 
        command, sep = "")
  ADMBoutput <- system(command, intern = FALSE)
  
  file.copy( file.path(tmpdir, 'nohess'), maindir, recursive = TRUE)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  2.     Hessian                           ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(iter == 2){
  
  dir.create(file.path(tmpdir, 'hess'))
  
  file.copy(file.path(maindir, exefile_to_run), file.path(tmpdir, 'hess', exefile_to_run))
  file.copy(file.path(maindir, 'starter.ss'),   file.path(tmpdir, 'hess', 'starter.ss'))
  file.copy(file.path(maindir, 'forecast.ss'),  file.path(tmpdir, 'hess', 'forecast.ss'))
  file.copy(file.path(maindir, data.file),      file.path(tmpdir, 'hess', data.file))
  file.copy(file.path(maindir, control.file),   file.path(tmpdir, 'hess', control.file))
  
  setwd(file.path(tmpdir, 'hess'))
  
  extras <- "-nox"
  
  command <- paste0(prefix, exefile_to_run, " ", extras)
  
  message("Running model in ", getwd(), "\n", "using the command:\n   ", 
          command, sep = "")
  ADMBoutput <- system(command, intern = FALSE)
  
  file.copy( file.path(tmpdir, 'hess'), maindir, recursive = TRUE)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  3.    Retrospective.                     ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(iter %in% 3:8) {
  retro.vec <- c(0,-4,-8,-12,-16,-20)
  file.copy(file.path(maindir, exefile_to_run), file.path(tmpdir, exefile_to_run))
  file.copy(file.path(maindir, 'starter.ss'),   file.path(tmpdir, 'starter.ss'))
  file.copy(file.path(maindir, 'forecast.ss'),  file.path(tmpdir, 'forecast.ss'))
  file.copy(file.path(maindir, data.file),      file.path(tmpdir, data.file))
  file.copy(file.path(maindir, control.file),   file.path(tmpdir, control.file))
  
  retro(masterdir=tmpdir, oldsubdir="", newsubdir="retros", 
  years= retro.vec[-iter + 4], extras = "-nox -nohess")
  
  file.copy( file.path(tmpdir, 'retros'), maindir, recursive = TRUE)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  4.    R0 profile.                       ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

NR0 <- length(R0.vec)

if(iter %in% c(9:(NR0+8))){
  
  root_R0profile <- file.path(tmpdir, 'LN_R0')
  mydir          <- file.path(root_R0profile, paste0('prof-', iter-8))
  
  # extras <- '-nox -nohess' #"-nox -nohess -ams 20000 -gbs 10000000 -cbs 5000000"
  
  dir.create(root_R0profile)
  dir.create(mydir)
  
  file.copy(file.path(maindir, 'starter.ss'),  file.path(mydir, 'starter.ss'))
  file.copy(file.path(maindir, 'forecast.ss'),  file.path(mydir, 'forecast.ss'))
  # file.copy(file.path(maindir, 'ss.par'),       file.path(mydir, 'ss.par'))
  file.copy(file.path(maindir, data.file),      file.path(mydir, data.file))
  file.copy(file.path(maindir, control.file),   file.path(mydir, control.file))
  file.copy(file.path(maindir, control.file),   file.path(mydir, 'control_modified.ss'), overwrite = TRUE)
  file.copy(file.path(maindir, exefile_to_run), file.path(mydir, exefile_to_run))
  
  starter <- SS_readstarter(file.path(mydir, 'starter.ss'))
  
  # change control file name in the starter file
  starter$ctlfile <- "control_modified.ss"
  
  # make sure the prior likelihood is calculated
  # for non-estimated quantities
  starter$prior_like <- 1
  
  # write modified starter file
  SS_writestarter(starter, dir=mydir, overwrite=TRUE)
  

  profile <- profile(dir = mydir, 
                        exe = exefile_to_run,
                        oldctlfile = control.file,
                        newctlfile    = "control_modified.ss",
                        extras        = '-nox -nohess',
                        string        = "SR_LN(R0)",
                        whichruns     = iter-8, # In each task of the cluster only the run h.vec[iter] is done.
                        profilevec    = R0.vec, verbose = FALSE)

  save(profile, file = file.path(root_R0profile, paste('LN_R0_profile', iter-8, '.RData', sep = "")))
  
  file.copy( root_R0profile, maindir, recursive = TRUE)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  4.    h profile.    20 start                   ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

iterS <- 20
iterE <- iterS+length(h.vec)

Nh <- length(h.vec)

if(iter %in% c(iterS:iterE)){
  
  root_R0profile <- file.path(tmpdir, 'h_steep')
  mydir          <- file.path(root_R0profile, paste0('prof-', iter-(iterS-1)))
  
  # extras <- '-nox -nohess' #"-nox -nohess -ams 20000 -gbs 10000000 -cbs 5000000"
  
  dir.create(root_R0profile)
  dir.create(mydir)
  
  file.copy(file.path(maindir, 'starter.ss'),  file.path(mydir, 'starter.ss'))
  file.copy(file.path(maindir, 'forecast.ss'),  file.path(mydir, 'forecast.ss'))
  # file.copy(file.path(maindir, 'ss.par'),       file.path(mydir, 'ss.par'))
  file.copy(file.path(maindir, data.file),      file.path(mydir, data.file))
  file.copy(file.path(maindir, control.file),   file.path(mydir, control.file))
  file.copy(file.path(maindir, control.file),   file.path(mydir, 'control_modified.ss'), overwrite = TRUE)
  file.copy(file.path(maindir, exefile_to_run), file.path(mydir, exefile_to_run))
  
  starter <- SS_readstarter(file.path(mydir, 'starter.ss'))
  
  # change control file name in the starter file
  starter$ctlfile <- "control_modified.ss"
  
  # make sure the prior likelihood is calculated
  # for non-estimated quantities
  starter$prior_like <- 1
  
  # write modified starter file
  SS_writestarter(starter, dir=mydir, overwrite=TRUE)
  
  
  profile <- profile(dir = mydir, 
                        exe = exefile_to_run,
                     oldctlfile = control.file,
                        newctlfile    = "control_modified.ss",
                        extras        = '-nox -nohess',
                        string        = "SR_BH_steep",
                        whichruns     = iter-(iterS-1), # In each task of the cluster only the run h.vec[iter] is done.
                        profilevec    = h.vec, verbose = FALSE)
  
  save(profile, file = file.path(root_R0profile, paste('h_steep_profile', iter-(iterS-1), '.RData', sep = "")))
  
  file.copy( root_R0profile, maindir, recursive = TRUE)
  
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  4.    sigmaR 30                     ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

iterS <- 45
iterE <- iterS+length(sigmaR.vec)



if(iter %in% c(iterS:iterE)){
  
  root_R0profile <- file.path(tmpdir, 'sigmaR')
  mydir          <- file.path(root_R0profile, paste0('prof-', iter-(iterS-1)))
  
  # extras <- '-nox -nohess' #"-nox -nohess -ams 20000 -gbs 10000000 -cbs 5000000"
  
  dir.create(root_R0profile)
  dir.create(mydir)
  
  file.copy(file.path(maindir, 'starter.ss'),  file.path(mydir, 'starter.ss'))
  file.copy(file.path(maindir, 'forecast.ss'),  file.path(mydir, 'forecast.ss'))
  # file.copy(file.path(maindir, 'ss.par'),       file.path(mydir, 'ss.par'))
  file.copy(file.path(maindir, data.file),      file.path(mydir, data.file))
  file.copy(file.path(maindir, control.file),   file.path(mydir, control.file))
  file.copy(file.path(maindir, control.file),   file.path(mydir, 'control_modified.ss'), overwrite = TRUE)
  file.copy(file.path(maindir, exefile_to_run), file.path(mydir, exefile_to_run))
  
  starter <- SS_readstarter(file.path(mydir, 'starter.ss'))
  
  # change control file name in the starter file
  starter$ctlfile <- "control_modified.ss"
  
  # make sure the prior likelihood is calculated
  # for non-estimated quantities
  starter$prior_like <- 1
  
  # write modified starter file
  SS_writestarter(starter, dir=mydir, overwrite=TRUE)
  
  
  profile <- profile(dir = mydir, 
                        exe = exefile_to_run,
                     oldctlfile = control.file,
                        newctlfile    = "control_modified.ss",
                        extras        = '-nox -nohess',
                        string        = "sigmaR",
                        whichruns     = iter-(iterS-1), # In each task of the cluster only the run h.vec[iter] is done.
                        profilevec    = sigmaR.vec, verbose = FALSE)
  
  save(profile, file = file.path(root_R0profile, paste('sigmaR_profile', iter-(iterS-1), '.RData', sep = "")))
  
  file.copy( root_R0profile, maindir, recursive = TRUE)
  
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  4.    JITTER   40                          ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
iterS <- 60

if(iter >= iterS & iter< 80){
  
  #command <- paste0(prefix, exefile_to_run, " ", extras)
 # mydir       <- file.path(root_jitter, paste0("jitter_", iter-iterS-1))
      root_jitter <- file.path(tmpdir,"jitter")
  mydir       <- file.path(root_jitter, paste0("jitter_", iter-iterS-1))

  extras <- '-nox -nohess' #"-nox -nohess -ams 20000 -gbs 10000000 -cbs 5000000"
  
  dir.create(root_jitter)
  dir.create(mydir)

  file.copy(file.path(maindir, 'starter.ss'),  file.path(mydir, 'starter.ss'))
 # file.copy(file.path(maindir, 'ss.par'),      file.path(mydir, 'ss.par'))
  file.copy(file.path(maindir, 'forecast.ss'), file.path(mydir, 'forecast.ss'))
  file.copy(file.path(maindir, data.file), file.path(mydir, data.file))
  file.copy(file.path(maindir, control.file), file.path(mydir, control.file))
  file.copy(file.path(maindir, exefile_to_run), file.path(mydir, exefile_to_run))
   # Edit the starter.ss file to use ss.par values for initial parameters
  # (i.e. use parameters from main run as starting values for accelerating convergence)

  ## If Njitter is a vector the fucntion run the iterations specified => we use iter to set different numbers.
  ## we run 2 jitters in each task.
  mrun <- ifelse('hess' %in% dir(maindir), 'hess', 'nohess')
  file.copy(file.path(maindir, mrun, 'ss3.par'), file.path(mydir, 'ss3.par'))
  starter_file <- SS_readstarter(file = file.path(mydir, 'starter.ss'))
  #starter_new <- starter_file
  #starter_new$init_values_src <- 1 # use ss.par after reading setup in the control file
  #SS_writestarter(starter_new, dir = mydir, overwrite = T)  
  
  
  starter_new <- readLines(starter_file)
  starter_new[[8]]  <- "1 #_init_values_src" # use ss.par after reading setup in the control file
 # writeLines(starter_new, con = starter_file)
  SS_writestarter(starter_new, dir = mydir, overwrite = T)    
  
 ## If Njitter is a vector the function run the iterations specified => we use iter to set different numbers.
  ## we run 2 jitters in each task.

  k <- iter - (iterS-1)
  #jit.likes <- SS_RunJitter(mydir=mydir, Njitter = (2*k-1):(2*k), extras = extras,
  #                          jitter_fraction = 0.1, printlikes = FALSE)
jit.likes <- jitter(dir=mydir, Njitter = (2*k-1):(2*k), extras = extras, 
                      jitter_fraction = 0.1, printlikes = FALSE,init_values_src)
  profilemodels <- SSgetoutput(dirvec=mydir, keyvec=(2*k-1):(2*k), getcovar=FALSE)
  # summarize output
  profilesummary <- SSsummarize(profilemodels)
  
  # save(profilemodels, profilesummary, file = paste('output/jitter_',k, '.RData'))
  save(profilemodels, profilesummary, file = file.path(root_jitter, paste('jitter_',k, '.RData')))
  
  file.copy( root_jitter, maindir, recursive = TRUE)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  4.    K  60                          ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
iterS <- 80
iterE <- iterS+length(k.vec)

Nh <- length(k.vec)

if(iter %in% c(iterS:iterE)){
  
  root_R0profile <- file.path(tmpdir, 'k')
  mydir          <- file.path(root_R0profile, paste0('prof-', iter-(iterS-1)))
  
  # extras <- '-nox -nohess' #"-nox -nohess -ams 20000 -gbs 10000000 -cbs 5000000"
  
  dir.create(root_R0profile)
  dir.create(mydir)
  
  file.copy(file.path(maindir, 'starter.ss'),  file.path(mydir, 'starter.ss'))
  file.copy(file.path(maindir, 'forecast.ss'),  file.path(mydir, 'forecast.ss'))
  # file.copy(file.path(maindir, 'ss.par'),       file.path(mydir, 'ss.par'))
  file.copy(file.path(maindir, data.file),      file.path(mydir, data.file))
  file.copy(file.path(maindir, control.file),   file.path(mydir, control.file))
  file.copy(file.path(maindir, control.file),   file.path(mydir, 'control_modified.ss'), overwrite = TRUE)
  file.copy(file.path(maindir, exefile_to_run), file.path(mydir, exefile_to_run))
  
  starter <- SS_readstarter(file.path(mydir, 'starter.ss'))
  
  # change control file name in the starter file
  starter$ctlfile <- "control_modified.ss"
  
  # make sure the prior likelihood is calculated
  # for non-estimated quantities
  starter$prior_like <- 1
  
  # write modified starter file
  SS_writestarter(starter, dir=mydir, overwrite=TRUE)
  
  
  profile <- SS_profile(dir = mydir, 
                        exe = exefile_to_run,
                        masterctlfile = control.file,
                        newctlfile    = "control_modified.ss",
                        extras        = '-nox -nohess',
                        string        = "VonBert_K_Fem_GP_1",
                        whichruns     = iter-(iterS-1), # In each task of the cluster only the run k.vec[iter] is done.
                        profilevec    = k.vec, verbose = FALSE)
  
  save(profile, file = file.path(root_R0profile, paste('k_profile', iter-(iterS-1), '.RData', sep = "")))
  
  file.copy( root_R0profile, maindir, recursive = TRUE)
  
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  4.    Linf  70                          ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
iterS <- 90
iterE <- iterS+length(Linf.vec)

Nh <- length(Linf.vec)

if(iter %in% c(iterS:iterE)){
  
  root_R0profile <- file.path(tmpdir, 'Linf')
  mydir          <- file.path(root_R0profile, paste0('prof-', iter-(iterS-1)))
  
  # extras <- '-nox -nohess' #"-nox -nohess -ams 20000 -gbs 10000000 -cbs 5000000"
  
  dir.create(root_R0profile)
  dir.create(mydir)
  
  file.copy(file.path(maindir, 'starter.ss'),  file.path(mydir, 'starter.ss'))
  file.copy(file.path(maindir, 'forecast.ss'),  file.path(mydir, 'forecast.ss'))
  # file.copy(file.path(maindir, 'ss.par'),       file.path(mydir, 'ss.par'))
  file.copy(file.path(maindir, data.file),      file.path(mydir, data.file))
  file.copy(file.path(maindir, control.file),   file.path(mydir, control.file))
  file.copy(file.path(maindir, control.file),   file.path(mydir, 'control_modified.ss'), overwrite = TRUE)
  file.copy(file.path(maindir, exefile_to_run), file.path(mydir, exefile_to_run))
  
  starter <- SS_readstarter(file.path(mydir, 'starter.ss'))
  
  # change control file name in the starter file
  starter$ctlfile <- "control_modified.ss"
  
  # make sure the prior likelihood is calculated
  # for non-estimated quantities
  starter$prior_like <- 1
  
  # write modified starter file
  SS_writestarter(starter, dir=mydir, overwrite=TRUE)
  
  
  profile <- profile(dir = mydir, 
                        exe = exefile_to_run,
                     oldctlfile = control.file,
                        newctlfile    = "control_modified.ss",
                        extras        = '-nox -nohess',
                        string        = "L_at_Amax_Fem_GP_1",
                        whichruns     = iter-(iterS-1), # In each task of the cluster only the run Linf.vec[iter] is done.
                        profilevec    = Linf.vec, verbose = FALSE)
  
  save(profile, file = file.path(root_R0profile, paste('Linf_profile', iter-(iterS-1), '.RData', sep = "")))
  
  file.copy( root_R0profile, maindir, recursive = TRUE)
  
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  4.    natm profile.    20 start                   ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

iterS <- 120
iterE <- iterS+length(NatM.vec)

Nh <- length(NatM.vec)

if(iter %in% c(iterS:iterE)){
  
  root_R0profile <- file.path(tmpdir, 'natM_')
  mydir          <- file.path(root_R0profile, paste0('prof-', iter-(iterS-1)))
  
  # extras <- '-nox -nohess' #"-nox -nohess -ams 20000 -gbs 10000000 -cbs 5000000"
  
  dir.create(root_R0profile)
  dir.create(mydir)
  
  file.copy(file.path(maindir, 'starter.ss'),  file.path(mydir, 'starter.ss'))
  file.copy(file.path(maindir, 'forecast.ss'),  file.path(mydir, 'forecast.ss'))
  # file.copy(file.path(maindir, 'ss.par'),       file.path(mydir, 'ss.par'))
  file.copy(file.path(maindir, data.file),      file.path(mydir, data.file))
  file.copy(file.path(maindir, control.file),   file.path(mydir, control.file))
  file.copy(file.path(maindir, control.file),   file.path(mydir, 'control_modified.ss'), overwrite = TRUE)
  file.copy(file.path(maindir, exefile_to_run), file.path(mydir, exefile_to_run))
  
  starter <- SS_readstarter(file.path(mydir, 'starter.ss'))
  
  # change control file name in the starter file
  starter$ctlfile <- "control_modified.ss"
  
  # make sure the prior likelihood is calculated
  # for non-estimated quantities
  starter$prior_like <- 1
  
  # write modified starter file
  SS_writestarter(starter, dir=mydir, overwrite=TRUE)
  
  
  profile <- profile(dir = mydir, 
                     exe = exefile_to_run,
                     oldctlfile = control.file,
                     newctlfile    = "control_modified.ss",
                     extras        = '-nox -nohess',
                     string        = "NatM",
                     whichruns     = iter-(iterS-1), # In each task of the cluster only the run h.vec[iter] is done.
                     profilevec    = NatM.vec, verbose = FALSE)
  
  save(profile, file = file.path(root_R0profile, paste('NatM_profile', iter-(iterS-1), '.RData', sep = "")))
  
  file.copy( root_R0profile, maindir, recursive = TRUE)
  
}

