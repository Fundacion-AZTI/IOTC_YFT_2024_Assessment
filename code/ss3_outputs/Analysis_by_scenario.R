

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

dir_plot <- "output/figures/LLdividedModel"
dir_table <- "output/tables/"

spat_config = '4A_io'
fish_names = get_fisheries(spat_config)$fleet_name
fish_names = gsub(pattern = ' ', replacement = '_', x = fish_names)
fleetnames <- paste0(1:25,"_",c(fish_names,c(fish_names[c(7,10,11,13)])))


scs <- c("00_BC","01_update_catch","02_update_cpue","03_update_length","04_update_warnings","05_update_M","06_update_Growth",
         "06b_update_GrowthTaggingData","07_update_Maturity",
         "08_selectivity_PS","09_boundaries","10_recDevs","11_RQ","12_cwp5x5","13_cwp5x5_RQ","14_cwp5x5_RQ_LL11_p2_free",
         "15_BiasCorrectionRamp_hess","16_LLsplit_LL1b_LL4_DN","19_EffortCreep","21_dwtag01","18_CPUEvariable")


desc <- c("BaseCase", "update catch","update cpue","update length","update warnings",
          "Natural mortality age 4.07 years M=0.467",
          "Farley 2023 growth","update_Growth Tagging Data",
          "Maturity Zudaire et al. 2022","update PS selectivity","update boundaries",
          "update recruitment deviates","Adding report quality","Regular grid cwp5x5","Regular grid and cwp5x5 and report quality",
          "free parameter-2 LL 3","Apply bias correction ramp","LL split LL1B and LL4 DN selectivity",
          "Effort creep 0.5% per day","Downweight tagging data by 0.1","CPUE cv variable")

scs <- scs[17]
desc <- desc[17]
scs_wd <-paste0("models/update/",scs)

plot_ss3 <-TRUE
add_perf_table <- TRUE
i<-1


  sc_ss3 <- SS_output(dir=scs_wd[i],  repfile = "Report.sso",covar=T)

  
  #................................................................
  #### PLOT RECRUITMENT DEVIATES  ###3
  
  sub_scs <- scs[1]
  sub_scs_wd <-paste0("models/update/",sub_scs)
 # mod_sum <- aggregate.ssMod(sub_scs, sub_scs_wd)
  
  ### Comparison f
  df <- sc_ss3$recruit
  df$RecDev <- "MainRecDev"
  df$RecDev[df$Yr>300] <- "LateRecDev"

  sub_df_long  <- df %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))%>% subset(Yr<309)

  ggplot(sub_df_long,aes(x=yrqtr,y=dev,group =RecDev,colour = RecDev))+geom_point( size = 2)+
    geom_line( )+xlim(1972,2025)+
    geom_hline(yintercept=0,lty=2)+
    scale_color_discrete(labels=levels(sub_df_long$RecDev))+
    xlab("Year")+ ylab("Log(RecDev)")+theme_fun()
  