#
#       READ ss3 outputs of different scenarios
#               Create a summary performance table
#       compare ss3 outputs of different scenarios
#                 - WINDOWS OLD VS NEW VS
#                 - NEW VS WINDOWS VS LINUX
#       update
#       #       -catch
#       #       -cpue
#       #       -length
#       #       -M
#       #       -growth
#       #       -maturity
#       #       -Rec?

#............................................................



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


#...................................................

##### CONVERGENCY ANALYSIS BETWEEN WINDOWS AND LINUX ####

#...................................................

  
  #...................................................
  #### READ, PLOT AND PERFORMANCE TABLE ####
  #...................................................
  
  
  scs <- c("4A_io_2021_v33017","4A_io_win_v33022","4A_io_lin_v33022",
           "4A_io_win_v33022_FixedParam2_Fl11",
           "4A_io_lin_v33022_FixedParam2_Fl11","4A_io_win_v33022_FixedParam2_Fl11_ss3par")  
  desc <- c("BaseCase_2021", "BC_win_V3.30.22.1","BC_linux_v3.30.22.1","win_FixedParam2_Fl11",
            "lin_FixedParam2_Fl11","win_FixedParam2_Fl11_ss3par")  
  scs_wd <-paste0("models/base_win_vs_lin/",scs)
  
  plot_ss3 <- FALSE
  add_perf_table <- TRUE
  
  for(i in 1:length(scs)){
    sc_ss3 <- SS_output(dir=scs_wd[i],  repfile = "Report.sso",covar=F)
    if (plot_ss3) { SS_plots(sc_ss3,uncertainty=T,png=T,forecastplot=F, fitrange = TRUE, 
                             parrows=5, parcols=4, showdev= TRUE) }
    if(add_perf_table){
    summary_file <- paste0(dir_table,"summaryPerformance.csv")
    df <-mod.performance(scs[i],desc[i],sc_ss3,summary_file)
    write.csv(df, file=summary_file,row.names=FALSE)}
    }
    
  #### end ####
  
  #...................................................
  #
  #### COMPARISON F AND SSB ####
  #
  #...................................................
  
  ### Comparison f
  
  sub_scs <- c("4A_io_2021_v33017","4A_io_win_v33022","4A_io_lin_v33022")  
  
  mod_sum <- aggregate.ssMod(scs, scs_wd)
  
  df <- mod_sum$Fvalue
  df <- df %>% rename_at(1:length(scs),~scs)
  df_long <- df %>% pivot_longer(cols =scs, names_to = "scenario", values_to = "value")
  sub_df_long <- df_long %>% filter(scenario %in% sub_scs)
  sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  sub_df_long  <- sub_df_long %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))
  
  ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario,colour = scenario))+geom_point( size = 1)+
    geom_line( )+xlim(1995,2023)+
    scale_color_discrete(labels=levels(sub_df_long$scenario))+
    xlab("Year")+ ylab("F/Fmsy")+theme_fun()
   
  SavePlot('lin_vs_win_F',15,10)
  
  ### end ###
  
  
  # Comparison SSB
  
  df <- mod_sum$SpawnBio
  df <- df %>% rename_at(1:length(scs),~scs)
  df_long <- df %>% pivot_longer(cols =scs, names_to = "scenario", values_to = "value")
  sub_df_long <- df_long %>% filter(scenario %in% sub_scs)
  sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  sub_df_long  <- sub_df_long %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))
  
  ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario,colour = scenario))+geom_point(   size = 1)+
    geom_line( )+xlim(1995,2023)+ylim(0,2.5e6)+
    scale_color_discrete(labels=levels(sub_df_long$scenario))+xlab("Year")+ ylab("SSB")+
    theme_fun()
  
  SavePlot('lin_vs_win_SSB',15,10)
  
  
  #### end ####
  
  
  #...................................................
  #### sub analysis ####
  #...................................................
  
  sub_scs <- c("4A_io_2021_v33017",
           "4A_io_lin_v33022_FixedParam2_Fl11")  
  
  ### Comparison f
  df <- mod_sum$Fvalue
  df <- df %>% rename_at(1:length(scs),~scs)
  df_long <- df %>% pivot_longer(cols =scs, names_to = "scenario", values_to = "value")
  
  sub_df_long <- df_long %>% filter(scenario %in% sub_scs)
  sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  
  ggplot(sub_df_long,aes(x=Yr,y=value,group =scenario,colour = scenario))+geom_point( size = 1)+
    geom_line( )+xlim(200,298)+
    scale_color_discrete(labels=levels(sub_df_long$scenario))+
    xlab("Year")+ ylab("F/Fmsy")+theme_fun()
  
  
  SavePlot('lin_vs_win_FixedParam2_F',15,10)
  
  ### end ###
  
  
  # Comparison SSB
  
  df <- mod_sum$SpawnBio
  df <- df %>% rename_at(1:length(scs),~scs)
  df_long <- df %>% pivot_longer(cols =scs, names_to = "scenario", values_to = "value")
  sub_df_long <- df_long %>% filter(scenario %in% sub_scs)
  sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  
  ggplot(sub_df_long,aes(x=Yr,y=value,group =scenario,colour = scenario))+geom_point(   size = 1)+
    geom_line( )+xlim(200,298)+ylim(0,2.5e6)+
    scale_color_discrete(labels=levels(sub_df_long$scenario))+xlab("Year")+ ylab("SSB")+
    theme_fun()
  
  SavePlot('lin_vs_win_FixedParam2_SSB',15,10)

  #### end ####


#...................................................
  
##### STEPWISE ####
  
#...................................................
  
  
  
  
  #...................................................
  #### STEPWISE- READ, PLOT AND PERFORMANCE TABLE ####
  #...................................................
  spat_config = '4A_io'
  fish_names = get_fisheries(spat_config)$fleet_name
  fish_names = gsub(pattern = ' ', replacement = '_', x = fish_names)
  fleetnames <- paste0(1:25,"_",c(fish_names,c(fish_names[c(7,10,11,13)])))
  
  scs <- c("00_BC","01_update_catch","02_update_cpue","03_update_length","04_update_warnings","05_update_M","06_update_Growth",
           "06b_update_GrowthTaggingData","07_update_Maturity",
           "08_selectivity_PS","09_boundaries","10_recDevs","11_RQ","12_cwp5x5","13_cwp5x5_RQ","14_cwp5x5_RQ_LL11_p2_free",
           "15_BiasCorrectionRamp_hess","16_LLsplit_LL1b_LL4_DN","19_EffortCreep","21_dwtag01",'22_TwoBlockCPUE','23_BCR1')
  
  
  desc <- c("BaseCase", "update catch","update cpue","update length","update warnings",
            "Natural mortality age 4.07 years M=0.467",
            "Farley 2023 growth","update_Growth Tagging Data",
            "Maturity Zudaire et al. 2022","update PS selectivity","update boundaries",
            "update recruitment deviates","Adding report quality","Regular grid cwp5x5","Regular grid and cwp5x5 and report quality",
            "free parameter-2 LL 3","Apply bias correction ramp","LL split LL1B and LL4 DN selectivity",
            "Effort creep 0.5% per day","Downweight tagging data by 0.1","Two block CPUE","Bias correction ramp of 1")
  
  scs <- scs[1:17]
  desc <- desc[1:17]
  scs_wd <-paste0("models/update/",scs)
  # scs <- paste0("sensitivities_16_CT/",c("16C_NsampLL5_LL_log_FL7_13DN"))
  # scs <- c("19_EffortCreep_CT","18_CPUEvariable_CT","20_H07_CT","20_H09_CT","21_tagDW01_CT")
  # scs <- paste0("sensitivities_17_CT/",c("17_NoRecDev","17_Rec3Region",
  #                                        "17_RecDev_188betweenRegions","17_Rec1Region"))
 # scs <- c("20_H07","20_H09")
  scs <- c("23_BCR1")
  scs_wd <-paste0("models/update/",scs)
  
  plot_ss3 <-FALSE
  add_perf_table <- TRUE
  i<-1
    
  for(i in 1:length(scs)){
    sc_ss3 <- SS_output(dir=scs_wd[i],  repfile = "Report.sso",covar=T)
    if (plot_ss3) { SS_plots(sc_ss3,uncertainty=T,png=T,forecastplot=F, fitrange = TRUE, 
                             parrows=5, parcols=4, showdev= TRUE) }
    if(add_perf_table){
      summary_file <- paste0(dir_table,"summaryPerformance.csv")
      df <-mod.performance(scs[i],desc[i],sc_ss3,summary_file)
      write.csv(df, file=summary_file,row.names=FALSE)}
  }
  
  #### end ####
  
  
  
  #...................................................
  #
  #### STEPWISE - COMPARISON F AND SSB ####
  #
  #...................................................
  
  
  
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
  
  i <- 17:21
  #i <- 17:20 last 4 models
  sub_scs <- scs[i]
  sub_scs_wd <-paste0("models/update/",sub_scs)
  mod_sum <- aggregate.ssMod(sub_scs, sub_scs_wd)
  
  ### Comparison f
  df <- mod_sum$Fvalue
  df <- df %>% rename_at(1:length(sub_scs),~sub_scs)
  df_long <- df %>% pivot_longer(cols =sub_scs, names_to = "scenario", values_to = "value")
  sub_df_long <- df_long %>% subset(scenario %in% sub_scs) 
  sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  sub_df_long  <- sub_df_long %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))
  
  ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario,colour = scenario))+geom_point( size = 1)+
    geom_line( )+xlim(1950,2023)+
    scale_color_discrete(labels=levels(sub_df_long$scenario))+
    xlab("Year")+ ylab("F/Fmsy")+theme_fun()
  
  SavePlot(paste0('Stepwise_update_F_',max(i)),15,10)
  
  ### end ###
  
  
  # Comparison SSB
  
  df <- mod_sum$SpawnBio
  df <- df %>% rename_at(1:length(sub_scs),~sub_scs)
  df_long <- df %>% pivot_longer(cols =sub_scs, names_to = "scenario", values_to = "value")
  sub_df_long <- df_long %>% subset(scenario %in% sub_scs)  %>% dplyr::filter(!(scenario=="00_BC" & Yr>=297))
  sub_df_long <- sub_df_long %>% subset(scenario %in% sub_scs) 
    sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  sub_df_long  <- sub_df_long %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))
  
  ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario,colour = scenario))+geom_point(   size = 1)+
    geom_line( )+xlim(1950,2023)+ylim(0,4.5e6)+
    scale_color_discrete(labels=levels(sub_df_long$scenario))+xlab("Year")+ ylab("SSB")+
    theme_fun()
  
  SavePlot(paste0('Stepwise_update_SSB_',max(i)),15,10)
  
  #COMPARISON SSB/B0
  
  # Comparison SSB/b0
  
  df <- mod_sum$SpawnBio
  B0 <- as.vector(df[df$Label=="SSB_Virgin",-c(length(i)+1,length(i)+2)])
  df[,-c(length(i)+1,length(i)+2)] <- df[,-c(length(i)+1,length(i)+2)]/B0
  df <- df %>% rename_at(1:length(sub_scs),~sub_scs)
  df_long <- df %>% pivot_longer(cols =sub_scs, names_to = "scenario", values_to = "value")
  sub_df_long <- df_long %>% subset(scenario %in% sub_scs)  %>% dplyr::filter(!(scenario=="00_BC" & Yr>=297))
  sub_df_long <- sub_df_long %>% subset(scenario %in% sub_scs) 
  sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  sub_df_long  <- sub_df_long %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))
  
  #SavePlot(paste0('Stepwise_update_SSB',max(i)),15,10)
  
  
  ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario,colour = scenario))+geom_point(   size = 1)+
    geom_line( )+xlim(1950,2023)+ylim(0,1.15)+
    scale_color_discrete(labels=levels(sub_df_long$scenario))+xlab("Year")+ ylab("SSB/B0")+
    theme_fun()
  
  SavePlot(paste0('Stepwise_update_SSB_B0_last4_',max(i)),15,10)
  
  
  #..................................................
  #### COMPARISON GRAY COLORS ####
  spat_config = '4A_io'
  fish_names = get_fisheries(spat_config)$fleet_name
  fish_names = gsub(pattern = ' ', replacement = '_', x = fish_names)
  fleetnames <- paste0(1:25,"_",c(fish_names,c(fish_names[c(7,10,11,13)])))
  
  
  scs <- c("00_BC","01_update_catch","02_update_cpue","03_update_length","04_update_warnings","05_update_M","06_update_Growth",
           "06b_update_GrowthTaggingData","07_update_Maturity",
           "08_selectivity_PS","09_boundaries","10_recDevs","11_RQ","12_cwp5x5","13_cwp5x5_RQ","14_cwp5x5_RQ_LL11_p2_free",
           "15_BiasCorrectionRamp_hess","16_LLsplit_LL1b_LL4_DN","19_EffortCreep","21_dwtag01",'22_TwoBlockCPUE','23_BCR1')
  
  
  desc <- c("BaseCase", "update catch","update cpue","update length","update warnings",
            "Natural mortality age 4.07 years M=0.467",
            "Farley 2023 growth","update_Growth Tagging Data",
            "Maturity Zudaire et al. 2022","update PS selectivity","update boundaries",
            "update recruitment deviates","Adding report quality","Regular grid cwp5x5","Regular grid and cwp5x5 and report quality",
            "free parameter-2 LL 3","Apply bias correction ramp","LL split LL1B and LL4 DN selectivity",
            "Effort creep 0.5% per day","Downweight tagging data by 0.1","Two block CPUE","Bias correction ramp of 1")
  
  
  
  i <- c(2,6)
  nmplot<- "BCR"
  RefModels <- TRUE
  if(RefModels == TRUE){
    scs <- c( "1_OneBlock_LLsel","2_TwoBlock_LLsel","3_EffortCreep","4_Dwtag01",'5_TwoBlockCPUE','6_BCR1')
    sub_scs <- scs[i]
    sub_scs_wd <-paste0("models/RefModels/",sub_scs)
    mod_sum <- aggregate.ssMod(sub_scs, sub_scs_wd)
  }else {
    sub_scs <- scs[i]
    sub_scs_wd <-paste0("models/update/",sub_scs)
    mod_sum <- aggregate.ssMod(sub_scs, sub_scs_wd)
}
   
  ### Comparison f
  df <- mod_sum$Fvalue
  df <- df %>% rename_at(1:length(sub_scs),~sub_scs)
  df_long <- df %>% pivot_longer(cols =sub_scs, names_to = "scenario", values_to = "value")
  sub_df_long <- df_long %>% subset(scenario %in% sub_scs)  %>% dplyr::filter(!(scenario=="00_BC" & Yr>=297))
  sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  sub_df_long  <- sub_df_long %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))
  
  scGrey<- levels(sub_df_long$scenario)[-length(levels(sub_df_long$scenario))]
  scRed <- levels(sub_df_long$scenario)[length(levels(sub_df_long$scenario))]
  scBlack <- levels(sub_df_long$scenario)[length(levels(sub_df_long$scenario))-1]  
  if(length(i)>2){
  ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario)) +
    geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scGrey),size=1) +
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scBlack),size=1.5) +
    geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scRed),size=1) +
    scale_color_manual(values = c(gray.colors(length(i)-2,start=0.7,end=0.5),'black', 'red')) +xlim(1950,2023)+ylim(0,2.5)+
    xlab("Year")+ ylab("F/Fmsy")+
    theme_fun()
  }else{
    ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario)) +
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scGrey),size=1) +
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scRed),size=1.25) +
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scBlack),size=1) +
      scale_color_manual(values = c(gray.colors(0,start=0.7,end=0), 'black', 'red')) +xlim(1950,2023)+ylim(0,2.5)+
      xlab("Year")+ ylab("F/Fmsy")+
      theme_fun()
    
  }
  SavePlot(paste0('Stepwise_update_F_GRAY_',max(i),'_',nmplot),15,10)
  #SavePlot(paste0('Stepwise_update_F_',max(i)),15,10)
  
  ### end ###
  
  
  # Comparison SSB
  
  df <- mod_sum$SpawnBio
  df <- df %>% rename_at(1:length(sub_scs),~sub_scs)
  df_long <- df %>% pivot_longer(cols =sub_scs, names_to = "scenario", values_to = "value")
  sub_df_long <- df_long %>% subset(scenario %in% sub_scs)  %>% dplyr::filter(!(scenario=="00_BC" & Yr>=297))
  sub_df_long <- sub_df_long %>% subset(scenario %in% sub_scs) 
  sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  sub_df_long  <- sub_df_long %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))
  
  #SavePlot(paste0('Stepwise_update_SSB',max(i)),15,10)
  scGrey<- levels(sub_df_long$scenario)[-length(levels(sub_df_long$scenario))]
  scRed <- levels(sub_df_long$scenario)[length(levels(sub_df_long$scenario))]
  scBlack <- levels(sub_df_long$scenario)[length(levels(sub_df_long$scenario))-1]  
  if(length(i)>2){ 
  ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario)) +
    geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scGrey),size=1) +
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scBlack),size=1.5) +
    geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scRed),size=1) +
    scale_color_manual(values = c(gray.colors(length(i)-2,start=0.7,end=0.5),'black', 'red')) +xlim(1950,2023)+ylim(0,4.5e6)+
    xlab("Year")+ ylab("SSB")+
    theme_fun()
  }else{
    ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario)) +
 #     geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scGrey),size=1) +
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scBlack),size=1.25)+ 
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scRed),size=1) +
    scale_color_manual(values = c(gray.colors(0,start=0.7,end=0.2),'black', 'red')) +xlim(1950,2023)+ylim(0,4.5e6)+
      xlab("Year")+ ylab("SSB")+
      theme_fun()
    }
  
  SavePlot(paste0('Stepwise_update_SSB_GRAY_',max(i),'_',nmplot),15,10)
  
  
  # Comparison SSB/b0
  
  df <- mod_sum$SpawnBio
  B0 <- as.vector(df[df$Label=="SSB_Virgin",-c(length(i)+1,length(i)+2)])
  df[,-c(length(i)+1,length(i)+2)] <- df[,-c(length(i)+1,length(i)+2)]/B0
  df <- df %>% rename_at(1:length(sub_scs),~sub_scs)
  df_long <- df %>% pivot_longer(cols =sub_scs, names_to = "scenario", values_to = "value")
  sub_df_long <- df_long %>% subset(scenario %in% sub_scs)  %>% dplyr::filter(!(scenario=="00_BC" & Yr>=297))
  sub_df_long <- sub_df_long %>% subset(scenario %in% sub_scs) 
  sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  sub_df_long  <- sub_df_long %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))
  
  #SavePlot(paste0('Stepwise_update_SSB',max(i)),15,10)

  scGrey<- levels(sub_df_long$scenario)[-length(levels(sub_df_long$scenario))]
  scRed <- levels(sub_df_long$scenario)[length(levels(sub_df_long$scenario))]
  scBlack <- levels(sub_df_long$scenario)[length(levels(sub_df_long$scenario))-1]  
  if(length(i)>2){ 
    ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario)) +
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scGrey),size=1) +
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scBlack),size=1.5) +
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scRed),size=1) +
      scale_color_manual(values = c(gray.colors(length(i)-2,start=0.9,end=0.5),'black', 'red')) +xlim(1950,2023)+ylim(0,1.5)+
      xlab("Year")+ ylab("SSB/B0")+
      theme_fun()
  }else{
    ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario)) +
      #     geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scGrey),size=1) +
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scBlack),size=1)+ 
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scRed),size=1.25) +
      scale_color_manual(values = c(gray.colors(0,start=0.7,end=0.1),'black', 'red')) +xlim(1950,2023)+ylim(0,1.5)+
      xlab("Year")+ ylab("SSB/B0")+
      theme_fun()
  }
  SavePlot(paste0('Stepwise_update_SSB_B0_GRAY_',max(i),'_',nmplot),15,10)
  
  
  
  
  #......................................................
  
  #### SSPLOT COMPARISON ####
  
  #.....................................................
  
  
  
  scs <- c("00_BC","01_update_catch","02_update_cpue","03_update_length","04_update_warnings","05_update_M","06_update_Growth",
           "06b_update_GrowthTaggingData","07_update_Maturity",
           "08_selectivity_PS","09_boundaries","10_recDevs","11_RQ","12_cwp5x5","13_cwp5x5_RQ","14_cwp5x5_RQ_LL11_p2_free",
           "15_BiasCorrectionRamp_hess","16_LLsplit_LL1b_LL4_DN","19_EffortCreep","21_dwtag01","20_H07","20_h09",
           "22_TwoBlockCPUE","18_CPUEvariable","23_Last8recdev_RemLC_B300","23_BCR1_dev300","23_recdev300","23_BCR1")
  
  sub_scs <- scs[c(18,25:28)]
  i <- 1
  mod.sum <- NULL
  for (i in 1:length(sub_scs)){
    #  load(paste("Results/",sc.nm[i],".RData",sep=""))
    oldsubdir = paste0("models/update/",sub_scs[i])
    ss3 <- SSgetoutput(dirvec=oldsubdir,  getcovar=TRUE,forecast=FALSE)
    mod.sum <- c(mod.sum,ss3)
    rm(ss3)
  }
  mod.plots <- SSsummarize(mod.sum)
  SSplotComparisons(mod.plots, legendlabels=sub_scs,print=TRUE,plotdir="output/figures/ComparisonBCR/")
  
  
  
  #......................................................
  
  #### SSPLOT COMPARISON REFERENCE MODELS ####
  
  #.....................................................
  
  
  
  scs <- c("00_BC","01_update_catch","02_update_cpue","03_update_length","04_update_warnings","05_update_M","06_update_Growth",
           "06b_update_GrowthTaggingData","07_update_Maturity",
           "08_selectivity_PS","09_boundaries","10_recDevs","11_RQ","12_cwp5x5","13_cwp5x5_RQ","14_cwp5x5_RQ_LL11_p2_free",
           "15_BiasCorrectionRamp_hess","16_LLsplit_LL1b_LL4_DN","19_EffortCreep","21_dwtag01","20_H07","20_h09",
           "22_TwoBlockCPUE","18_CPUEvariable","23_Last8recdev_RemLC_B300","23_BCR1_dev300","23_recdev300","23_BCR1")
  
  sub_scs <- scs[c(1,18,25:28)]
  i <- 1
  mod.sum <- NULL
  for (i in 1:length(sub_scs)){
    #  load(paste("Results/",sc.nm[i],".RData",sep=""))
    oldsubdir = paste0("models/update/",sub_scs[i])
    ss3 <- SSgetoutput(dirvec=oldsubdir,  getcovar=TRUE,forecast=FALSE)
    mod.sum <- c(mod.sum,ss3)
    rm(ss3)
  }
  mod.plots <- SSsummarize(mod.sum)
  SSplotComparisons(mod.plots, legendlabels=sub_scs,print=TRUE,plotdir="output/figures/ComparisonBCR/")
  
  #...................................................
  ### STEPWISE sub analysis
  #...................................................
  

  
  scs <- c("03_update_cpue_updateLC_LL","03_update_cpue_updateLC_LL_LF","03_update_cpue_updateLC_LL_LF_PSfsc")  
  desc <- c("03_update_cpue_updateLC_LL","03_update_cpue_updateLC_LL_LF","03_update_cpue_updateLC_LL_LF_PSfsc")  
  scs_wd <-paste0("models/update/",scs)
  
  plot_ss3 <- TRUE
  add_perf_table <- TRUE
  
  for(i in 3:length(scs)){
    sc_ss3 <- SS_output(dir=scs_wd[i],  repfile = "Report.sso",covar=F)
    if (plot_ss3) { SS_plots(sc_ss3,uncertainty=T,png=T,forecastplot=F, fitrange = TRUE, 
                             parrows=5, parcols=4, showdev= TRUE) }
    if(add_perf_table){
      summary_file <- paste0(dir_table,"summaryPerformance.csv")
      df <-mod.performance(scs[i],desc[i],sc_ss3,summary_file)
      write.csv(df, file=summary_file,row.names=FALSE)}
  }
  
  
  sub_scs <- c("00_BC","01_update_catch","02_update_cpue",
               "03_update_cpue_updateLC_LL","03_update_cpue_updateLC_LL_LF",
               "03_update_cpue_updateLC_LL_LF_PSfsc","03_update_cpue_updateLC_LL_LF_PSfsc_HD",
               "03_update_cpue_updateLC_LL_LF_PSfsc_HD_PSls","03_update_cpue_updateLC_LL_LF_PSfsc_HD_PSls_GI",
               "03_update_cpue_updateLC_LL_LF_PSfsc_HD_PSls_GI_BB","03_update_cpue_updateLC_LL_LF_PSfsc_HD_PSls_GI_BB_TR",
               "03_update_cpue_updateLC_LL_LF_PSfsc_HD_PSls_GI_BB_TR_OT",
               "03_update_only_length_until_296",
               "03_update_length_boundary","03_update_length")#,"03_update_cpue_updateLC_FL3_7_10","03_update_cpue_updateLC_FL3_7_10_11_13")#","03_update_length_FixParam12_Fl1_4_14_15_LL3_11_l0")  )#,"03_update_length_FixParam12_Fl14_Fl9_l0")  
  sub_scs_wd <-paste0("models/update/",sub_scs)
  mod_sum <- aggregate.ssMod(sub_scs, sub_scs_wd)
  ### Comparison f
  df <- mod_sum$Fvalue
  df <- df %>% rename_at(1:length(sub_scs),~sub_scs)
  df_long <- df %>% pivot_longer(cols =sub_scs, names_to = "scenario", values_to = "value")
  
  sub_df_long <- df_long %>% subset(scenario %in% sub_scs)  %>% dplyr::filter(!(scenario=="00_BC" & Yr>=297))
  sub_df_long <- sub_df_long %>% subset(scenario %in% sub_scs)  %>% dplyr::filter(!(scenario=="03_update_length_boundary" ))
  sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  sub_df_long  <- sub_df_long %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))
  
  ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario,colour = scenario))+geom_point( size = 2)+
    geom_line( )+xlim(1995,2023)+
    scale_color_discrete(labels=levels(sub_df_long$scenario))+
    xlab("Year")+ ylab("F/Fmsy")+theme_fun()
  
  
  SavePlot('Stepwise_and_update_length_byGear_F',15,10)
  
  ### end ###
  
  
  # STEPWISE SUB- ANALYSIS Comparison SSB
  
  df <- mod_sum$SpawnBio
  df <- df %>% rename_at(1:length(sub_scs),~sub_scs)
  df_long <- df %>% pivot_longer(cols =sub_scs, names_to = "scenario", values_to = "value")
  
  sub_df_long <- df_long %>% subset(scenario %in% sub_scs)  %>% dplyr::filter(!(scenario=="00_BC" & Yr>=297))
  sub_df_long <- sub_df_long %>% subset(scenario %in% sub_scs)  %>% dplyr::filter(!(scenario=="03_update_only_length_until_296" ))
  sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  sub_df_long  <- sub_df_long %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))
  
  ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario,colour = scenario))+geom_point(   size = 2)+
    geom_line( )+xlim(1995,2023)+ylim(0,2.5e6)+
    scale_color_discrete(labels=levels(sub_df_long$scenario))+xlab("Year")+ ylab("SSB")+
    theme_fun()
  
  SavePlot('Stepwise_and_update_length_byGear_SSB',15,10)
  
  #### end ####
  
  #update CPUE -VAR?
  #biology
  
  
  
  #...................................................
  #### Analysis residuals by season ####
  #...................................................
  scs <- "15_recDev2021"
  scs_wd <-paste0("models/update/",scs)
  
  plot_ss3 <- TRUE
  add_perf_table <- TRUE
  
    sc_ss3 <- SS_output(dir=scs_wd,  repfile = "Report.sso",covar=F)

   CPUE <- sc_ss3$cpue %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))
   CPUE$seas <- ifelse(CPUE$yrqtr - floor(CPUE$yrqtr) <0.2,1,
                  ifelse(CPUE$yrqtr - floor(CPUE$yrqtr) <0.4,2,
                         ifelse(CPUE$yrqtr - floor(CPUE$yrqtr) <0.7,3,4)))
   CPUE$Residuals <- CPUE$Obs-CPUE$Exp
   CPUE$Area <- as.factor(CPUE$Area)
  ggplot(CPUE, aes(x=yrqtr,y=Residuals)) +
    geom_line(aes(color = Area),size=1) + labs(x="Year", y="Residuals:OBS-EXP") + 
    theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
          legend.position = 'bottom') +
    scale_y_continuous(breaks = breaks_extended(3)) +
    guides(color = guide_legend(title = "CPUE REGION"))+
    facet_wrap( ~ seas, ncol = 2)+theme_fun()
  SavePlot('Comparison_residuals_by_season',15,10)
  
  
  
  #### EXTRA FIGURES LL SPLITTED
  
  scs <- c("00_BC","01_update_catch","02_update_cpue","03_update_length","04_update_warnings","05_update_M","06_update_Growth",
           "06b_update_GrowthTaggingData","07_update_Maturity",
           "08_selectivity_PS","09_boundaries","10_recDevs","11_RQ","12_cwp5x5","13_cwp5x5_RQ","14_cwp5x5_RQ_LL11_p2_free",
           "15_BiasCorrectionRamp_hess","16_LLsplit_LL1b_LL4_DN","19_EffortCreep","21_dwtag01")
  
  
  desc <- c("BaseCase", "update catch","update cpue","update length","update warnings",
            "Natural mortality age 4.07 years M=0.467",
            "Farley 2023 growth","update_Growth Tagging Data",
            "Maturity Zudaire et al. 2022","update PS selectivity","update boundaries",
            "update recruitment deviates","Adding report quality","Regular grid cwp5x5","Regular grid and cwp5x5 and report quality",
            "free parameter-2 LL 3","Apply bias correction ramp","LL split LL1B and LL4 DN selectivity",
            "Effort creep 0.5% per day","Downweight tagging data by 0.1")
  
  scs <- scs[18]
  scs_wd <-paste0("models/update/",scs)
  
  plot_ss3 <-TRUE
  add_perf_table <- TRUE
  i<-1
  sc_ss3 <- SS_output(dir=scs_wd[i],  repfile = "Report.sso",covar=T)
  
  SSplotSelex(sc_ss3,fleets=c(7,10,11,13,22,23,24),subplots=2,plot=TRUE,print = TRUE,lwd=3,legendloc="bottomright",
              plotdir="output/figures/LLdividedModel")
  SSplotComps(sc_ss3,fleets="all",subplots=21,plot=TRUE,print = TRUE,legendloc="bottomright",
              plotdir="output/figures/LLdividedModel")
  SSplotTags(sc_ss3,subplots=1, tagrows=3, tagcols=5)
  SSplotTags(sc_ss3,subplots=9,plot=TRUE,print = TRUE,
             plotdir="output/figures/LLdividedModel")
  SSplotTags(sc_ss3,subplots=10,plot=TRUE,print = TRUE,
             plotdir="output/figures/LLdividedModel")
  SSplotTags(sc_ss3,subplots=10,taggroups = "TG_1")
  
  i <- c(1:6)
  nmplot<- "RefMOdels"
  
  scs <- c("0_BC_2021", "1_OneBlock_LLsel","2_TwoBlock_LLsel",'3_TwoBlockCPUE',"4_Dwtag01","5_EffortCreep")
  sub_scs <- scs[i]
  sub_scs_wd <-paste0("models/RefModels/",sub_scs)
  mod_sum <- aggregate.ssMod(sub_scs, sub_scs_wd)
  
  
  ### Comparison f
  df <- mod_sum$Fvalue
  df <- df %>% rename_at(1:length(sub_scs),~sub_scs)
  df_long <- df %>% pivot_longer(cols =sub_scs, names_to = "scenario", values_to = "value")
  sub_df_long <- df_long %>% subset(scenario %in% sub_scs)  %>% dplyr::filter(!(scenario=="00_BC" & Yr>=297))
  sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  sub_df_long  <- sub_df_long %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))
  
  scGrey<- levels(sub_df_long$scenario)[-length(levels(sub_df_long$scenario))]
  scRed <- levels(sub_df_long$scenario)[length(levels(sub_df_long$scenario))]
  scBlack <- levels(sub_df_long$scenario)[length(levels(sub_df_long$scenario))-1]  
  scBlue <- 
    scGreen <-
    sc
  if(length(i)>2){
    ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario)) +
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scGrey),size=1) +
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scBlack),size=1.5) +
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scRed),size=1) +
      scale_color_manual(values = c(gray.colors(length(i)-2,start=0.7,end=0.5),'black', 'red')) +xlim(1950,2023)+ylim(0,2.5)+
      xlab("Year")+ ylab("F/Fmsy")+
      theme_fun()
  }else{
    ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario)) +
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scGrey),size=1) +
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scRed),size=1.25) +
      geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scBlack),size=1) +
      scale_color_manual(values = c(gray.colors(0,start=0.7,end=0), 'black', 'red')) +xlim(1950,2023)+ylim(0,2.5)+
      xlab("Year")+ ylab("F/Fmsy")+
      theme_fun()
    
  }
  SavePlot(paste0('Stepwise_update_F_GRAY_',max(i),'_',nmplot),15,10)
  #SavePlot(paste0('Stepwise_update_F_',max(i)),15,10)
  
  ### end ###
  
  
  # Comparison SSB REFERENCE MDOELS
  
  df <- mod_sum$SpawnBio
  df <- df %>% rename_at(1:length(sub_scs),~sub_scs)
  df_long <- df %>% pivot_longer(cols =sub_scs, names_to = "scenario", values_to = "value")
  sub_df_long <- df_long %>% subset(scenario %in% sub_scs)  %>% dplyr::filter(!(scenario=="00_BC" & Yr>=297))
  sub_df_long <- sub_df_long %>% subset(scenario %in% sub_scs) 
  sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  sub_df_long  <- sub_df_long %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))
  
  #SavePlot(paste0('Stepwise_update_SSB',max(i)),15,10)
  scRed <-  "0_BC_2021"#, "1_OneBlock_LLsel","2_TwoBlock_LLsel",'3_TwoBlockCPUE',"4_Dwtag01","5_EffortCreep")
  scBlack <- "1_OneBlock_LLsel"
  scBlue <- "2_TwoBlock_LLsel"
  scGreen <- '3_TwoBlockCPUE'
  scPurple <-"4_Dwtag01"
  scOrange <-"5_EffortCreep"
  
  p1 <- ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario)) +
    geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scRed),size=1) +
    geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scBlack),size=1.5) +
    geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scBlue),size=1) +
    geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scGreen),size=1) +
    geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scPurple),size=1) +
    geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scOrange),size=1) +
    scale_color_manual(values = c( 'red','black','blue','green','purple','orange')) +xlim(1950,2023)+ylim(0,4.5e6)+
    xlab("Year")+ ylab("SSB")+
    theme_fun()
  p1
  SavePlot(paste0('Stepwise_update_SSB_',max(i),'_',nmplot),15,10)
  
  
  # Comparison SSB/b0
  
  df <- mod_sum$SpawnBio
  B0 <- as.vector(df[df$Label=="SSB_Virgin",-c(length(i)+1,length(i)+2)])
  df[,-c(length(i)+1,length(i)+2)] <- df[,-c(length(i)+1,length(i)+2)]/B0
  df <- df %>% rename_at(1:length(sub_scs),~sub_scs)
  df_long <- df %>% pivot_longer(cols =sub_scs, names_to = "scenario", values_to = "value")
  sub_df_long <- df_long %>% subset(scenario %in% sub_scs)  %>% dplyr::filter(!(scenario=="00_BC" & Yr>=297))
  sub_df_long <- sub_df_long %>% subset(scenario %in% sub_scs) 
  sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  sub_df_long  <- sub_df_long %>%   mutate(yrqtr=qtr2yearqtr(Yr,1950,13))%>%
    mutate(yrqtr_season=floor((yrqtr-floor(yrqtr))*4+1))
  
  #SavePlot(paste0('Stepwise_update_SSB',max(i)),15,10)
  scRed <-  "0_BC_2021"#, "1_OneBlock_LLsel","2_TwoBlock_LLsel",'3_TwoBlockCPUE',"4_Dwtag01","5_EffortCreep")
  scBlack <- "1_OneBlock_LLsel"
  scBlue <- "2_TwoBlock_LLsel"
  scGreen <- '3_TwoBlockCPUE'
  scPurple <-"4_Dwtag01"
  scOrange <-"5_EffortCreep"
  
  p2 <- ggplot(sub_df_long,aes(x=yrqtr,y=value,group =scenario)) +
    geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scRed),size=1) +
    geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scBlack),size=1.5) +
    geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scBlue),size=1) +
    geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scGreen),size=1) +
    geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scPurple),size=1) +
    geom_line(aes(color=scenario), data = . %>% subset(., scenario %in% scOrange),size=1) +
    scale_color_manual(values = c( 'red','black','blue','green','purple','orange')) +xlim(1950,2023)+ylim(0,1.5)+
    xlab("Year")+ ylab("SSB/B0")+
    theme_fun()
  P2
  SavePlot(paste0('Stepwise_update_SSB_B0_',max(i),'_',nmplot),15,10)
  
  ggarrange(p1,p2,ncol=2,common.legend = TRUE)
  
  SavePlot(paste0('Stepwise_update_SSB_and_SSB_B0_',max(i),'_',nmplot),15,10)
  