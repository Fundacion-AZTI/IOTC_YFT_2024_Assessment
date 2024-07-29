#
#       READ ss3 outputs of different scenarios
#               Create a summary performance table
#       compare ss3 outputs of different scenarios
#                 - WINDOWS OLD VS NEW VS
#                 - NEW VS WINDOWS VS LINUX
#............................................................



library(r4ss)
library(dplyr)
library(tidyr)
library(ggplot2)


proj_dir = here::here()
setwd(proj_dir)

source("code/analysingModels/auxiliary_functions_4analysingModels.R")
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
  
  ggplot(sub_df_long,aes(x=Yr,y=value,group =scenario,colour = scenario))+geom_point( size = 1)+
    geom_line( )+xlim(200,298)+
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
  
  ggplot(sub_df_long,aes(x=Yr,y=value,group =scenario,colour = scenario))+geom_point(   size = 1)+
    geom_line( )+xlim(200,298)+ylim(0,2.5e6)+
    scale_color_discrete(labels=levels(sub_df_long$scenario))+xlab("Year")+ ylab("SSB")+
    theme_fun()
  
  SavePlot('lin_vs_win_SSB',15,10)
  
  
  #### end ####
  
  
  #...................................................
  ### sub analysis
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
  #### READ, PLOT AND PERFORMANCE TABLE ####
  #...................................................
  
  
  scs <- c("00_BC","01_update_catch","02_update_cpue","03_update_length")  
  desc <- c("BaseCase", "update catch","update cpue","update length")  
  scs_wd <-paste0("models/update/",scs)
  
  plot_ss3 <- TRUE
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
  
  sub_scs <- scs
  sub_scs_wd <-paste0("models/update/",sub_scs)
  mod_sum <- aggregate.ssMod(sub_scs, sub_scs_wd)
  
  df <- mod_sum$Fvalue
  df <- df %>% rename_at(1:length(sub_scs),~sub_scs)
  df_long <- df %>% pivot_longer(cols =sub_scs, names_to = "scenario", values_to = "value")
  sub_df_long <- df_long %>% filter(scenario %in% sub_scs)
  sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  
  ggplot(sub_df_long,aes(x=Yr,y=value,group =scenario,colour = scenario))+geom_point( size = 1)+
    geom_line( )+xlim(200,298)+
    scale_color_discrete(labels=levels(sub_df_long$scenario))+
    xlab("Year")+ ylab("F/Fmsy")+theme_fun()
  
  SavePlot('Stepwise_F',15,10)
  
  ### end ###
  
  
  # Comparison SSB
  
  df <- mod_sum$SpawnBio
  df <- df %>% rename_at(1:length(sub_scs),~sub_scs)
  df_long <- df %>% pivot_longer(cols =sub_scs, names_to = "scenario", values_to = "value")
  sub_df_long <- df_long %>% filter(scenario %in% sub_scs)
  sub_df_long$scenario <- as.factor(as.character(sub_df_long$scenario))
  levels(sub_df_long$scenario) <- droplevels(sub_df_long$scenario)
  
  ggplot(sub_df_long,aes(x=Yr,y=value,group =scenario,colour = scenario))+geom_point(   size = 1)+
    geom_line( )+xlim(200,298)+ylim(0,2.5e6)+
    scale_color_discrete(labels=levels(sub_df_long$scenario))+xlab("Year")+ ylab("SSB")+
    theme_fun()
  
  SavePlot('Stepwise_SSB',15,10)
  
  
  #### end ####
  
  
  #...................................................
  ### sub analysis
  #...................................................
  
  sub_scs <- c("4A_io_2021_v33017",
               "4A_io_lin_v33022_FixedParam2_Fl11")  
  sub_scs_wd <-paste0("models/base_win_vs_lin/",sub_scs)
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
  
  
  SavePlot('Stepwise_F',15,10)
  
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
  
  SavePlot('Stepwise_SSB',15,10)
  
  #### end ####
  
  