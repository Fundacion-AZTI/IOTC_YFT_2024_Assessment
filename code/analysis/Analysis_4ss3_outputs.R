#
#       READ ss3 outputs of different and create a summary performance table
#       compare ss3 outputs of different scenarios
#............................................................



library(r4ss)
library(dplyr)
library(tidyr)
library(ggplot2)


proj_dir = here::here()
setwd(proj_dir)

source("code/analysis/auxiliary_functions_4analysis.R")
# Sharepoint path:
source('sharepoint_path.R')
setwd(shrpoint_path)

dir_plot <- "output/figures/"
dir_table <- "output/tables/"


#...................................................

#### READ, PLOT AND PERFORMANCE TABLE ####

#...................................................


scs <- c("4A_io_2021_v33017","4A_io_win_v330221","4A_io_lin_v330221")  #"4A_io_lin_v33021",
desc <- c("BaseCase_2021", "BaseCase_V3.30.22.1","BaseCase_linux_v3.30.22.1")  #"BaseCase_linux_v3.30.21",
scs_wd <-paste0("models/base_win_vs_lin/",scs)

plot_ss3 <- FALSE
add_perf_table <- TRUE

for(i in 1:length(scs)){
  sc_ss3 <- SS_output(dir=scs_wd[i],  repfile = "Report.sso",covar=F)
  if (plot_ss3) { SS_plots(sc_ss3,uncertainty=T,png=F,forecastplot=F, fitrange = TRUE, parrows=5, parcols=4, showdev= TRUE) }
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


mod_sum <- aggregate.ssMod(scs, scs_wd)

df <- mod_sum$Fvalue
df <- df %>% rename_at(1:length(scs),~scs)
df_long <- df %>% pivot_longer(cols =scs, names_to = "scenario", values_to = "value")
df_long$scenario <- as.factor(as.character(df_long$scenario))


ggplot(df_long,aes(x=Yr,y=value,group =scenario,colour = scenario))+geom_point( size = 1)+
  geom_line( )+xlim(200,298)+# geom_smooth(method = "loess")+
  scale_color_discrete(labels=levels(df_long$scenario))+
  xlab("Year")+ ylab("F/Fmsy")+theme_fun()
 

SavePlot('F_lin_vs_win',15,10)

### end ###


# Comparison SSB

df <- mod_sum$SpawnBio
df <- df %>% rename_at(1:length(scs),~scs)
df_long <- df %>% pivot_longer(cols =scs, names_to = "scenario", values_to = "value")

ggplot(df_long,aes(x=Yr,y=value,group =scenario,colour = scenario))+geom_point(   size = 1)+
  geom_line( )+xlim(200,298)+ylim(0,2.5e6)+# geom_smooth(method = "loess")+
  scale_color_discrete(labels=scs)+xlab("Year")+ ylab("SSB")+
  theme_fun()

SavePlot('SSB_lin_vs_win',15,10)


#### end ####
  
  
 
 