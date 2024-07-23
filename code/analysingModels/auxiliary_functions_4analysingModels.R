#
#   some functions to facilitate the analysis of the outputs of ss3
#
#     summary.check- creates a data.frame with some general variables to check initial convregency issues.
#     savePlot - a default function to save plots
#     aggregate.ssMod <- read and aggregate different ss3 scenarios, to facilitate comparison
#
#..........................................................................

#.......................................................................
#
#       SAVEPLOT
#...........................................................

SavePlot<-function(plotname,width=8,height=4){
  file <- file.path(dir_plot,paste0(plotname,'.png'))
  dev.print(png,file,width=width,height=height,units='in',res=300)
}

#.......................................................................
#
#       PLOT SETTINGS
#...........................................................

theme_fun <-function(){
  theme_bw()+
  theme(axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size=18, angle=0),
        axis.text.y = element_text(size=18, angle=0),
        title=element_text(size=18,angle=0),
        legend.text = element_text(size=12))}
#.......................................................................
#
#       MODEL PERFORMANCE 
#...........................................................
mod.performance <- function(sc_nm,desc,sc_ss3,summary_file){
if (file.exists(summary_file)) { 

  df <- read.csv(summary_file)
  if(!sc_nm %in% df$Name ) {
    sc_sum <- summary.check(sc_nm, sc_ss3,desc)
    df <- as.data.frame(rbind(df, sc_sum))
    print("The file exists, but adding a new row") 
  }}else { 
    print("The file does not exist") 
    df <- summary.check(sc_nm, sc_ss3,desc)
  }
  return(df)
}
#.......................................................................
#
#       SUMMARY.CHECK
#...........................................................

summary.check <- function(sc.nm, ss3,desc){
  nms <- c( "Name",	"Description","LKL",	"conv",	"hessian",	"n_param",	"n_param_bound", "Time","Read_sspar")
  df <- data.frame(matrix(ncol=length(nms)))
  colnames(df) <- nms
  #Remove forecast parameters
  pattern <- c("ForeRecr")
  n_param  <- ss3$parameters   %>%  filter(!grepl(pattern,Label))%>% filter(!is.na(Active_Cnt)) %>% 
    summarise(n = n())
  
  n_param_bound  <- ss3$parameters   %>%  filter(!grepl(pattern,Label)) %>% filter(!is.na(Active_Cnt)) %>%  filter(Status %in% c("LO","HI")) %>% 
    summarise(n = n())
  
  aux <- c(sc.nm,desc,ss3$likelihoods_used["TOTAL","values"],ss3$maximum_gradient_component, ss3$log_det_hessian,n_param, n_param_bound,ss3$RunTime,ss3$Start_from_par)
  df[1, ] <- aux
  return(df)
}


#.......................................................................
#
#       AGGREGATE.SSMOD
#...........................................................

aggregate.ssMod <- function(scs, path_scs){
  mod_sum <- NULL
  for (i in 1:length(scs)){
    sc_dir = path_scs[i]
    ss3 <- SSgetoutput(dirvec=sc_dir ,  getcovar=FALSE,forecast=FALSE)
    mod_sum <- c(mod_sum,ss3)
    rm(ss3)

  }
  return(SSsummarize(mod_sum))
  }






