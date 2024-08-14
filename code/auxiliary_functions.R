# -------------------------------------------------------------------------
# Transform from SS model time step (quarter) to year-quarter:
ssts2yq = function(qtr,initial = 1950, base = 13) {
  yearqtr = (qtr-base)/4+initial+1/8
  return(yearqtr)
}


# -------------------------------------------------------------------------
# Add land map to a existing sf plot:
add_sf_map = function(my_plot) {
  
  out_plot = my_plot + 
    geom_sf(data = worldmap, fill = "gray60", color = "gray60") +
    coord_sf(expand = FALSE, xlim = xLim, ylim = yLim) +
    xlab(NULL) + ylab(NULL) +
    scale_y_continuous(breaks = yBreaks) + scale_x_continuous(breaks = xBreaks) 
  return(out_plot)
  
}

# -------------------------------------------------------------------------
# Get longitude and latitude information from Grid (Dan's function)

get.lat.from.id = function(id) {
  id = as.character(id)
  size_grid_ = substr(id,1,1)
  
  size_lat_ = ifelse(size_grid_==1,30, # corrected based on metadata
                     ifelse(size_grid_==2,10,
                            ifelse(size_grid_==3,10,
                                   ifelse(size_grid_==4,20,
                                          ifelse(size_grid_==5,1,5)))))
  quadrant =substr(id,2,2)
  lat_label  = substr(id,3,4)
  lat = as.numeric(lat_label)+size_lat_/2
  lat = ifelse(quadrant == 1  | quadrant == 4, lat,-lat)			
}

get.long.from.id = function(id) {
  id = as.character(id)
  size_grid_ = substr(id,1,1)
  
  size_long_ = ifelse(size_grid_==1,30, # corrected based on metadata
                      ifelse(size_grid_==2,20,
                             ifelse(size_grid_==3,10,
                                    ifelse(size_grid_==4,20,
                                           ifelse(size_grid_==5,1,5)))))
  
  quadrant = substr(id,2,2)
  long_label  = substr(id,5,7)
  long = as.numeric(long_label)+size_long_/2
  long = ifelse(quadrant == 1  | quadrant == 2,long, -long+360)
}


# -------------------------------------------------------------------------
# Get 4A 'Area' column based on longitude and latitude:
get_4Aarea_from_lonlat = function(Long, Lat) {
  # Areas: 1 (1a), 2 (1b), 3 (2), 4 (3), 5 (4)
  out_area =  ifelse(Lat > 10 & Long <= 75, 1, # originally, it was Long < 75, but produced unassigned areas for GI after GridCell correction (Type 1 30x30)
                     ifelse((Lat > -10 & Lat < 10 & Long  < 60) | (Lat > -15 & Lat < 10 & Long  > 60 & Long <= 75), 2, # originally, it was Long < 75, but produced unassigned areas for GI after GridCell correction (Type 1 30x30)
                            ifelse((Lat > -60 & Lat < -10 & Long > 20 & Long < 40) | (Lat > -30 & Lat < -10 & Long > 40 & Long  < 60), 3,
                                   ifelse((Lat > -60 & Lat < -30 & Long > 40 & Long < 60) | (Lat > -60 & Lat <= -15 & Long  > 60 & Long <= 160), 4, # originally, the max long was 150, but new data std had data until long 160. 
                                          ifelse(Lat > -15 & Long > 75 & Long < 150, 5, 0)))))		
  return(out_area)
  
}


# -------------------------------------------------------------------------
# Create 4A AssessmentArea and ModelArea columns from 'Area' column:
create_4Aarea_cols = function(Data) {
  
  Data = plyr::ddply(Data,c("Area","FisheryCode"),.fun = function(d) {
    d$AssessmentArea = d$Area
    d = mutate_cond(d,FisheryCode=='FS' & Area ==1, AssessmentArea = 2)
    d = mutate_cond(d,FisheryCode=='FS' & Area ==4, AssessmentArea = 5)
    d = mutate_cond(d,FisheryCode=='LS' & Area ==1, AssessmentArea = 2)
    d = mutate_cond(d,FisheryCode=='LS' & Area ==4, AssessmentArea = 5)
    d = mutate_cond(d,FisheryCode=='LF', AssessmentArea = 5)
    d = mutate_cond(d,FisheryCode=='BB', AssessmentArea = 2)
    d = mutate_cond(d,FisheryCode=='GI' & (Area == 2 | Area==3), AssessmentArea = 1)
    d = mutate_cond(d,FisheryCode=='GI' & Area == 4, AssessmentArea = 5)
    d = mutate_cond(d,FisheryCode=='HD', AssessmentArea = 1)
    d = mutate_cond(d,FisheryCode=='TR' & Area == 1, AssessmentArea = 2)
    d = mutate_cond(d,FisheryCode=='TR' & Area == 4, AssessmentArea = 5)
    d = mutate_cond(d,FisheryCode=='OT' & (Area == 2 | Area==3), AssessmentArea = 1)
    d = mutate_cond(d,FisheryCode=='OT' & Area == 4, AssessmentArea = 5)
    return(d)})	
  
  Data = plyr::ddply(Data,c("Area","FisheryCode"),.fun = function(d) {
    d$AssessmentAreaName = d$AssessmentArea
    d$ModelArea = d$AssessmentArea
    d = mutate_cond(d,AssessmentArea=='1',AssessmentAreaName = '1a')
    d = mutate_cond(d,AssessmentArea=='2',AssessmentAreaName = '1b')
    d = mutate_cond(d,AssessmentArea=='3',AssessmentAreaName = '2')
    d = mutate_cond(d,AssessmentArea=='4',AssessmentAreaName = '3')
    d = mutate_cond(d,AssessmentArea=='5',AssessmentAreaName = '4')
    d = mutate_cond(d,AssessmentArea=='1',ModelArea = '1')
    d = mutate_cond(d,AssessmentArea=='2',ModelArea = '1')
    d = mutate_cond(d,AssessmentArea=='3',ModelArea = '2')
    d = mutate_cond(d,AssessmentArea=='4',ModelArea = '3')
    d = mutate_cond(d,AssessmentArea=='5',ModelArea = '4')	
    return(d)})
  
  return(Data)
  
}


# -------------------------------------------------------------------------
# Several functions relevant for data preparation

yearqtr2qtr = function(year,qtr,initial,base) {
  qtr = (base-1)+4*(year-initial)+qtr
}

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

Paste <- function (..., sep = "")  paste(..., sep = sep)
Sum <- function (..., na.rm = T)  sum(..., na.rm = na.rm)


# -------------------------------------------------------------------------
# Filter 4A LF data for SS input:
filter_LF_4A = function(data) {
  
  work = data %>% 
    dplyr::filter(!(Fleet %in% c('TWN','SYC') & Gear == 'LL')) %>%
    dplyr::filter(!(ModelFishery == "LL 1a" & Year %in% c(1970:1995, 2010:2020))) %>% # please check if 2020-2022 data needs to be excluded
    dplyr::filter(!(ModelFishery == "LL 1b" & Year %in% c(1950:1959))) %>%
    dplyr::filter(!(ModelFishery == "LL 2" & Year %in% c(1950:1959))) %>%
    dplyr::filter(!(ModelFishery == "LL 3" & Year %in% c(1950:1959))) %>%
    dplyr::filter(!(ModelFishery == "LL 4" & Year %in% c(1950:1959,2001:2005,2015,2019))) %>%  # please check if 2020-2022 data needs to be excluded
    dplyr::filter(!(ModelFishery == "GI 4" & Year %in% c(1975:1987))) %>%
    dplyr::filter(!(ModelFishery == "HD 1a" & Year %in% c(1950:2007))) %>%
    dplyr::filter(!(ModelFishery == "OT 4" & Year %in% c(1983,2016))) %>%
    dplyr::filter(!(ModelFishery == "TR 4" & Year %in% c(2016:2019))) %>%  # please check if 2020-2022 data needs to be excluded
    dplyr::filter(!(ModelFishery == "TR 1b")) %>%
    dplyr::filter(!(ModelFishery == "TR 2")) 
  
  return(work)
  
}

# -------------------------------------------------------------------------
# Repeat rows to create std Grid:

transform_to_stdgrid = function(df, std_res = 5) { # input will be a data.frame with single row
  
  # - do nothing with grid_type == 5 (1x1) or 6 (5x5)
  # - catch data have all grid_type == 6
  # - when grid_type == 5, information inside the grid will be combined
  # without weighting
  # std_res = 5 # for long and lat, since std grid is 5x5
  if(df$grid_type %in% c('5', '6')) {
    out_df = df
  } else {
    if(df$grid_type %in% c('4')) {
      lat_res = 20 # lat size
      long_res = 20 # long size
    }
    if(df$grid_type %in% c('3')) {
      lat_res = 10 # lat size
      long_res = 10 # long size
    }
    if(df$grid_type %in% c('2')) {
      lat_res = 10 # lat size
      long_res = 20 # long size
    }
    if(df$grid_type %in% c('1')) {
      lat_res = 30 # lat size
      long_res = 30 # long size
    }
    lat_range = c(df$lat - lat_res*0.5 + std_res*0.5, df$lat + lat_res*0.5 - std_res*0.5)
    long_range = c(df$long - long_res*0.5 + std_res*0.5, df$long + long_res*0.5 - std_res*0.5)
    tmp_grid = expand.grid(long = seq(from = long_range[1], to = long_range[2], by = std_res),
                           lat = seq(from = lat_range[1], to = lat_range[2], by = std_res))
    n_rep = nrow(tmp_grid)
    out_df = df %>% dplyr::slice(rep(1:n(), each = n_rep))
    # Divide sno and len freq by number of rows
    out_df = out_df %>% mutate(sno = sno/n_rep)
    out_df = out_df %>% mutate(across(l010:l198 , ~ ./n_rep))
    # Replace long lat values:
    out_df$long = tmp_grid$long
    out_df$lat = tmp_grid$lat
  }
  
  return(out_df)
}

# -------------------------------------------------------------------------
# Several growth-related functions:

growth2stage.f<- function(age, param)
{
  Linf <- param[1]
  k1 <- param[2]
  k2 <- param[3]
  astar <- param[4]
  frac <- param[5]
  a0 <- param[6]
  
  a <- age + a0
  
  # calculate a0 for second vb curve relative to a0 for first vb curve
  a02.rel.a01 <- astar+1/k2*log(1-frac*(1-exp(-k1*astar)))
  g <- a*0
  tf <- a<=astar & !is.na(a)
  g[tf] <- Linf*frac*(1-exp(-k1*a[tf]))
  tf <- a>astar & !is.na(a)
  g[tf] <- Linf*(1-exp(-k2*(a[tf] - a02.rel.a01)))
  g[is.na(a)]<- NA
  
  return(g)
}

sqdiff.2stage <- function(age, len, param) {
  exp.len <- growth2stage.f(age, param)
  sqdiff <- (len-exp.len)^2
  return(sqdiff)
}


calc.age.2stage <- function(FL, param, min.age, max.age) {
  age.est <- rep(NA,length(FL))
  for(i in 1:length(FL))
    age.est[i] <- nlminb(5, sqdiff.2stage, len=FL[i], param=param, lower=min.age, upper=max.age)$par
  return(age.est)
}


# -------------------------------------------------------------------------
#   some functions to facilitate the analysis of the outputs of ss3
#
#     summary.check- creates a data.frame with some general variables to check initial convergency issues.
#     savePlot - a default function to save plots
#     aggregate.ssMod <- read and aggregate different ss3 scenarios, to facilitate comparison
#

# -------------------------------------------------------------------------
# SAVEPLOT

SavePlot<-function(plotname,width=8,height=4){
  file <- file.path(dir_plot,paste0(plotname,'.png'))
  dev.print(png,file,width=width,height=height,units='in',res=300)
}

# -------------------------------------------------------------------------
# PLOT SETTINGS

theme_fun <-function(){
  theme_bw()+
    theme(axis.title.x = element_text(size = 18, face = "bold"),
          axis.title.y = element_text(size = 18, face = "bold"),
          axis.text.x = element_text(size=18, angle=0),
          axis.text.y = element_text(size=18, angle=0),
          title=element_text(size=18,angle=0),
          legend.text = element_text(size=12))
}

# -------------------------------------------------------------------------
# MODEL PERFORMANCE 
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

# -------------------------------------------------------------------------
#  SUMMARY.CHECK

summary.check <- function(sc.nm, ss3,desc){
  nms <- c( "Name",	"Description","LKL",	"conv",	"hessian",	"n_param",	"n_param_bound", "Time","Read_sspar")
  df <- data.frame(matrix(ncol=length(nms)))
  colnames(df) <- nms
  #Remove forecast parameters
  pattern <- c("ForeRecr")
  n_param  <- ss3$parameters   %>%  dplyr::filter(!grepl(pattern,Label)) %>% dplyr::filter(!is.na(Active_Cnt)) %>% 
    summarise(n = n())
  
  n_param_bound  <- ss3$parameters   %>%  dplyr::filter(!grepl(pattern,Label)) %>% dplyr::filter(!is.na(Active_Cnt)) %>%   dplyr::filter(Status %in% c("LO","HI")) %>% 
    summarise(n = n())
  
  aux <- c(sc.nm,desc,ss3$likelihoods_used["TOTAL","values"],ss3$maximum_gradient_component, ss3$log_det_hessian,n_param, n_param_bound,ss3$RunTime,ss3$Start_from_par)
  df[1, ] <- aux
  return(df)
}

# -------------------------------------------------------------------------
# AGGREGATE.SSMOD

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
