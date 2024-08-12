# Save here all R auxiliary functions
# -------------------------------------------------------------------------
# Dan Fu's functions:

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

get_4Aarea_from_lonlat = function(Long, Lat) {
  # Areas: 1 (1a), 2 (1b), 3 (2), 4 (3), 5 (4)
  out_area =  ifelse(Lat > 10 & Long <= 75, 1, # originally, it was Long < 75, but produced unassiged areas for GI after GridCell correction
                     ifelse((Lat > -10 & Lat < 10 & Long  < 60) | (Lat > -15 & Lat < 10 & Long  > 60 & Long <= 75), 2, # originally, it was Long < 75, but produced unassiged areas for GI after GridCell correction
                            ifelse((Lat > -60 & Lat < -10 & Long > 20 & Long < 40) | (Lat > -30 & Lat < -10 & Long > 40 & Long  < 60), 3,
                                   ifelse((Lat > -60 & Lat < -30 & Long > 40 & Long < 60) | (Lat > -60 & Lat <= -15 & Long  > 60 & Long <= 150), 4,
                                          ifelse(Lat > -15 & Long > 75 & Long < 150, 5, 0)))))		
  return(out_area)
  
}


qtr2yearqtr = function(qtr,initial,base) {
  yearqtr = (qtr-base)/4+initial+1/8
}

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
# Filter LF data for SS input:

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
# Get area from lon lat:
get_area_4A = function(data) { # columns should be named Lat Long
  
  data = data %>% mutate(Area = ifelse(Lat > 10 & Long <= 75, 1, # originally, it was Long < 75, but produced unassigned areas for GI after GridCell correction
         ifelse((Lat > -10 & Lat < 10 & Long  < 60) | (Lat > -15 & Lat < 10 & Long  > 60 & Long <= 75), 2, # originally, it was Long < 75, but produced unassigned areas for GI after GridCell correction
                ifelse((Lat > -60 & Lat < -10 & Long > 20 & Long < 40) | (Lat > -30 & Lat < -10 & Long > 40 & Long  < 60), 3,
                       ifelse((Lat > -60 & Lat < -30 & Long > 40 & Long < 60) | (Lat > -60 & Lat <= -15 & Long  > 60 & Long <= 150), 4,
                              ifelse(Lat > -15 & Long > 75 & Long < 150, 5, 0)))))
  )
  return(data)
  
}
