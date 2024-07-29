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