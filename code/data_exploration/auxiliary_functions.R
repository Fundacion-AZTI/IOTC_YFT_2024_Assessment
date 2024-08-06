

# -------------------------------------------------------------------------
# Transform from model time step (quarter) to year-quarter:
ssts2yq = function(qtr,initial = 1950, base = 13) {
  yearqtr = (qtr-base)/4+initial+1/8
  return(yearqtr)
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
    out_df = df %>% dplyr::slice(rep(1:n(), each = nrow(tmp_grid)))
    # Replace long lat values:
    out_df$long = tmp_grid$long
    out_df$lat = tmp_grid$lat
  }
  
  return(out_df)
}


# -------------------------------------------------------------------------
# Make grid plot:

add_sf_map = function(my_plot) {
  
  out_plot = my_plot + 
    geom_sf(data = worldmap, fill = "gray60", color = "gray60") +
    coord_sf(expand = FALSE, xlim = xLim, ylim = yLim) +
    xlab(NULL) + ylab(NULL) +
    scale_y_continuous(breaks = yBreaks) + scale_x_continuous(breaks = xBreaks) 
  return(out_plot)
  
}


