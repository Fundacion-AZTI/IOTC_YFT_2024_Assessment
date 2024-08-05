# These parameters will be used in all R scripts for plotting

# For maps, define limits:
xLim = c(20, 120)
yLim = c(-40, 30)

# Read land map information:
worldmap = ne_countries(scale = "medium", returnclass = "sf")

# Define breaks in maps:
yBreaks = seq(from = -40, to = 40, by = 20)
xBreaks = seq(from = 40, to = 120, by = 30)

# Define image parameters:
img_type = '.png'
img_res = 300 # dpi

# Theme for ggplot:
theme_set(theme_bw())

# Define colors for fleets:
fleet_pal = brewer.pal(n = 9, name = 'Set1')
fleet_col = c('FS' = fleet_pal[1], 'LS' = fleet_pal[2], 'LL' = fleet_pal[3], 'LF' = fleet_pal[4], 
              'GI' = fleet_pal[5], 'HD' = fleet_pal[6], 'TR' = fleet_pal[7], 'BB' = fleet_pal[8], 'OT' = fleet_pal[9])

# Folder in sharepoint where to save plots:
plot_dir = 'output/figures'