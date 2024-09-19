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
img_width = 170 # max width image

# Theme for ggplot:
theme_set(theme_bw())

# Define colors for fleets:
fleet_pal = brewer.pal(n = 9, name = 'Set1')
fleet_col = c('FS' = fleet_pal[1], 'LS' = fleet_pal[2], 'LL' = fleet_pal[3], 'LF' = fleet_pal[4], 
              'GI' = fleet_pal[5], 'HD' = fleet_pal[6], 'TR' = fleet_pal[7], 'BB' = fleet_pal[8], 'OT' = fleet_pal[9])

# Define data.frames with lines for spatial config ------------------------
reg_lines_4A = rbind(data.frame(lon1 = 75, lat1 = 12.5, lon2 = 75, lat2 = -15), # vertical 
                     data.frame(lon1 = 60, lat1 = -10, lon2 = 60, lat2 = -30), # vertical 
                     data.frame(lon1 = 40, lat1 = -30, lon2 = 40, lat2 = -40), # vertical 
                     data.frame(lon1 = 40, lat1 = -10, lon2 = 60, lat2 = -10), # horizontal 
                     data.frame(lon1 = 60, lat1 = -15, lon2 = 120, lat2 = -15), # horizontal 
                     data.frame(lon1 = 40, lat1 = -30, lon2 = 60, lat2 = -30) # horizontal 
)
reg_lines_1ab = data.frame(lon1 = 52, lat1 = 10, lon2 = 75, lat2 = 10) # 1a and 1b
reg_lines_2A = rbind(data.frame(lon1 = 75, lat1 = 20, lon2 = 75, lat2 = -15), # vertical 
                     data.frame(lon1 = 60, lat1 = -15, lon2 = 60, lat2 = -30), # vertical 
                     data.frame(lon1 = 40, lat1 = -30, lon2 = 40, lat2 = -40), # vertical 
                     data.frame(lon1 = 60, lat1 = -15, lon2 = 75, lat2 = -15), # horizontal 
                     data.frame(lon1 = 40, lat1 = -30, lon2 = 60, lat2 = -30) # horizontal 
)

# Folder in sharepoint where to save plots:
plot_dir = 'output/figures'

# Folder in sharepoint where to save tables:
tab_dir = 'output/tables'