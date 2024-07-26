require(dplyr)
require(purrr)
require(ggplot2)
source('sharepoint_path.R')
source('code/data_exploration/auxiliary_functions.R')
data_folder = 'data/processed'

# -------------------------------------------------------------------------
# Make std grid 
# Specify data folder in sharepoint:

# Read datasets:
catch_spt = read_csv(file.path(shrpoint_path, data_folder, 'catch_grid.csv'))
size_spt = read_csv(file.path(shrpoint_path, data_folder, 'size_grid.csv'))
# Some processing:
colnames(catch_spt) = tolower(colnames(catch_spt))
colnames(size_spt) = tolower(colnames(size_spt))
catch_spt = catch_spt %>% mutate(grid = as.character(grid))
size_spt = size_spt %>% mutate(grid = as.character(grid))

# Remember that Long Lat columns are the centroid of the IOTC grid, calculated in LF or CE scripts.

# -------------------------------------------------------------------------
# Create a standard grid (5x5) in the IO:
grid_size = 5

merged_spt = rbind(catch_spt[,c('long', 'lat')], size_spt[,c('long', 'lat')])
MyPoints = merged_spt %>% st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
range(MyPoints$long)
range(MyPoints$lat)
# Specify lower left corner:
min_lon = 25
min_lat = -60
stdGrid = st_make_grid(MyPoints, cellsize = c(grid_size, grid_size), offset = c(min_lon, min_lat)) %>%
  st_set_crs(4326) %>% st_sf() %>% dplyr::mutate(grid_ID = 1:n())
# ggplot(stdGrid) + geom_sf()
save(stdGrid, file = file.path(shrpoint_path, data_folder, 'stdGrid.RData'))
# st_centroid(stdGrid) %>% dplyr::mutate(long = sf::st_coordinates(.)[,1], lat = sf::st_coordinates(.)[,2])

# -------------------------------------------------------------------------
# Create grid for catch data:
# See IOTC code
catch_spt = catch_spt %>% mutate(grid_type = str_sub(grid, 1, 1))
# No need to standardize catch data since grid type is always 6 (5x5, same as std grid)
# Double check this:
table(catch_spt$grid_type)

# Do some processing:
catch_spt = catch_spt %>% mutate(samp_ID = 1:n()) # add samp_ID column
catchPoints = catch_spt %>% st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
# Find stdGrid that corresponds to each catch point (it takes a while):
catchStd = st_join(stdGrid, left = TRUE, catchPoints) %>% na.omit
save(catchStd, file = file.path(shrpoint_path, data_folder, 'catchStd.RData'))

# -------------------------------------------------------------------------
# Create grid for size data:
# See IOTC code

size_spt = size_spt %>% mutate(grid_type = str_sub(grid, 1, 1))
# Do some processing:
size_spt = size_spt %>% mutate(samp_ID = 1:n()) # add samp_ID column
# Transform to std grid:
size_spt_tf = size_spt %>% group_split(samp_ID) %>% 
  purrr::map(~ transform_to_stdgrid(.x)) %>% 
  list_rbind()
sizePoints = size_spt_tf %>% st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
# Find stdGrid that corresponds to each size point (it takes a while):
sizeStd = st_join(stdGrid, left = TRUE, sizePoints) %>% na.omit
save(sizeStd, file = file.path(shrpoint_path, data_folder, 'sizeStd.RData'))

# -------------------------------------------------------------------------

# Limits for plotting later:
limites = data.frame(xlim1 = min_lon, ylim1 = min_lat,
                     xlim2 = ceiling((max(MyPoints$Long))/5)*5, ylim2 = ceiling((max(MyPoints$Lat))/5)*5)
save(limites, file = file.path('data', 'limites.RData'))
