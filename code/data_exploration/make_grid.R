source('sharepoint_path.R')
source('code/data_exploration/parameters_for_plots.R')
source('code/data_exploration/auxiliary_functions.R')

data_folder = 'data/processed'
# Length bin column names (lowercase):
C_labels = c(paste0("l0", seq(from = 10, to = 98, by = 2)), paste0("l", seq(from = 100, to = 198, by = 2)))

# Define grid dim to use (degrees):
grid_size = 5

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

merged_spt = rbind(catch_spt[,c('long', 'lat')], size_spt[,c('long', 'lat')])
MyPoints = merged_spt %>% st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
range(MyPoints$long)
range(MyPoints$lat)
# Specify lower left corner:
min_lon = 25
min_lat = -60
stdGrid = st_make_grid(MyPoints, cellsize = c(grid_size, grid_size), offset = c(min_lon, min_lat)) %>%
  st_set_crs(4326) %>% st_sf() %>% dplyr::mutate(grid_ID = 1:n())
save(stdGrid, file = file.path(shrpoint_path, data_folder, paste0('stdGrid_', grid_size,'.RData')))

# Plot std grid:
worldmap = ne_countries(scale = "medium", returnclass = "sf")

ggplot(stdGrid) + geom_sf(fill = 'white') +
geom_sf(data = worldmap, fill = "gray60", color = "gray60") +
  coord_sf(expand = FALSE, xlim = c(25, 150), ylim = c(-60, 30)) 

# -------------------------------------------------------------------------
# Create grid for catch data:
# See IOTC code
catch_spt = catch_spt %>% mutate(grid_type = str_sub(grid, 1, 1))
# No need to standardize catch data since grid type is always 6 (5x5, same as std grid)
# Double check this:
table(catch_spt$grid_type)

# Do some processing:
catchPoints = catch_spt %>% st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
# Find stdGrid that corresponds to each catch point (it takes a while):
catchStd = st_join(stdGrid, left = TRUE, catchPoints) %>% na.omit
# Remove sf object since not important for now and may make things slower:
st_geometry(catchStd) = NULL
# Aggregate information by std grid:
# These variables are aggregated without any kind of weighting: month, schooltype, gear
catchStd = catchStd %>% group_by(grid_ID, year, quarter, gear, fleet, fisherycode, modelarea, modelfleet) %>%
            summarise_at(c('ncnofish', 'ncmtfish'), sum)
save(catchStd, file = file.path(shrpoint_path, data_folder, paste0('catchStd_', grid_size,'.RData')))

# -------------------------------------------------------------------------
# Create grid for size data:
# See IOTC code

size_spt = size_spt %>% mutate(grid_type = str_sub(grid, 1, 1))
# Do some processing:
size_spt = size_spt %>% mutate(samp_ID = 1:n()) # add samp_ID column
# Transform to std grid (from irregular grids to grid_size):
size_spt_tf = size_spt %>% group_split(samp_ID) %>% 
  purrr::map(~ transform_to_stdgrid(.x, std_res = grid_size)) %>% 
  list_rbind()
sizePoints = size_spt_tf %>% st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
# Find stdGrid that corresponds to each size point (it takes a while):
sizeStd = st_join(stdGrid, left = TRUE, sizePoints) %>% na.omit
# Remove sf object since not important for now and may make things slower:
st_geometry(sizeStd) = NULL
# Aggregate information by std grid (important for 1x1 grids in size data):
# These variables are aggregated without any kind of weighting: month, schooltype, gear
tmp_1 = sizeStd %>% group_by(grid_ID, year, quarter, gear, fleet, fisherycode, modelarea, modelfleet) %>%
          summarise_at('reporting_quality', median)
tmp_2 = sizeStd %>% group_by(grid_ID, year, quarter, gear, fleet, fisherycode, modelarea, modelfleet) %>%
  summarise_at(c('sno', C_labels), sum)
# Merge both datasets:
sizeStd = inner_join(tmp_1, tmp_2)
save(sizeStd, file = file.path(shrpoint_path, data_folder, paste0('sizeStd_', grid_size,'.RData')))


# -------------------------------------------------------------------------

# Merged size with catch data:
mergedStd = left_join(sizeStd, catchStd)

# Obtain imputation data frame (to fill in missing catch information for some grids):

# First level of aggregation (remove grid resolution, but keeps modelarea)
catchStdAgg1 = catchStd %>% group_by(year, quarter, fleet, gear, fisherycode, modelarea, modelfleet) %>%
  summarise_at(c('ncnofish', 'ncmtfish'), mean) # Important to get mean to not overweights missing information
# Second level of aggregation (remove grid and quarter resolution, but keeps modelarea)
catchStdAgg2 = catchStd %>% group_by(year, fleet, gear, fisherycode, modelarea, modelfleet) %>%
  summarise_at(c('ncnofish', 'ncmtfish'), mean) # Important to get mean to not overweights missing information
# Third level of aggregation (remove grid, quarter, and modelarea resolution)
catchStdAgg3 = catchStd %>% group_by(year, fleet, gear, fisherycode) %>%
  summarise_at(c('ncnofish', 'ncmtfish'), mean) # Important to get mean to not overweights missing information
# Last level of aggregation (remove grid, quarter, modelarea, fleet, and gear resolution)
catchStdAgg4 = catchStd %>% group_by(year, fisherycode) %>%
  summarise_at(c('ncnofish', 'ncmtfish'), mean) # Important to get mean to not overweights missing information

# Merge all aggregated data.frames:
mergedStd = left_join(mergedStd, catchStdAgg1, 
                      by = c('year', 'quarter', 'fleet', 'gear', 'fisherycode', 'modelarea', 'modelfleet'),
                      suffix = c('', '_1'))
mergedStd = left_join(mergedStd, catchStdAgg2, 
                      by = c('year', 'fleet', 'gear', 'fisherycode', 'modelarea', 'modelfleet'),
                      suffix = c('', '_2'))
mergedStd = left_join(mergedStd, catchStdAgg3, 
                      by = c('year', 'fleet', 'gear', 'fisherycode'),
                      suffix = c('', '_3'))
mergedStd = left_join(mergedStd, catchStdAgg4, 
                      by = c('year', 'fisherycode'),
                      suffix = c('', '_4'))

# Plot percentage of imputation by aggregation level:
imputationLevel = data.frame(level1 = sum(is.na(mergedStd$ncnofish))/nrow(mergedStd),
                             level2 = sum(is.na(mergedStd$ncnofish_1))/nrow(mergedStd),
                             level3 = sum(is.na(mergedStd$ncnofish_2))/nrow(mergedStd),
                             level4 = sum(is.na(mergedStd$ncnofish_3))/nrow(mergedStd))
imputationLevel = tidyr::gather(imputationLevel)
imputationLevel = imputationLevel %>% dplyr::add_row(key = 'level0', value = 1 - sum(imputationLevel$value), .before = 1)
imputationLevel = imputationLevel %>% mutate(perc = value*100, key = factor(key, labels = 0:4))

p1 = ggplot(imputationLevel, aes(x = key, y = perc)) +
  geom_bar(stat = "identity") +
  xlab('Imputation level') + ylab('% of grid observations')
ggsave(file.path(shrpoint_path, plot_dir, paste0('imputation_grid_', grid_size, img_type)), plot = p1,
       width = img_width*0.5, height = 70, units = 'mm', dpi = img_res)

# Now fill in NA in catch columns based on levels of imputation:
mergedStd = mergedStd %>% mutate(ncnofish = if_else(is.na(ncnofish), ncnofish_1, ncnofish), 
                                 ncmtfish = if_else(is.na(ncmtfish), ncmtfish_1, ncmtfish))
mergedStd = mergedStd %>% mutate(ncnofish = if_else(is.na(ncnofish), ncnofish_2, ncnofish), 
                                 ncmtfish = if_else(is.na(ncmtfish), ncmtfish_2, ncmtfish))
mergedStd = mergedStd %>% mutate(ncnofish = if_else(is.na(ncnofish), ncnofish_3, ncnofish), 
                                 ncmtfish = if_else(is.na(ncmtfish), ncmtfish_3, ncmtfish))
mergedStd = mergedStd %>% mutate(ncnofish = if_else(is.na(ncnofish), ncnofish_4, ncnofish), 
                                 ncmtfish = if_else(is.na(ncmtfish), ncmtfish_4, ncmtfish))

# Remove imputation columns:
mergedStd = mergedStd %>% select(-c('ncnofish_1', 'ncnofish_2', 'ncnofish_3', 'ncnofish_4',
                                    'ncmtfish_1', 'ncmtfish_2', 'ncmtfish_3', 'ncmtfish_4'))

# Make sure we dont have NA:
sum(is.na(mergedStd$ncnofish))
sum(is.na(mergedStd$ncmtfish))

# Save merged data frame:
save(mergedStd, file = file.path(shrpoint_path, data_folder, paste0('mergedStd_', grid_size,'.RData')))

