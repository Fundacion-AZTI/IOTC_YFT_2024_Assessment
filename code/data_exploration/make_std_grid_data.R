source('sharepoint_path.R')
source('code/parameters_for_plots.R')
source('code/auxiliary_functions.R')

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
# Spatially standardize catch data (e.g., from 30x30 to grid_size x grid_size):
catch_spt = catch_spt %>% mutate(grid_type = str_sub(grid, 1, 1))
# No need to standardize catch data since grid type is always 6 (5x5, same as std grid)
# Double check this:
table(catch_spt$grid_type)

# -------------------------------------------------------------------------
# Spatially standardize catch data (e.g., from 30x30 to grid_size x grid_size):
size_spt = size_spt %>% mutate(grid_type = str_sub(grid, 1, 1))
# Do some processing:
size_spt = size_spt %>% mutate(samp_ID = 1:n()) # add samp_ID column
dim(size_spt)
# Transform to std grid (from larger irregular grids to grid_size):
size_spt_tf = size_spt %>% group_split(samp_ID) %>% 
  purrr::map(~ transform_to_stdgrid(.x, std_res = grid_size)) %>% 
  list_rbind()
dim(size_spt_tf)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Create a standard grid in the IO based on std points of catch and size data:

merged_spt = rbind(catch_spt[,c('long', 'lat')], size_spt_tf[,c('long', 'lat')])
MyPoints = merged_spt %>% st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
range(MyPoints$long)
range(MyPoints$lat)
# Specify lower left corner:
min_lon = 20
min_lat = -60
stdGrid = st_make_grid(MyPoints, cellsize = c(grid_size, grid_size), offset = c(min_lon, min_lat)) %>%
  st_set_crs(4326) %>% st_sf() %>% dplyr::mutate(grid_ID = 1:n())

# Calculate area on land of each grid (in km2):
stdGrid_temp = stdGrid %>% group_split(grid_ID) %>% 
                purrr::map(~ calculate_area_on_land(.x)) %>%
                list_rbind() %>% select(-geometry)
stdGrid = left_join(stdGrid, stdGrid_temp, by = 'grid_ID')
save(stdGrid, file = file.path(shrpoint_path, data_folder, paste0('stdGrid_', grid_size,'.RData')))

# Create points std Grid to get lon lat information later:
stdGridPoint = st_centroid(stdGrid) %>% 
  dplyr::mutate(lon = sf::st_coordinates(.)[,1], lat = sf::st_coordinates(.)[,2])
st_geometry(stdGridPoint) = NULL

# Plot std grid:
ggplot() +
  geom_sf(data = worldmap, fill = 'grey60', color = "gray60") +
  geom_sf(data = stdGrid, fill = NA) +
  coord_sf(expand = FALSE, xlim = c(15, 165), ylim = c(-65, 35))
  

# -------------------------------------------------------------------------
# Merge std grid with catch data:
catchPoints = catch_spt %>% st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
dim(catchPoints)
# Find stdGrid that corresponds to each catch point (it takes a while):
catchStd = st_join(stdGrid, left = TRUE, catchPoints) %>% na.omit
dim(catchStd) # should be the same as before
# Remove sf object since not important for now and may make things slower:
st_geometry(catchStd) = NULL
# Do not remove grids on land since we are assuming those are correct and we dont want to remove catch information
# Aggregate information by std grid:
# These variables are aggregated without any kind of weighting: month, schooltype
catchStd = catchStd %>% group_by(grid_ID, year, quarter, gear, fleet, fisherycode, modelarea, modelfleet, modelfishery) %>%
  summarise_at(c('ncnofish', 'ncmtfish'), sum)
# Get lon lat information:
catchStd = left_join(catchStd, stdGridPoint, by = 'grid_ID')
# Save:
save(catchStd, file = file.path(shrpoint_path, data_folder, paste0('catchStd_', grid_size,'.RData')))


# -------------------------------------------------------------------------
# Merge std grid with size data:
sizePoints = size_spt_tf %>% st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)
dim(sizePoints)
sum(is.na(sizePoints$reporting_quality)) # check no NAs for reporting quality
# Find stdGrid that corresponds to each size point (it takes a while):
sizeStd = st_join(stdGrid, left = TRUE, sizePoints) %>% dplyr::filter(!is.na(year))
dim(sizeStd) # should be the same as before
sum(is.na(sizeStd$reporting_quality)) # check no NAs
# Remove sf object since not important for now and may make things slower:
st_geometry(sizeStd) = NULL
# Remove extrapolated grids (i.e., grid_type = 1:4) that are 99.9% on land:
sizeStd = sizeStd %>% dplyr::filter(!(grid_type %in% as.character(1:4) & portion_on_land >= 0.999))
dim(sizeStd)
# Now correct sno and length freq for extrapolated grids (divide proportionally)
# Do it in two parts to save some time:
df_1 = sizeStd %>% dplyr::filter(grid_type %in% as.character(5:6))
df_2 = sizeStd %>% dplyr::filter(grid_type %in% as.character(1:4)) %>% group_split(samp_ID) %>% 
  purrr::map(~ correct_size_comp(.x)) %>% 
  list_rbind()
sizeStd = rbind(df_1, df_2)

# Aggregate information by std grid (important for 1x1 grids in size data):
# These variables are aggregated without any kind of weighting: month, schooltype
tmp_1 = sizeStd %>% group_by(grid_ID, year, quarter, gear, fleet, fisherycode, modelarea, modelfleet, modelfishery) %>%
          summarise_at(c('reporting_quality'), mean) # IMPORTANT: mean reporting quality
tmp_2 = sizeStd %>% group_by(grid_ID, year, quarter, gear, fleet, fisherycode, modelarea, modelfleet, modelfishery) %>%
  summarise_at(c('sno', C_labels), sum)
# Merge both datasets:
sizeStd = inner_join(tmp_1, tmp_2)
save(sizeStd, file = file.path(shrpoint_path, data_folder, paste0('sizeStd_', grid_size,'.RData')))


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Merged size with catch data:
# Remove some unnecessary columns from catch data:
mergedStd = left_join(sizeStd, catchStd %>% select(-c('area_on_land', 'grid_area', 'portion_on_land', 'lon', 'lat')))

# Missing catch grid information (%)
sum(is.na(mergedStd$ncnofish))/nrow(mergedStd)

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

# Now fill in NA in catch columns based on levels of imputation:
mergedStd = mergedStd %>% mutate(type_imputation = if_else(!is.na(ncnofish), 0, NA))
mergedStd = mergedStd %>% mutate(ncnofish = if_else(is.na(ncnofish), ncnofish_1, ncnofish), 
                                 ncmtfish = if_else(is.na(ncmtfish), ncmtfish_1, ncmtfish),
                                 type_imputation = if_else(is.na(type_imputation) & !is.na(ncnofish), 1, type_imputation))
mergedStd = mergedStd %>% mutate(ncnofish = if_else(is.na(ncnofish), ncnofish_2, ncnofish), 
                                 ncmtfish = if_else(is.na(ncmtfish), ncmtfish_2, ncmtfish),
                                 type_imputation = if_else(is.na(type_imputation) & !is.na(ncnofish), 2, type_imputation))
mergedStd = mergedStd %>% mutate(ncnofish = if_else(is.na(ncnofish), ncnofish_3, ncnofish), 
                                 ncmtfish = if_else(is.na(ncmtfish), ncmtfish_3, ncmtfish),
                                 type_imputation = if_else(is.na(type_imputation) & !is.na(ncnofish), 3, type_imputation))
mergedStd = mergedStd %>% mutate(ncnofish = if_else(is.na(ncnofish), ncnofish_4, ncnofish), 
                                 ncmtfish = if_else(is.na(ncmtfish), ncmtfish_4, ncmtfish),
                                 type_imputation = if_else(is.na(type_imputation) & !is.na(ncnofish), 4, type_imputation))

# Remove imputation columns:
mergedStd = mergedStd %>% select(-c('ncnofish_1', 'ncnofish_2', 'ncnofish_3', 'ncnofish_4',
                                    'ncmtfish_1', 'ncmtfish_2', 'ncmtfish_3', 'ncmtfish_4'))

# Make sure we dont have NA:
sum(is.na(mergedStd$ncnofish))
sum(is.na(mergedStd$ncmtfish))

# Make imputation plot:
imputationLevel = mergedStd %>% group_by(type_imputation) %>% summarise(n = n()) %>% mutate(perc = (n/sum(n))*100)
sum(imputationLevel$perc)
p1 = ggplot(imputationLevel, aes(x = factor(type_imputation), y = perc)) +
  geom_bar(stat = "identity") +
  xlab('Imputation level') + ylab('% of grid observations')
ggsave(file.path(shrpoint_path, plot_dir, paste0('imputation_grid_', grid_size, img_type)), plot = p1,
       width = img_width*0.5, height = 70, units = 'mm', dpi = img_res)

# Check if imputation methods did not produce outliers:
ggplot(data = mergedStd) + geom_boxplot(aes(x = factor(type_imputation), y = ncnofish)) + facet_wrap(~ fisherycode)
ggplot(data = mergedStd) + geom_boxplot(aes(x = factor(type_imputation), y = ncmtfish)) + facet_wrap(~ fisherycode)

# Get lon lat information:
mergedStd = left_join(mergedStd, stdGridPoint, by = 'grid_ID')

# Save merged data frame:
save(mergedStd, file = file.path(shrpoint_path, data_folder, paste0('mergedStd_', grid_size,'.RData')))
