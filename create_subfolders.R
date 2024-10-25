# Create subfolders in the working folder specified in sharepoint_path.R
source('sharepoint_path.R')

# Data folders
dir.create(file.path(shrpoint_path, "data", "raw"), showWarnings = FALSE, recursive = TRUE) 
dir.create(file.path(shrpoint_path, "data", "processed"), showWarnings = FALSE, recursive = TRUE) 
dir.create(file.path(shrpoint_path, "data", "ss3_inputs", "4A_io"), showWarnings = FALSE, recursive = TRUE) 
dir.create(file.path(shrpoint_path, "data", "ss3_inputs", "2A_io", "agg"), showWarnings = FALSE, recursive = TRUE) 
dir.create(file.path(shrpoint_path, "data", "ss3_inputs", "2A_io", "aaf"), showWarnings = FALSE, recursive = TRUE) 
dir.create(file.path(shrpoint_path, "data", "ss3_inputs", "1A_io", "agg"), showWarnings = FALSE, recursive = TRUE) 
dir.create(file.path(shrpoint_path, "data", "ss3_inputs", "1A_io", "aaf"), showWarnings = FALSE, recursive = TRUE) 

# Model folders
dir.create(file.path(shrpoint_path, "models", "base", "4A_io"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(shrpoint_path, "models", "base", "2A_io"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(shrpoint_path, "models", "base", "1A_io"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(shrpoint_path, "models", "configurations", "4A_io", "sensitivity"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(shrpoint_path, "models", "configurations", "2A_io", "agg"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(shrpoint_path, "models", "configurations", "2A_io", "aaf"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(shrpoint_path, "models", "configurations", "1A_io", "agg"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(shrpoint_path, "models", "configurations", "1A_io", "aaf"), showWarnings = FALSE, recursive = TRUE)
