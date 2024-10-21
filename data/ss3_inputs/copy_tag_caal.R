# Copy tag and caal inputs (in Github repository) and paste them in working folder
source('sharepoint_path.R')
file.copy(list.files('data/ss3_inputs/4A_io', full.names = TRUE), 
          file.path(shrpoint_path, 'data/ss3_inputs/4A_io'))
file.copy(list.files('data/ss3_inputs/2A_io/agg', full.names = TRUE), 
          file.path(shrpoint_path, 'data/ss3_inputs/2A_io/agg'))
file.copy(list.files('data/ss3_inputs/2A_io/aaf', full.names = TRUE), 
          file.path(shrpoint_path, 'data/ss3_inputs/2A_io/aaf'))
file.copy(list.files('data/ss3_inputs/1A_io/agg', full.names = TRUE), 
          file.path(shrpoint_path, 'data/ss3_inputs/1A_io/agg'))
file.copy(list.files('data/ss3_inputs/1A_io/aaf', full.names = TRUE), 
          file.path(shrpoint_path, 'data/ss3_inputs/1A_io/aaf'))