## code to prepare `australia_map_raster` dataset goes here


australia_map_raster <- raster::raster("ignore/australia.tif") 
usethis::use_data(australia_map_raster, internal = TRUE, overwrite = TRUE)
