
## code to prepare `australia_map_raster`, the map of Australia that
## plot_locations() draws by default (internal data)
australia_map_raster <- raster::raster("ignore/australia.tif")
australia_map_raster <- australia_map_raster %>% raster::as.data.frame(xy = T,na.rm=T)

usethis::use_data(australia_map_raster, internal = TRUE, overwrite = TRUE)
