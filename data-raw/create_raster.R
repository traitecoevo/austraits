#Code to prepare Australia raster file for plotting sites
library(mapdata) # to access worldHires data

# get oz outline
oz <-maps::map("worldHires", "Australia", exact=FALSE,
               xlim=c(110,160),ylim=c(-45,-5),
               boundary=TRUE,
               interior=TRUE, fill=T)

# australia outline sf_object ||  format: sf polygon
australia_sp <- sf::st_as_sf(oz)

# rasterise sf_object to raster || format: st raster
australia_raster <- stars::st_rasterize(australia_sp, nx=500, ny=500) %>% 
  raster::as.data.frame(xy = T,na.rm=T)

usethis::use_data(australia_raster, overwrite = TRUE)

