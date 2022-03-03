
## code to prepare an example dataset, that go public

devtools::load_all()

austraits_all <- load_austraits(version = "3.0.2", path = "ignore/data/austraits")
dataset_id <- c("Falster_2003", "Falster_2005_1", "Falster_2005_2", "Zanne_2009")

austraits_lite <- extract_dataset(austraits_all, dataset_id)

## code to prepare `australia_map_raster` dataset goes here

australia_map_raster <- raster::raster("ignore/australia.tif") 
australia_map_raster <- australia_map_raster %>% raster::as.data.frame(xy = T,na.rm=T)

usethis::use_data(austraits_lite, australia_map_raster, internal = TRUE, overwrite = TRUE)


