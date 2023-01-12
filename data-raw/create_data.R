
## code to prepare an example dataset, that go public

devtools::load_all()
set.seed(109)
austraits_all <- load_austraits(version = "3.0.2", path = "ignore/data/austraits")
dataset_id <- c( unique(austraits_all$traits$dataset_id) %>% sample(5), "Falster_2003", "Falster_2005_1", "Falster_2005_2", "Wright_2019")
austraits_lite <- extract_dataset(austraits_all, dataset_id)

# updated release
austraits_newrel <- load_austraits(version = "4.0.0", path = "ignore/data/austraits")

datasets <- c("Crous_2013", "Crous_2019", "Buckton_2019", "Kooyman_2011", "Bloomfield_2018", 
              "Wright_2019", "Westoby_2014", "Vesk_2019", "Leigh_2003", "Prior_2003",
              "Prior_2016", "Choat_2006", "Choat_2012", "ABRS_1981")

austraits_lite_post <- austraits_newrel %>% extract_dataset(dataset_id = datasets)

## code to prepare `australia_map_raster` dataset goes here

australia_map_raster <- raster::raster("ignore/australia.tif") 
australia_map_raster <- australia_map_raster %>% raster::as.data.frame(xy = T,na.rm=T)

usethis::use_data(austraits_lite, australia_map_raster, austraits_lite_post, internal = TRUE, overwrite = TRUE)


