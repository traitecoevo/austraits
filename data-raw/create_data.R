
## code to prepare an example dataset, that go public

devtools::load_all()

austraits_all <- load_austraits("ignore/data/austraits/")
dataset_id <- c("Falster_2003", "Falster_2005_1", "Falster_2005_2", "Zanne_2009")

austraits <- extract_dataset(austraits_all, dataset_id)

usethis::use_data(austraits, internal = TRUE, overwrite = TRUE)
