#Pull in data
data_versions <- list()

#Load lite
data_versions[["lite"]] <- load_austraits()

#Load test
filenames <- list.files("ignore/")
path <- paste0("ignore/", filenames)

if(file.exists(path)) {
  data_versions[[stringr::str_extract(filenames, "[[:digit:]]\\.[[:digit:]]\\.[[:digit:]]")]] <- load_austraits(path)
}

for(v in names(data_versions)) {
  austraits <- data_versions[[v]]
  
  test_that("Function doesn't throw error", {
    expect_invisible(austraits %>% plot_trait_distribution_beeswarm("wood_density", "dataset_id", "Westoby_2014"))
  }) 
  
}
