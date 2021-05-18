#Pull in data
data(austraits)

#Extract a dataset
dataset_id <- c("Falster_2003")
subset <- extract_dataset(austraits, dataset_id = dataset_id)

test_that("extracted dataset has some structure as austraits build", {
  expect_equal(length(austraits), length(extract_dataset(austraits, "Falster_2003")))
  expect_equal(class(austraits), class(extract_dataset(austraits, "Falster_2003")))
  expect_equal(names(austraits), names(subset))
  expect_named(austraits, names(subset))
})

test_that("extraction of dataset was successful", {
  expect_match(dataset_id, unique(subset$traits$dataset_id))
  expect_equal(1, dplyr::n_distinct(subset$traits$dataset_id))x
})
