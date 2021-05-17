#Pull in data
data(austraits)

#Extract a dataset
subset <- extract_dataset(austraits, "Falster_2003")

test_that("extracted dataset has some structure as austraits build", {
  expect_equal(length(austraits), length(extract_dataset(austraits, "Falster_2003")))
  expect_equal(class(austraits), class(extract_dataset(austraits, "Falster_2003")))
  expect_equal(names(austraits), names(subset))
  expect_named(austraits, names(subset))
})

test_that("extraction of dataset was successful")
