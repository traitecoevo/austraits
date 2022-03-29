#Pull in data
austraits <- austraits_lite
wide_data <- austraits$traits %>% summarise_trait_means() %>% trait_pivot_wider()

test_that("function shouldn't complain and throw errors", {
  expect_silent(austraits$traits %>% summarise_trait_means() %>% trait_pivot_wider())
})

test_that("functions should throw error when provided wrong input", {
expect_error(trait_pivot_wider(austraits), label = "The austraits object is a list. Try austraits$trait")
expect_error(trait_pivot_wider(austraits$sites), label = "Relevent column names from trait data frame not found")
expect_error(trait_pivot_wider(austraits$methods), label = "Relevent column names from trait data frame not found")
expect_error(trait_pivot_longer(austraits), label = "Cannot gather an already long format dataframe")
})

test_that("input and output are of expected structure", {
expect_match(class(wide_data), "list")
expect_equal(class(wide_data), "list")
expect_equal(wide_data %>% length, 5)
})

test_that("before and after pivots match", {
  #Checking if widened data has the same length as variables that we are spreading
  expect_equal(length(wide_data), length(c("value", "unit", "date", "value_type", "replicates"))) 
  #Checking number of columns of widened data matches the number of ID columns + number of traits
  expect_equal(ncol(wide_data[[1]]),  (austraits$traits %>% dplyr::select(-c(trait_name, value, unit, date, value_type, replicates)) %>% ncol()) + (unique(austraits$traits$trait_name) %>% length()) )
  #Checking the number of columns matches original data after pivoting wide and then back to long again
  expect_equal(ncol(austraits$traits), ncol(trait_pivot_longer(wide_data)) )
})
