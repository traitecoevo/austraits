#Pull in data
data(austraits)
#Filter some data
data <- dplyr::filter(austraits$traits, dataset_id == "Falster_2003")
wide_data <- austraits::trait_pivot_wider(data)

test_that("function shouldn't complain and throw errors", {
  expect_silent(trait_pivot_wider(data))
  #This test is producing messages in checks but not when tested interactively
  #expect_silent(trait_pivot_longer(wide_data)) 
})

test_that("functions should throw error when provided wrong input", {
expect_error(trait_pivot_wider(austraits), label = "The austraits object is a list. Try austraits$trait")
expect_error(trait_pivot_wider(austraits$sites), label = "Relevent column names from trait data frame not found")
expect_error(trait_pivot_wider(austraits$methods), label = "Relevent column names from trait data frame not found")
expect_error(trait_pivot_longer(austraits), label = "Cannot gather an already long format dataframe")
})

test_that("input and output are of expected structure", {
expect_match(class(trait_pivot_wider(data)), "list")
expect_equal(class(trait_pivot_longer(wide_data)), c("tbl_df","tbl","data.frame"))
expect_equal(trait_pivot_wider(data) %>% length, 5)
})

test_that("before and after pivots match", {
  # dim(data)
  # dim(trait_pivot_longer(wide_data))
  # names(data) 
  # names(trait_pivot_wider(data)[[1]])
})

