#Pull in data
data(austraits)
#Filter some data
data <- dplyr::filter(austraits$traits, dataset_id == "Falster_2003")
wide_data <- trait_pivot_wider(data)

test_that("function shouldn't complain and throw errors", {
  expect_silent(trait_pivot_wider(data))
  #This test is producing messages in R CMD check but not when tested interactively
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
  #Checking if widened data has the same length as variables that we are spreading
  expect_equal(length(wide_data), length(c("value", "unit", "date", "value_type", "replicates"))) 
  #Checking number of columns of widened data matches the number of ID columns + number of traits
  expect_equal(ncol(trait_pivot_wider(data)[[1]]),  (ncol(data %>% dplyr::select(-c(trait_name, value))) + (unique(data$trait_name) %>% length())) )
  #Checking number of rows in long format is a multiple of each wide formatted dataframe
  expect_equal(nrow(data), (data %>% dplyr::filter(trait_name == "leaf_area") %>% nrow()) * (unique(data$trait_name) %>% length()))
  #Checking the number of columns matches original data after pivoting wide and then back to long again
  expect_equal(ncol(data), ncol(trait_pivot_longer(wide_data)) )
})
