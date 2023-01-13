library(purrr)

austraits <- list(austraits_lite,
                  austraits_lite_post)

# austraits_lite tests
wide_data <- austraits_lite$traits %>% summarise_trait_means() %>% trait_pivot_wider()
test_that("input and output are of expected structure", {
  expect_type(wide_data, "list")
  expect_named(wide_data)
  expect_gt(ncol(wide_data$value), ncol(austraits$traits))
})


test_that("before and after pivots match", {
  #Checking if widened data has the same length as variables that we are spreading
  expect_equal(length(wide_data), length(c("value", "unit", "date", "value_type", "replicates"))) 
  #Checking number of columns of widened data matches the number of ID columns + number of traits
  expect_equal(ncol(wide_data[[1]]),  (austraits$traits %>% dplyr::select(-c(trait_name, value, unit, date, value_type, replicates)) %>% ncol()) + (unique(austraits$traits$trait_name) %>% length()) )
  #Checking the number of columns matches original data after pivoting wide and then back to long again
  expect_equal(ncol(austraits$traits), ncol(trait_pivot_longer(wide_data)) )
})


# austraits_list and austraits_lite_post
test_pivot_errors <- function(austraits){
  test_that("functions should throw error when provided wrong input", {
    expect_error(trait_pivot_wider(austraits), label = "The austraits object is a list. Try austraits$trait")
    expect_error(trait_pivot_wider(austraits$taxa), label = "Relevent column names from trait data frame not found")
    expect_error(trait_pivot_wider(austraits$methods), label = "Relevent column names from trait data frame not found")
    expect_error(trait_pivot_longer(austraits), label = "Cannot gather an already long format dataframe")
  })
}

map(austraits, 
    test_pivot_errors)

test_that("function shouldn't complain and throw errors", {
  expect_visible(austraits_lite$traits %>% summarise_trait_means() %>% trait_pivot_wider())
  # expect_visible(austraits_lite_post$traits %>% trait_pivot_wider())
})

# austraits_lite_post
# test_that("structure after pivoting", {
#   expect_gt(ncol(austraits_lite_post$traits %>% trait_pivot_wider()), ncol(austraits_lite_post$traits))
#   expect_lt(nrow(austraits_lite_post$traits %>% trait_pivot_wider()), nrow(austraits_lite_post$traits))
# })
