library(purrr)

austraits_lite_small <-
  austraits_lite %>% 
  extract_dataset(c( "Baker_2019", "Falster_2003"))


austraits_lite_post_small <-
  austraits_lite_post %>% 
  extract_dataset(c( "Baker_2019", "Falster_2003"))

test_that("pivot on subset of data", {
  
  # austraits_lite tests
  expect_silent(
    wide_data <- austraits_lite_small %>% 
      pluck("traits") %>% 
      summarise_trait_means() %>% trait_pivot_wider()
  )

#  expect_silent(
#    wide_data_post <- austraits_lite_post_small %>% 
#      pluck("traits") %>% 
#      summarise_trait_means() %>% trait_pivot_wider()
#  )

  
  expect_type(wide_data, "list")
  expect_named(wide_data)
  vals <- c("dataset_id", "taxon_name", "site_name", "context_name", "observation_id", "fire_response", "regen_strategy", "fire_cued_seeding", "leaf_angle", "leaf_area", "leaf_compoundness", "original_name")
  expect_equal(names(wide_data$value), vals)

  # before and after pivots match"
  
  #Checking if widened data has the same length as variables that we are spreading
  expect_equal(length(wide_data), length(c("value", "unit", "date", "value_type", "replicates"))) 
  #Checking number of columns of widened data matches the number of ID columns + number of traits
  expect_equal(ncol(wide_data$value),  (austraits_lite_small$traits %>% dplyr::select(-c(trait_name, value, unit, date, value_type, replicates)) %>% ncol()) + (unique(austraits_lite_small$traits$trait_name) %>% length()) )
  #Checking the number of columns matches original data after pivoting wide and then back to long again
  expect_equal(ncol(austraits_lite_small$traits), ncol(trait_pivot_longer(wide_data)) )
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

walk(list(austraits_lite,
         austraits_lite_post), 
    test_pivot_errors)
