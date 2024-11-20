not_supported_austraits <- list(austraits_3.0.2_lite, austraits_4.2.0_lite) 

inputs <- list(austraits_5.0.0_lite, austraits_5.0.0_lite$traits)

test_pivot_success <- function(austraits){
  test_that("pivot on subset of data", {
    
    expect_silent(
      wide_data <- austraits %>% trait_pivot_wider()
    )
    
    expect_type(wide_data, "list")
    expect_named(wide_data)
  })
}

purrr::walk(inputs,
            test_pivot_success)


test_that("Widen structure is expected", {
  wide_data <- austraits_5.0.0_lite %>% trait_pivot_wider()
  names(wide_data)
  #Checking if widened data has the same length as variables that we are spreading
  expect_gt(ncol(wide_data), ncol(austraits_5.0.0_lite$traits)) 
  expect_lt(nrow(wide_data), nrow(austraits_5.0.0_lite$traits)) 
  expect_true(any(colnames(wide_data) %in% unique(austraits_5.0.0_lite$traits$trait_name)))
}
)
# austraits_list and austraits_lite_post
test_pivot_errors <- function(austraits){
  test_that("functions should throw error when provided wrong input", {
    expect_error(trait_pivot_wider(austraits), label = "The austraits object is a list. Try austraits$trait")
    expect_error(trait_pivot_wider(austraits$taxa), label = "Relevent column names from trait data frame not found")
    expect_error(trait_pivot_wider(austraits$methods), label = "Relevent column names from trait data frame not found")
    expect_error(trait_pivot_longer(austraits), label = "Cannot gather an already long format dataframe")
  })
}

purrr::walk(not_supported_austraits, 
    test_pivot_errors)
