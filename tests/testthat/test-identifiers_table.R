austraits_identifiers <- readRDS("benchmarks/austraits_identifiers.rds")

test_that("extract functions work with identifiers table", {
  expect_no_error(austraits_identifiers %>% extract_dataset("Schulze_2014"))
  expect_no_error(austraits_identifiers %>% extract_dataset("Falster_2003"))
  expect_no_error(austraits_identifiers %>% extract_data(table = "identifiers", col = "identifier_type", col_value = "catalogNumber"))
  expect_error(austraits_identifiers %>% extract_data(table = "identifiers", col = "identifier_type", col_value = "museum"))
}
)
  
test_that("bind function works with identifiers table", {
  Schulze_2014 <- austraits_identifiers %>% extract_dataset("Schulze_2014")
  Falster_2003 <- austraits_identifiers %>% extract_dataset("Falster_2003")
  expect_silent(bind_databases(Schulze_2014, Falster_2003))
}
)

test_that("join_ functions work with identifiers table", {
  expect_no_error(austraits_identifiers %>% join_location_coordinates())
  expect_no_error(austraits_identifiers %>% join_identifiers())
  expect_no_error(austraits_identifiers %>% flatten_database())
  expect_equal(austraits_identifiers %>% flatten_database() %>% ncol(), 68)
}
)

test_that("additional functions work with identifiers table", {
  expect_no_error(print.traits.build(austraits_identifiers))
  expect_no_error(check_compatibility(austraits_identifiers))
  expect_no_error(lookup_trait(austraits_identifiers, "leaf"))
}
)
