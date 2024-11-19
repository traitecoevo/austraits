Banksia_1 <- austraits_5.0.0_lite %>% extract_taxa(taxon_name = "Banksia serrata")
Banksia_2 <- austraits_5.0.0_lite %>% extract_taxa(taxon_name = "Banksia ericifolia")
fire <- austraits_5.0.0_lite %>% extract_data(table = "contexts", col = "context_property", col_value = "fire")
season <- austraits_5.0.0_lite %>% extract_data(table = "contexts", col = "context_property", col_value = "season")

test_that("Function runs", {
  expect_no_error(bound_Banksia <- bind_databases(Banksia_1, Banksia_2))
  expect_silent(bind_databases(Banksia_1))
  expect_silent(bind_databases(Banksia_1, Banksia_2))
  expect_silent(bind_databases(fire, season))
  test_database_structure(database = bound_Banksia)
}
)

test_that("Function returned expected number of rows", {
  expect_equal(nrow(bind_databases(Banksia_1)$traits), nrow(Banksia_1$traits))
  expect_lt(nrow(bind_databases(fire, season)$traits), nrow(fire$traits)+nrow(season$traits))
  expect_equal(nrow(bind_databases(Banksia_1, Banksia_2)$traits), nrow(Banksia_1$traits)+nrow(Banksia_2$traits))
  expect_lt(nrow(bind_databases(Banksia_1, Banksia_2)$contexts), nrow(Banksia_1$contexts)+nrow(Banksia_2$contexts))
  expect_lt(nrow(bind_databases(Banksia_1, Banksia_2, fire, season)$contexts), nrow(Banksia_1$contexts)+nrow(Banksia_2$contexts)+nrow(fire$contexts)+nrow(season$contexts))
}
)

test_that("Order of tables has not changed", {
  expect_equal(names(bind_databases(Banksia_1, Banksia_2)), names(austraits_5.0.0_lite))
  expect_equal(names(bind_databases(fire, season)), names(austraits_5.0.0_lite))
  expect_equal(names(bind_databases(Banksia_1, fire)), names(austraits_5.0.0_lite))
}
)

test_that("Number of columns has not changed", {
  more_Banksia <- bind_databases(Banksia_1, Banksia_2)
  expect_equal(names(more_Banksia$traits), names(austraits_5.0.0_lite$traits))
  expect_equal(names(more_Banksia$contexts), names(austraits_5.0.0_lite$contexts))
  expect_equal(sort(names(more_Banksia$definitions)), sort(names(austraits_5.0.0_lite$definitions)))
}
)
