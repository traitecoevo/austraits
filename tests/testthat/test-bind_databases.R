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
  names_minus_identifiers <- function(x) {
    names(x) %>% purrr::discard(~ .x == "identifiers")
  }
  expect_equal(
    names_minus_identifiers(bind_databases(Banksia_1, Banksia_2)), 
    names(austraits_5.0.0_lite))
  expect_equal(names_minus_identifiers(bind_databases(fire, season)), names(austraits_5.0.0_lite))
  expect_equal(names_minus_identifiers(bind_databases(Banksia_1, fire)), names(austraits_5.0.0_lite))
}
)

test_that("Number of columns has not changed", {
  more_Banksia <- bind_databases(Banksia_1, Banksia_2)
  expect_equal(names(more_Banksia$traits), names(austraits_5.0.0_lite$traits))
  expect_equal(names(more_Banksia$contexts), names(austraits_5.0.0_lite$contexts))
  expect_equal(sort(names(more_Banksia$definitions)), sort(names(austraits_5.0.0_lite$definitions)))
}
)

test_that("Binding warns when one key names different references", {
  a <- austraits_5.0.0_lite %>% extract_dataset("Apgaua_2017")
  b <- austraits_5.0.0_lite %>% extract_dataset("Choat_2006")
  # The same reference twice is fine
  expect_no_error(bind_databases(a, a))
  # A different reference under the same key is not
  b$sources <- RefManageR::as.BibEntry(list(
    key = "Apgaua_2017", bibtype = "Article", author = "Someone Else",
    title = "A different paper", journal = "A Journal", year = "2017"))
  expect_warning(bind_databases(a, b), "`Apgaua_2017` \\(Apgaua_2017; Choat_2006\\)")
  # The same reference formatted differently is not
  b$sources <- RefManageR::as.BibEntry(list(
    key = "Apgaua_2017", bibtype = "Article", author = "D. Apgaua",
    title = "Plant Functional Groups Within a Tropical Forest Exhibit Different Wood Functional Anatomy.",
    journal = "Funct. Ecol.", year = "2017"))
  expect_no_warning(bind_databases(a, b))
})

test_that("`extract_data` keeps the references the extracted observations cite", {
  a <- austraits_5.0.0_lite %>% extract_dataset("Apgaua_2017")
  expect_true("Apgaua_2017" %in% names(a$sources))
  expect_false("Choat_2006" %in% names(a$sources))
})
