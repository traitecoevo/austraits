not_supported_austraits <- list(austraits_3.0.2_lite,
                                austraits_4.2.0_lite)

test_join_error <- function(austraits){
  test_that("old versions will complain", {
    expect_error(join_location_coordinates(austraits))
    expect_error(join_methods(austraits))
    expect_error(join_context_properties(austraits))
    expect_error(join_taxa(austraits))
    expect_error(join_taxonomic_updates(austraits))
  })
}

purrr::walk(not_supported_austraits,
            test_join_error)

test_that("functions should work without warnings", {
  expect_silent(join_location_coordinates(austraits_5.0.0_lite))
  expect_silent(join_methods(austraits_5.0.0_lite))
  expect_silent(join_context_properties(austraits_5.0.0_lite))
  expect_silent(join_taxa(austraits_5.0.0_lite))
  expect_silent(join_taxonomic_updates(austraits_5.0.0_lite))
})

test_that("structure doesn't change", {
  expect_type(join_location_coordinates(austraits_5.0.0_lite), "list")
  expect_type(join_methods(austraits_5.0.0_lite), "list")
  expect_type(join_context_properties(austraits_5.0.0_lite), "list")
  expect_type(join_taxa(austraits_5.0.0_lite), "list")
  expect_type(join_taxonomic_updates(austraits_5.0.0_lite), "list")
  expect_no_error(database_with_coordinates <- join_location_coordinates(austraits_5.0.0_lite))
  test_database_structure(database = database_with_coordinates)
  expect_no_error(database_with_methods <- join_methods(austraits_5.0.0_lite))
  test_database_structure(database = database_with_methods)
  expect_no_error(database_with_contexts <- join_context_properties(austraits_5.0.0_lite))
  test_database_structure(database = database_with_contexts)
})

test_that("variables are added", {
  expect_true(ncol(join_location_coordinates(austraits_5.0.0_lite)$traits) > ncol(austraits_5.0.0_lite$traits))
  expect_true(ncol(join_methods(austraits_5.0.0_lite)$traits) > ncol(austraits_5.0.0_lite$traits))
  expect_true(ncol(join_context_properties(austraits_5.0.0_lite)$traits) > ncol(austraits_5.0.0_lite$traits)) #Need an example where I have context information to add
  expect_true(ncol(join_taxa(austraits_5.0.0_lite)$traits) > ncol(austraits_5.0.0_lite$traits))
  expect_true(ncol(join_taxonomic_updates(austraits_5.0.0_lite)$traits) > ncol(austraits_5.0.0_lite$traits))
  
  expect_true(any(names(join_location_coordinates(austraits_5.0.0_lite)$traits) %in% c("latitude (deg)","longitude (deg)")))
  expect_true(any(names(join_taxa(austraits_5.0.0_lite)$traits) %in%c("family", "genus")))
  expect_true(any(names(join_methods(austraits_5.0.0_lite)$traits) %in%c("methods")))
})

test_that("fails appropriately", {
  expect_error(join_context_properties(austraits_5.0.0_lite, format = "bad"))
  expect_error(join_location_coordinates(austraits_5.0.0_lite, format = "bad"))
  expect_error(join_location_properties(austraits_5.0.0_lite, format = "bad"))
  expect_error(join_taxa(austraits_5.0.0_lite, format = "bad"))
  expect_error(join_contributors(austraits_5.0.0_lite, format = "bad"))
  
  })
test_that("`join_sources` adds each record's references without adding rows", {
  joined <- join_sources(austraits_5.0.0_lite)
  expect_equal(nrow(joined$traits), nrow(austraits_5.0.0_lite$traits))
  # (this release predates `source_primary_type`, so has no `dataset_id_type` to join)
  expect_true(all(c("dataset_id_citation", "source_id_citation", "secondary_id", "secondary_citation") %in% names(joined$traits)))

  # Read from the old methods columns of a database built before traits.build 2.3
  expected <- austraits_5.0.0_lite$methods %>%
    dplyr::distinct(dataset_id, source_primary_citation)
  got <- joined$traits %>% dplyr::distinct(dataset_id, dataset_id_citation)
  expect_equal(
    got$dataset_id_citation[match(expected$dataset_id, got$dataset_id)],
    expected$source_primary_citation
  )

  # A record without a `source_id` cites the dataset's own reference
  i <- is.na(joined$traits$source_id) & joined$traits$dataset_id == "Apgaua_2017"
  expect_true(any(i))
  expect_equal(
    unique(joined$traits$source_id_citation[i]),
    format_citation(austraits_5.0.0_lite$sources[[which(names(austraits_5.0.0_lite$sources) == "Apgaua_2017")]])
  )

  # A key with no entry in `sources` gets NA
  db <- austraits_5.0.0_lite
  db$traits$source_id[1] <- "Nobody_1900"
  expect_true(is.na(join_sources(db, vars = "source_id_citation")$traits$source_id_citation[1]))

  # `vars` selects the columns
  joined <- join_sources(austraits_5.0.0_lite, vars = "secondary_id")
  expect_equal(setdiff(names(joined$traits), names(austraits_5.0.0_lite$traits)), "secondary_id")
})

test_that("`flatten_database` takes references from `join_sources`, once", {
  flat <- flatten_database(austraits_5.0.0_lite)
  expect_true(all(c("dataset_id_citation", "source_id_citation") %in% names(flat)))
  expect_false(any(c("source_primary_citation", "source_secondary_key") %in% names(flat)))
  expect_false(any(grepl("\\.(x|y)$", names(flat))))
})
