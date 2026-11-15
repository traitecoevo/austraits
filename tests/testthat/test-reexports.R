test_that("austraits re-exports every traits.use function except plot_locations()", {
  from_traits_use <- setdiff(getNamespaceExports("traits.use"), "plot_locations")
  expect_true(all(from_traits_use %in% getNamespaceExports("austraits")))

  for (f in from_traits_use) {
    expect_identical(
      getExportedValue("austraits", f), getExportedValue("traits.use", f),
      info = f
    )
  }

  expect_identical(austraits::extract_trait, traits.use::extract_trait)

  # austraits' own plot_locations() wraps the traits.use one
  expect_false(identical(austraits::plot_locations, traits.use::plot_locations))
})
