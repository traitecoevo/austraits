
# Miscellaneous bugs that have been raised
# Want this file to come after loading so that dataset is available

test_that("Dataframe is extracted correctly", {
  austraits <- load_austraits(version = "5.0.0", path = "ignore/data/austraits")
  
  # Extract Veronica first
  veronica <- extract_taxa(austraits, genus = "Veronica")
  
  # Filter to lifespan traits using dplyr
  veronica_lifespan <- veronica$traits %>%
    dplyr::filter(trait_name == "lifespan")
  
  # Extract trait after
  veronica |>  extract_trait("lifespan") -> genus_first
  
  # Extract trait first
  austraits |> extract_trait("lifespan") -> lifespan
  
  # Extract taxa after
  lifespan |> extract_taxa(genus = "Veronica") -> trait_first
  
  expect_setequal(trait_first$traits$value, veronica_lifespan$value)
  expect_setequal(trait_first$traits$value, genus_first$traits$value)
  expect_setequal(veronica_lifespan$value, genus_first$traits$value)
})
