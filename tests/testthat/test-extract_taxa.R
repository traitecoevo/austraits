library(stringr)

austraits <- austraits_lite

test_that("Error triggered", {
  expect_error(austraits %>% extract_taxa())
})

test_that("Function runs", {
  expect_visible(austraits %>% extract_taxa(family = "Rubiaceae"))
  expect_visible(austraits %>% extract_taxa(genus = "Melaleuca"))
})


test_that("Output is correct", {
  expect_type(austraits %>% extract_taxa(family = "Rubiaceae"), "list")
  
  genus = "Melaleuca"
  test_aca <- austraits %>% extract_taxa(genus = genus)
  expect_equal(test_aca$taxa$genus %>% unique(), genus)
  expect_equal(word(test_aca$taxa$taxon_name, 1)[1], genus)
  expect_equal(word(test_aca$traits$taxon_name, 1)[1], genus)
  
  family = "Rubiaceae"
  test_prot <- austraits %>% extract_taxa(family = family)
  expect_equal(test_prot$taxa$family %>% unique(), family)
  })


test_that("Dataframe is extracted correctly", {
  austraits <- load_austraits(version = "3.0.2", path = "ignore/data/austraits")
  
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
