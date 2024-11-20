
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
  veronica %>%  extract_trait("lifespan") -> genus_first
  
  # Extract trait first
  austraits %>% extract_trait("lifespan") -> lifespan
  
  # Extract taxa after
  lifespan %>% extract_taxa(genus = "Veronica") -> trait_first
  
  expect_setequal(trait_first$traits$value, veronica_lifespan$value)
  expect_setequal(trait_first$traits$value, genus_first$traits$value)
  expect_setequal(veronica_lifespan$value, genus_first$traits$value)
})

test_that("Function executes - convert functions", {
  expect_visible(convert_df_to_list(dplyr::starwars))
  expect_visible(convert_list_to_df1(as.list(dplyr::starwars)[2]))
  
  demo_list1 <- list(word1 = "this", word2 = "is", word3 = "an", word4 = "example", word5 = "list")
  demo_list2 <- list(word1 = "and", word2 = "a", word3 = "second", word4 = "list", word5 = "also")
  combined_list <- list(demo_list1, demo_list2)
  
  expect_named(convert_list_to_df2(combined_list))
})


test_that("Function throws error - function not supported", {
  expect_error(
    function_not_supported(austraits_3.0.2_lite)
  )
  expect_error( function_not_supported(austraits_4.2.0_lite))
}
)
