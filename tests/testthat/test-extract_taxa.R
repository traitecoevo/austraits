library(stringr)

austraits <- austraits_lite

test_that("Error triggered", {
  expect_error(austraits %>% extract_taxa(family = "Rubiaceae", genus = "Macadamia"))
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
