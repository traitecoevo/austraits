library(stringr)

austraits <- austraits_lite

test_that("Error triggered", {
  expect_error(austraits %>% extract_taxa(family = "Proteaceae", genus = "Acacia"))
  expect_error(austraits %>% extract_taxa())
})

test_that("Function runs", {
  expect_visible(austraits %>% extract_taxa(family = "Proteaceae"))
  expect_visible(austraits %>% extract_taxa(genus = "Acacia"))
})


test_that("Output is correct", {
  expect_type(austraits %>% extract_taxa(family = "Proteaceae"), "list")
  
  genus = "Acacia"
  test_aca <- austraits %>% extract_taxa(genus = genus)
  expect_equal(test_aca$taxa$genus %>% unique(), genus)
  expect_equal(word(test_aca$taxa$taxon_name, 1)[1], genus)
  expect_equal(word(test_aca$traits$taxon_name, 1)[1], genus)
  
  family = "Proteaceae"
  test_prot <- austraits %>% extract_taxa(family = family)
  expect_equal(test_prot$taxa$family %>% unique(), family)
  })
