

test_database_structure <- function(database, taxa = NA, dataset_id = NA, n_row = NA) {
  
  table_names <- c("traits", "locations", "contexts", "methods", "excluded_data", "taxonomic_updates", "taxa", "contributors", 
                   "sources", "definitions", "schema", "metadata", "build_info")
  
  expect_type(database, "list")
  # should this be "traits.build or austraits??
  #expect_equal(class(database), "austraits")
  expect_equal(class(database), "traits.build")
  
  expect_equal(names(database), table_names)
  
  expect_contains(database$traits$taxon_name |> unique(), database$taxa$taxon_name |> unique())
  expect_contains(database$traits$dataset_id |> unique(), database$methods$dataset_id |> unique())
  expect_contains(paste(database$traits$dataset_id, database$traits$trait_name) |> unique(), paste(database$methods$dataset_id, database$methods$trait_name) |> unique())
  
  if(!is.na(taxa)) {
    expect_contains(database$traits$taxon_name |> unique(), taxa |> unique())
  }
  
  if(!is.na(dataset_id)) {
    expect_contains(database$traits$dataset_id |> unique(), dataset_id |> unique())
  }
  
  if(!is.na(n_row)) {
    expect_equal(database$traits |> nrow(), n_row)
  }
}
