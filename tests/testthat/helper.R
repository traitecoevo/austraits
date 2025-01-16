

test_database_structure <- function(database, taxa = NA, dataset_id = NA, n_row = NA) {
  
  table_names <- c("traits", "locations", "contexts", "methods", "excluded_data", "taxonomic_updates", "taxa", "contributors", 
                   "sources", "definitions", "schema", "metadata", "build_info")
  
  table_names_deintifiers <- c("traits", "locations", "contexts", "methods", "excluded_data", "taxonomic_updates", "taxa", "contributors", 
                   "identifiers", "sources", "definitions", "schema", "metadata", "build_info")
  
  testthat::expect_type(database, "list")
  testthat::expect_equal(class(database), "traits.build")
  
  testthat::expect_equal(names(database), table_names)
  
  testthat::expect_contains(database$traits$taxon_name |> unique(), database$taxa$taxon_name |> unique())
  testthat::expect_contains(database$traits$dataset_id |> unique(), database$methods$dataset_id |> unique())
  testthat::expect_contains(paste(database$traits$dataset_id, database$traits$trait_name) |> unique(), paste(database$methods$dataset_id, database$methods$trait_name) |> unique())
  
  if(!is.na(taxa)) {
    testthat::expect_contains(database$traits$taxon_name |> unique(), taxa |> unique())
  }
  
  if(!is.na(dataset_id)) {
    testthat::expect_contains(database$traits$dataset_id |> unique(), dataset_id |> unique())
  }
  
  if(!is.na(n_row)) {
    testthat::expect_equal(database$traits |> nrow(), n_row)
  }
}
