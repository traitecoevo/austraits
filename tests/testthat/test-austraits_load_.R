# Downloads AusTraits releases from Zenodo unless they are already cached
skip_on_cran()
skip_if_offline()


versions <- c("3.0.2", "4.2.0", "5.0.0")
path = "ignore/data/austraits"

test_get_versions <- function(version, path){
  test_that("get_versions is working", {
    x <- get_versions(path = path, update = TRUE)
    
    expect_visible(x)
    expect_named(x)
    expect_type(x, "list")
    expect_length(x, ncol(x))
  })
}

for (version in versions) test_get_versions(version, path)


test_load_austraits <- function(version, path){
  test_that("load_austraits is working", {
    austraits <- load_austraits(version = version, path = path, update = TRUE)
    
    expect_visible(austraits)
    expect_named(austraits)
    expect_type(austraits, "list")
    expect_s3_class(austraits, "traits.build")
    expect_equal(austraits$build_info$version |> as.character() |> stringr::str_remove("^v"), version)
  })
}

for (version in versions) test_load_austraits(version, path = path)


test_that("load_austraits gives sources the traits_build_sources class", {
  austraits <- load_austraits(version = "5.0.0", path = path, update = FALSE)
  expect_s3_class(austraits$sources, "traits_build_sources")
  key <- unname(names(austraits$sources)[1])
  # `$` looks up a reference by key, as `[[` does
  expect_equal(unname(names(austraits$sources[[key]])), key)
  expect_equal(unname(names(eval(parse(text = paste0("austraits$sources$`", key, "`"))))), key)

  # The same release by doi
  versions_table <- get_versions(path = path, update = FALSE)
  by_doi <- load_austraits(doi = versions_table$doi[versions_table$version == "5.0.0"], path = path, update = FALSE)
  expect_identical(by_doi, austraits)
})

test_that("Right errors are tripped", {
  expect_error(load_austraits("3.0.6", path = path))
  expect_error(load_austraits(doi = "10.5281/zenodo.5112005",
                                   path = path))
  expect_error(load_austraits(path = NULL))
})

