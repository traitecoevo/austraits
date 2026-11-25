# Lists AusTraits' releases on Zenodo and downloads AusTraits 5.0.0 (17 MB),
# the oldest release austraits supports, into ignore/data/austraits, unless
# it is cached there already. Everything else about loading releases is
# tested offline, in test-load_austraits_offline.R and
# test-get_versions_offline.R.
skip_on_cran()
skip_if_offline()

path <- "ignore/data/austraits"

test_that("get_versions() lists AusTraits' releases on Zenodo", {
  x <- get_versions(path = path, update = TRUE)

  expect_s3_class(x, "tbl_df")
  expect_named(x, c("publication_date", "doi", "version", "id"))
  expect_true(all(c("7.0.0", "6.0.0", "5.0.0") %in% x$version))
  expect_equal(x$doi[x$version == "5.0.0"], "10.5281/zenodo.10156222")
  # newest first
  expect_false(is.unsorted(rev(x$publication_date)))
  expect_true(file.exists(file.path(path, "austraits_versions.json")))

  # update = FALSE reads the list just saved
  expect_identical(get_versions(path = path, update = FALSE), x)
  expect_identical(get_version_latest(path = path, update = FALSE), x$version[1])
})

test_that("load_austraits() loads AusTraits 5.0.0, by version or by doi", {
  austraits <- load_austraits(version = "5.0.0", path = path, update = TRUE)

  expect_s3_class(austraits, "traits.build")
  expect_equal(as.character(austraits$build_info$version), "5.0.0")
  expect_true(all(c("traits", "locations", "contexts", "methods", "taxa", "sources") %in% names(austraits)))

  # 5.0.0 predates the traits_build_sources class, which load_austraits()
  # gives it; `$` looks up a reference by key, as `[[` does
  expect_s3_class(austraits$sources, "traits_build_sources")
  key <- unname(names(austraits$sources)[1])
  expect_equal(unname(names(austraits$sources[[key]])), key)
  expect_equal(unname(names(eval(parse(text = paste0("austraits$sources$`", key, "`"))))), key)

  # The same release by doi, and by version with a "v"
  expect_identical(load_austraits(doi = "10.5281/zenodo.10156222", path = path, update = FALSE), austraits)
  expect_identical(load_austraits(version = "v5.0.0", path = path, update = FALSE), austraits)
})

test_that("load_austraits() rejects a version or doi that is not a release", {
  expect_error(load_austraits(version = "3.0.6", path = path, update = FALSE), "Requested version/doi is incorrect")
  # A Zenodo record that is not a release of AusTraits
  expect_error(load_austraits(doi = "10.5281/zenodo.5112005", path = path, update = FALSE), "Requested version/doi is incorrect")
})
