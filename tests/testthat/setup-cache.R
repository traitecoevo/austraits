# Keep every test's downloads out of the user's own cache folder. Tests that
# load releases also call local_test_cache() (helper-cache.R), so that what
# one test saves (the same fake dois, other files) never reaches another.
local({
  cache <- tempfile("traits.use-cache")
  withr::local_options(traits.use.cache_dir = cache, .local_envir = testthat::teardown_env())
  withr::defer(unlink(cache, recursive = TRUE), envir = testthat::teardown_env())
})
