# A cache folder of the test's own, removed when it ends
local_test_cache <- function(env = parent.frame()) {
  cache <- tempfile("cache")
  withr::local_options(traits.use.cache_dir = cache, .local_envir = env)
  withr::defer(unlink(cache, recursive = TRUE), envir = env)
  cache
}
