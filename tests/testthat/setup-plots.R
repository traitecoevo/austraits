# Draw every plot made during the tests to a null device, so running the
# tests from an interactive session doesn't open plot windows. Setup files
# run only for test runs, not when load_all() sources helpers.
grDevices::pdf(NULL)
withr::defer(grDevices::dev.off(), testthat::teardown_env())
