library(purrr)

versions <- c("3.0.2", "4.0.0")
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

walk(versions,
    ~ test_get_versions(.x, path))


test_load_austraits <- function(version, path){
  test_that("load_austraits is working", {
    austraits <- load_austraits(version = version, path = path, update = TRUE)
    
    expect_visible(austraits)
    expect_named(austraits)
    expect_type(austraits, "list")
    expect_error(load_austrait())
  })
}

map(versions,
    ~ test_load_austraits(.x, path = path))


test_that("Right errors are tripped", {
  expect_error(load_austraits("3.0.6", path = path))
  expect_error(load_austraits(doi = "10.5281/zenodo.5112005",
                                   path = path))
  expect_error(load_austraits(path = NULL))
})

