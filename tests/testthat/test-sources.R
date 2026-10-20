# `austraits_5.0.0_lite` was built before the class existed, so its sources
# are a plain BibEntry: the "before" against which the class is compared
plain <- austraits_5.0.0_lite$sources
sources <- as_traits_build_sources(plain)
key <- "Falster_2003"
some_keys <- c("Falster_2003", "Falster_2005_1", "Crous_2013")

test_that("the fixture is a plain BibEntry with the keys used below", {
  expect_identical(class(plain), c("BibEntry", "bibentry"))
  expect_contains(names(plain), some_keys)
  expect_identical(
    class(sources),
    c("traits_build_sources", "BibEntry", "bibentry")
  )
})

test_that("as_traits_build_sources() classifies only a BibEntry, once", {
  expect_identical(as_traits_build_sources(sources), sources)
  expect_null(as_traits_build_sources(NULL))
  expect_identical(as_traits_build_sources(list()), list())
  expect_identical(as_traits_build_sources(1:3), 1:3)
})

test_that("`$` with a key returns that entry", {
  # The bug in #185: on a plain BibEntry, `$` is field access, so a key
  # returns one NULL per entry, named by every key
  expect_length(plain$Falster_2003, length(plain))
  expect_true(all(vapply(plain$Falster_2003, is.null, logical(1))))

  entry <- sources$Falster_2003
  expect_s3_class(entry, "traits_build_sources")
  expect_length(entry, 1)
  expect_identical(names(entry), key)
  expect_identical(entry, sources[[key]])
  expect_identical(unclass(entry), unclass(plain[[key]]))
  expect_identical(sources$Falster_2005_1, sources[["Falster_2005_1"]])
})

test_that("`$` with any other name is still field access", {
  for (field in c("title", "author", "year", "doi", "journal", "key",
                  "bibtype", "Not_a_key_or_field")) {
    expect_identical(
      eval(call("$", sources, field)),
      eval(call("$", plain, field)),
      info = field
    )
  }
  # ... including on a single entry returned by `$`
  expect_identical(sources$Falster_2003$title, plain[[key]]$title)
  expect_identical(sources$Falster_2003$author, plain[[key]]$author)
  # names() is RefManageR's `x$key`, so it must still see the field
  expect_identical(names(sources), names(plain))
})

test_that("`[` and `[[` return the same entries and keep the class", {
  indices <- list(some_keys, 2:4, names(plain) %in% some_keys)
  for (i in indices) {
    expect_s3_class(sources[i], "traits_build_sources")
    expect_identical(unclass(sources[i]), unclass(plain[i]))
  }
  expect_identical(unname(names(sources[some_keys])), some_keys)

  # RefManageR's search syntax calls `[` recursively to find indices. Pin the
  # search options it reads (these are its defaults) and restore them after.
  # No year searches: RefManageR matches years with lubridate::year() on a
  # `dateobj` stored without a time zone, and this fixture's dates are local
  # midnight in Sydney, so `year = "2005"` finds nothing in UTC (as on CI)
  old_options <- RefManageR::BibOptions(
    match.author = "family.name", use.regex = TRUE, ignore.case = TRUE
  )
  on.exit(RefManageR::BibOptions(old_options), add = TRUE)

  falster <- c("Falster_2003", "Falster_2005_1", "Falster_2005_2")
  searches <- list(
    or = sources[list(author = "Falster"), list(journal = "Phytologist")],
    author = sources[author = "Falster"],
    and = sources[author = "Falster", journal = "Ecology"]
  )
  expected <- list(
    or = plain[list(author = "Falster"), list(journal = "Phytologist")],
    author = plain[author = "Falster"],
    and = plain[author = "Falster", journal = "Ecology"]
  )
  # Check what each search matched first, so a search that matches nothing
  # fails here rather than as a missing class
  expect_identical(unname(names(searches$author)), falster)
  expect_identical(unname(names(searches$and)), "Falster_2005_1")
  expect_contains(unname(names(searches$or)), falster)
  expect_gt(length(searches$or), length(falster))
  for (j in names(searches)) {
    expect_gt(length(searches[[j]]), 0)
    expect_s3_class(searches[[j]], "traits_build_sources")
    expect_identical(unclass(searches[[j]]), unclass(expected[[j]]), info = j)
  }

  expect_message(none <- sources["Not_a_key"], "No results")
  expect_identical(none, list())

  expect_s3_class(sources[[key]], "traits_build_sources")
  expect_s3_class(sources[[1]], "traits_build_sources")
  expect_identical(unclass(sources[[2]]), unclass(plain[[2]]))
})

test_that("`c()` keeps the class when the first argument has it", {
  combined <- c(sources[some_keys[1:2]], sources[some_keys[3]])
  expect_s3_class(combined, "traits_build_sources")
  expect_identical(unname(names(combined)), some_keys)
  expect_identical(
    unclass(combined),
    unclass(c(plain[some_keys[1:2]], plain[some_keys[3]]))
  )

  expect_s3_class(c(sources[1:2], plain[3:4]), "traits_build_sources")
  expect_s3_class(
    purrr::reduce(list(sources[1:2], plain[3:4], sources[5]), c),
    "traits_build_sources"
  )
  # Dispatch is on the first argument, so a plain one first gives a plain
  # BibEntry; bind_databases() re-applies the class for this reason
  expect_false(inherits(c(plain[1:2], sources[3:4]), "traits_build_sources"))
})

test_that("printing and formatting are unchanged", {
  expect_identical(capture.output(print(sources)), capture.output(print(plain)))
  expect_identical(
    capture.output(print(sources$Falster_2003)),
    capture.output(print(plain[key]))
  )
  for (style in c("text", "Bibtex", "Biblatex", "citation", "markdown", "R")) {
    expect_identical(
      capture.output(print(sources, .opts = list(style = style))),
      capture.output(print(plain, .opts = list(style = style))),
      info = style
    )
  }
  expect_identical(format(sources), format(plain))
  expect_identical(toBiblatex(sources), toBiblatex(plain))
  expect_identical(as.data.frame(sources), as.data.frame(plain))
})

test_that("WriteBib() writes the same file with or without the class", {
  write_bib <- function(bib) {
    path <- tempfile(fileext = ".bib")
    on.exit(unlink(path))
    suppressMessages(RefManageR::WriteBib(bib, path, verbose = FALSE))
    readBin(path, "raw", file.size(path))
  }
  with_class <- write_bib(sources)
  without_class <- write_bib(plain)
  expect_gt(length(with_class), 0)
  expect_identical(with_class, without_class)
})

test_that("saveRDS() / readRDS() round-trip keeps the class and lookup", {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(sources, path)
  restored <- readRDS(path)
  expect_identical(restored, sources)
  expect_identical(restored$Falster_2003, sources[[key]])
})

test_that("the class is a plain BibEntry where austraits is not loaded", {
  # Consumers of a published .rds (austraits-api, the portal) read it without
  # austraits, so S3 dispatch must fall through to RefManageR's methods,
  # exactly as for an object built before the class existed
  skip_on_cran()
  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  skip_if_not(file.exists(rscript))

  dir <- tempfile("sources")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  saveRDS(sources, file.path(dir, "with.rds"))
  saveRDS(plain, file.path(dir, "without.rds"))
  writeLines(
    c(
      sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")),
      sprintf("dir <- %s", deparse(dir)),
      "with <- readRDS(file.path(dir, 'with.rds'))",
      "without <- readRDS(file.path(dir, 'without.rds'))",
      "same <- function(f) identical(",
      "  unclass(tryCatch(f(with), error = conditionMessage)),",
      "  unclass(tryCatch(f(without), error = conditionMessage)))",
      "checks <- function() c(",
      "  key = same(function(x) x$Falster_2003),",
      "  field = same(function(x) x$title),",
      "  double_bracket = same(function(x) x[['Falster_2003']]),",
      "  single_bracket = same(function(x) x['Falster_2003']),",
      "  format = same(function(x) format(x)),",
      "  print = same(function(x) capture.output(print(x))))",
      "before <- checks()",
      "loadNamespace('RefManageR')",
      "after <- checks()",
      "write <- function(x, f) {",
      "  suppressMessages(RefManageR::WriteBib(x, f, verbose = FALSE))",
      "  readBin(f, 'raw', file.size(f))}",
      "after['WriteBib'] <- identical(",
      "  write(with, file.path(dir, 'with.bib')),",
      "  write(without, file.path(dir, 'without.bib')))",
      "saveRDS(list(before = before, after = after,",
      "  austraits_loaded = isNamespaceLoaded('austraits')),",
      "  file.path(dir, 'result.rds'))"
    ),
    file.path(dir, "check.R")
  )
  status <- system2(
    rscript, c("--vanilla", shQuote(file.path(dir, "check.R"))),
    stdout = FALSE, stderr = FALSE
  )
  expect_identical(status, 0L)
  result <- readRDS(file.path(dir, "result.rds"))

  expect_false(result$austraits_loaded)
  # Without RefManageR loaded, utils' bibentry methods apply to both alike
  failing <- function(x) toString(names(x)[!x])
  expect_true(all(result$before), info = failing(result$before))
  # With it loaded, `$` with a key is field access again, as before #185
  expect_true(all(result$after), info = failing(result$after))
})

test_that("bind_databases() gives the combined sources the class", {
  banksia_1 <- austraits_5.0.0_lite %>%
    extract_taxa(taxon_name = "Banksia serrata")
  banksia_2 <- austraits_5.0.0_lite %>%
    extract_taxa(taxon_name = "Banksia ericifolia")

  # As built by traits.build: per-dataset sources without the class
  unclassify <- function(database) {
    class(database$sources) <- c("BibEntry", "bibentry")
    database
  }
  for (databases in list(
    list(banksia_1, banksia_2),
    list(unclassify(banksia_1), unclassify(banksia_2)),
    list(unclassify(banksia_1), banksia_2),
    list(unclassify(banksia_1))
  )) {
    bound <- bind_databases(databases = databases)
    expect_s3_class(bound$sources, "traits_build_sources")
    keys <- databases %>%
      lapply(function(x) names(x$sources)) %>%
      unlist() %>%
      unique() %>%
      sort()
    expect_identical(unname(names(bound$sources)), keys)
    for (k in keys) {
      expect_identical(eval(call("$", bound$sources, k)), bound$sources[[k]])
      expect_length(eval(call("$", bound$sources, k)), 1)
    }
  }
})

test_that("extract_*() keep the class, and add it to databases without it", {
  # austraits_5.0.0_lite lacks the class, as does any database read with
  # readRDS() rather than load_austraits()
  extracted <- list(
    extract_dataset(austraits_5.0.0_lite, "Falster_2003"),
    extract_data(austraits_5.0.0_lite, "traits", "dataset_id", "Falster_2003"),
    extract_trait(austraits_5.0.0_lite, "leaf_area"),
    extract_taxa(austraits_5.0.0_lite, genus = "Banksia")
  )
  database <- classify_sources(austraits_5.0.0_lite)
  expect_s3_class(database$sources, "traits_build_sources")
  extracted <- c(extracted, list(
    extract_dataset(database, "Falster_2003"),
    extract_trait(database, "leaf_area")
  ))
  for (x in extracted) {
    expect_s3_class(x, "traits.build")
    expect_s3_class(x$sources, "traits_build_sources")
    expect_gt(length(x$sources), 0)
    k <- names(x$sources)[1]
    expect_identical(eval(call("$", x$sources, k)), x$sources[[k]])
  }
  expect_identical(
    extract_dataset(database, "Falster_2003")$sources$Falster_2003,
    sources[[key]]
  )
})

test_that("classify_sources() leaves sources that are not a BibEntry alone", {
  expect_identical(classify_sources(list(sources = NULL)), list(sources = NULL))
  expect_identical(classify_sources(list(traits = 1)), list(traits = 1))
  expect_identical(
    classify_sources(list(sources = list())),
    list(sources = list())
  )
})
