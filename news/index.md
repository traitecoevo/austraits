# Changelog

## austraits 4.0.1

- [`load_austraits()`](https://traitecoevo.github.io/austraits/reference/load_austraits.md)
  loads AusTraits 8.0.0. With traits.use 1.0.0 it stopped with “More
  than one .rds file found”, because 8.0.0 publishes its observed means
  as `.rds` beside the database; traits.use 1.0.1, now required, never
  takes that file for the database.
- `load_austraits(means = TRUE)` reads the observed means from that
  `.rds` (exactly as computed, and a smaller download) instead of the
  `.csv.gz`, through traits.use 1.0.1.

## austraits 4.0.0

- [`library(austraits)`](https://traitecoevo.github.io/austraits/) no
  longer prints a message asking to cite the package;
  `citation("austraits")` still gives the citation.
- Needs `traits.use` 1.0.0 or later, which reads AusTraits 8.0.0 and has
  [`list_versions()`](https://traitecoevo.github.io/traits.use/reference/list_versions.html)
  and the observed means functions.
- `austraits` is now the AusTraits-specific layer over
  [`traits.use`](https://github.com/traitecoevo/traits.use), which holds
  the generic code for any traits.build database (austraits#188). The
  code and its history moved there; `austraits` keeps
  [`load_austraits()`](https://traitecoevo.github.io/austraits/reference/load_austraits.md),
  [`get_versions()`](https://traitecoevo.github.io/austraits/reference/get_versions.md),
  [`get_version_latest()`](https://traitecoevo.github.io/austraits/reference/get_version_latest.md),
  the Getting Started vignette and the map of Australia used by
  [`plot_locations()`](https://traitecoevo.github.io/austraits/reference/plot_locations.md).
- Every function exported by `traits.use` is re-exported, so
  [`austraits::extract_trait()`](https://traitecoevo.github.io/traits.use/reference/extract_trait.html)
  and the other functions keep working. This includes the new
  [`load_database()`](https://traitecoevo.github.io/traits.use/reference/load_database.html),
  [`join_sources()`](https://traitecoevo.github.io/traits.use/reference/join_sources.html)
  and
  [`get_references()`](https://traitecoevo.github.io/traits.use/reference/get_references.html)
  (the references the data cite, as text or BibTeX); see the traits.use
  NEWS for the changes since 3.1.1 (e.g. `sources` can be looked up by
  key with `$`, and support for databases built with traits.build 2.3).
- `load_austraits(means = TRUE)` loads a release’s observed means
  instead of the database: one row per taxon and trait, computed from
  the release’s records with
  [`summarise_trait_means()`](https://traitecoevo.github.io/traits.use/reference/summarise_trait_means.html)
  and published with releases after 7.0.0 (austraits#184).
  [`summarise_trait_means()`](https://traitecoevo.github.io/traits.use/reference/summarise_trait_means.html)
  and
  [`trait_means_dictionary()`](https://traitecoevo.github.io/traits.use/reference/trait_means_dictionary.html)
  are re-exported from `traits.use`, so the same means can be computed
  for any subset of AusTraits.
- [`get_versions()`](https://traitecoevo.github.io/austraits/reference/get_versions.md)
  and
  [`get_version_latest()`](https://traitecoevo.github.io/austraits/reference/get_version_latest.md)
  list the releases with
  [`traits.use::list_versions()`](https://traitecoevo.github.io/traits.use/reference/list_versions.html),
  which lists the releases of any traits.build database from its doi and
  is re-exported here (traits.use#52). They return the same table as
  before, and save it in `path` (`austraits_versions.json`), so
  `update = FALSE` works offline. Before, they read only the first page
  of Zenodo’s listing, so would have left out releases once AusTraits
  had more than fit on it.
- [`load_austraits()`](https://traitecoevo.github.io/austraits/reference/load_austraits.md)
  finds the release with
  [`get_versions()`](https://traitecoevo.github.io/austraits/reference/get_versions.md)
  and loads it with
  [`traits.use::load_database()`](https://traitecoevo.github.io/traits.use/reference/load_database.html),
  so `update = FALSE` loads a cached release offline, including a cache
  made by austraits 3.x (from the file list in its `austraits.json`): a
  cached file is reused only when it matches the release’s md5 checksum,
  a failed download no longer leaves a partial file, and `sources` get
  the `traits_build_sources` class.
- New from `traits.use`:
  [`plot_trait_distribution_jitter()`](https://traitecoevo.github.io/traits.use/reference/plot_trait_distribution_jitter.html),
  [`plot_categorical_trait_distribution()`](https://traitecoevo.github.io/traits.use/reference/plot_categorical_trait_distribution.html)
  and
  [`plot_trait_distribution()`](https://traitecoevo.github.io/traits.use/reference/plot_trait_distribution.html),
  faster plots from austraits.portal.
  `plot_trait_distribution_beeswarm()` is removed: use
  [`plot_trait_distribution_jitter()`](https://traitecoevo.github.io/traits.use/reference/plot_trait_distribution_jitter.html)
  or
  [`plot_trait_distribution()`](https://traitecoevo.github.io/traits.use/reference/plot_trait_distribution.html).
- [`plot_locations()`](https://traitecoevo.github.io/austraits/reference/plot_locations.md)
  calls
  [`traits.use::plot_locations()`](https://traitecoevo.github.io/traits.use/reference/plot_locations.html)
  with the map of Australia by default; `basemap` also takes `NULL` (no
  map) or another map as a data frame of cells (`x`, `y`).
- Requires R 4.1.0 or later. `lubridate` is no longer needed.
- Fix bug in `plot_locations` that silently left out locations east of
  153°E (Brisbane, the Gold Coast, Byron Bay) and north of 9.5°S (Torres
  Strait), about 2% of AusTraits 5.0.0 records with coordinates.
  Locations beyond the map of Australia are now reported in a message.
  The new argument `basemap = NULL` draws every location without the
  map, for databases from outside Australia. `...` is now passed on to
  `geom_pointdensity()`, and `feature = NA` works.

## austraits 3.1.1

- Fix bug in function `load_austraits` that was preventing it from
  loading austraits.build v.7.0.0, as both relational and flattened
  database versions are now on Zenodo. For now, ignoring flattened data
  table.

## austraits 3.1.0

- Change functions to work with traits.build databases that include an
  identifiers table (or ones that don’t)

## austraits 3.0.2

- Updated dependencies, placing graphics related packages in Suggests
- Added internal function to check the compatibility of databases
- Added internal function notify users that some database versions will
  not be supported
- The following functions will no longer support AusTraits version \<
  5.0.0:
  - `extract_*`
  - `trait_pivot_wider`
  - `join_*`
  - `plot_site/locations`
  - `plot_trait_beeswarm`
  - `as_wide_table`
- `trait_pivot_longer` is deprecated
- `summarise_trait_means` will trigger warning due to uninformed
  calculations
- Added new function `extract_data`
- Added new function `bind_databases`
- Added new function `flatten_databases`
- Updated print function `print.traits.build` - thanks
  [@Rekyt](https://github.com/Rekyt)!
