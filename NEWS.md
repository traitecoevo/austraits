# austraits 4.1.0

- `load_austraits()`, `get_versions()` and `get_version_latest()` always keep AusTraits in the folder that traits.use shares between all your projects, `database_cache_dir()`, as `traits.use::load_database()` does from traits.use 1.1.0, instead of in `data/austraits` in the working directory. Each release is downloaded once per computer, and `list_cached_databases()` and `clear_database_cache()` see it. `path` is now `NULL` by default; give `path = "data/austraits"` to also keep a copy there (the release, its record and the list of releases, so the folder loads offline on its own). A release already in `path` is copied into the cache rather than downloaded again, once it matches Zenodo's checksum; offline, the folder is used on its own. If a `data/austraits` folder exists and no `path` is given, they say so once a session.
- `load_austraits()` no longer fetches the list of releases from Zenodo on every call: the saved list is used until it is more than 60 days old (`options(traits.use.update_after_days = )`), or until you ask for a release it does not have. `update = NULL` is its new default; `update = TRUE` fetches the list now, and `update = FALSE` works offline, as before. A release downloaded before still loads offline however old the saved list is, with a message. `get_versions()` and `get_version_latest()` ask Zenodo each time by default, as before, so what they list is current; they take `update = NULL` too.
- `load_austraits()` says, once a session, when the release it loads is not the latest.
- `load_austraits(doi = )` takes a doi given as a url (`https://doi.org/...`), as `traits.use::load_database()` does.
- Needs traits.use 1.1.0. The cheat sheet is updated.

# austraits 4.0.1

- `load_austraits()` loads AusTraits 8.0.0. With traits.use 1.0.0 it stopped with "More than one .rds file found", because 8.0.0 publishes its observed means as `.rds` beside the database; traits.use 1.0.1, now required, never takes that file for the database.
- `load_austraits(means = TRUE)` reads the observed means from that `.rds` (exactly as computed, and a smaller download) instead of the `.csv.gz`, through traits.use 1.0.1.

# austraits 4.0.0
- `library(austraits)` no longer prints a message asking to cite the package; `citation("austraits")` still gives the citation.
- Needs `traits.use` 1.0.0 or later, which reads AusTraits 8.0.0 and has `list_versions()` and the observed means functions.
- `austraits` is now the AusTraits-specific layer over [`traits.use`](https://github.com/traitecoevo/traits.use), which holds the generic code for any traits.build database (austraits#188). The code and its history moved there; `austraits` keeps `load_austraits()`, `get_versions()`, `get_version_latest()`, the Getting Started vignette and the map of Australia used by `plot_locations()`.
- Every function exported by `traits.use` is re-exported, so `austraits::extract_trait()` and the other functions keep working. This includes the new `load_database()`, `join_sources()` and `get_references()` (the references the data cite, as text or BibTeX); see the traits.use NEWS for the changes since 3.1.1 (e.g. `sources` can be looked up by key with `$`, and support for databases built with traits.build 2.3).
- `load_austraits(means = TRUE)` loads a release's observed means instead of the database: one row per taxon and trait, computed from the release's records with `summarise_trait_means()` and published with releases after 7.0.0 (austraits#184). `summarise_trait_means()` and `trait_means_dictionary()` are re-exported from `traits.use`, so the same means can be computed for any subset of AusTraits.
- `get_versions()` and `get_version_latest()` list the releases with `traits.use::list_versions()`, which lists the releases of any traits.build database from its doi and is re-exported here (traits.use#52). They return the same table as before, and save it in `path` (`austraits_versions.json`), so `update = FALSE` works offline. Before, they read only the first page of Zenodo's listing, so would have left out releases once AusTraits had more than fit on it.
- `load_austraits()` finds the release with `get_versions()` and loads it with `traits.use::load_database()`, so `update = FALSE` loads a cached release offline, including a cache made by austraits 3.x (from the file list in its `austraits.json`): a cached file is reused only when it matches the release's md5 checksum, a failed download no longer leaves a partial file, and `sources` get the `traits_build_sources` class.
- New from `traits.use`: `plot_trait_distribution_jitter()`, `plot_categorical_trait_distribution()` and `plot_trait_distribution()`, faster plots from austraits.portal. `plot_trait_distribution_beeswarm()` is removed: use `plot_trait_distribution_jitter()` or `plot_trait_distribution()`.
- `plot_locations()` calls `traits.use::plot_locations()` with the map of Australia by default; `basemap` also takes `NULL` (no map) or another map as a data frame of cells (`x`, `y`).
- Requires R 4.1.0 or later. `lubridate` is no longer needed.
- Fix bug in `plot_locations` that silently left out locations east of 153°E (Brisbane, the Gold Coast, Byron Bay) and north of 9.5°S (Torres Strait), about 2% of AusTraits 5.0.0 records with coordinates. Locations beyond the map of Australia are now reported in a message. The new argument `basemap = NULL` draws every location without the map, for databases from outside Australia. `...` is now passed on to `geom_pointdensity()`, and `feature = NA` works.

# austraits 3.1.1
- Fix bug in function `load_austraits` that was preventing it from loading austraits.build v.7.0.0, as both relational and flattened database versions are now on Zenodo. For now, ignoring flattened data table.

# austraits 3.1.0
- Change functions to work with traits.build databases that include an identifiers table (or ones that don't)

# austraits 3.0.2
- Updated dependencies, placing graphics related packages in Suggests
- Added internal function to check the compatibility of databases
- Added internal function notify users that some database versions will not be supported
- The following functions will no longer support AusTraits version < 5.0.0:
    - `extract_*` 
    - `trait_pivot_wider`
    - `join_*`
    - `plot_site/locations`
    - `plot_trait_beeswarm`
    - `as_wide_table`
- `trait_pivot_longer` is deprecated
- `summarise_trait_means` will trigger warning due to uninformed calculations
- Added new function `extract_data`
- Added new function `bind_databases`
- Added new function `flatten_databases`
- Updated print function `print.traits.build` - thanks @Rekyt!