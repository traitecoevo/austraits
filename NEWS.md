# austraits 4.0.0
- `austraits` is now the AusTraits-specific layer over [`traits.use`](https://github.com/traitecoevo/traits.use), which holds the generic code for any traits.build database (austraits#188). The code and its history moved there; `austraits` keeps `load_austraits()`, `get_versions()`, `get_version_latest()`, the Getting Started vignette and the map of Australia used by `plot_locations()`.
- Every function exported by `traits.use` is re-exported, so `austraits::extract_trait()` and the other functions keep working. This includes the new `load_database()` and `join_sources()`; see the traits.use NEWS for the changes since 3.1.1 (e.g. `sources` can be looked up by key with `$`, and support for databases built with traits.build 2.3).
- `load_austraits()` finds the release and loads it with `traits.use::load_database()`: a cached file is reused only when it matches the release's md5 checksum, a failed download no longer leaves a partial file, and `sources` get the `traits_build_sources` class.
- `plot_locations()` calls `traits.use::plot_locations()` with the map of Australia by default; `basemap` also takes `NULL` (no map) or another map as a data frame of cells (`x`, `y`).
- Requires R 4.1.0 or later. `lubridate` is no longer needed.

# austraits (development version, before 4.0.0)
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